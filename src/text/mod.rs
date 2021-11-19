use arrayvec::ArrayVec;
use itertools::Itertools;
use std::collections::VecDeque;
use std::convert::{TryFrom, TryInto};
use std::iter;
use std::ops::Deref;
use take_until::TakeUntilExt;

use crate::util;
use crate::Dictionary;

pub mod parse;

pub struct TextEngine<'a> {
    alph_table: AlphTable<'a>,
    abbr_table: AbbrTable<'a>,
    utt: UnicodeTransTable<'a>,
}

impl<'a> TextEngine<'a> {
    pub fn new(
        alph_table: AlphTable<'a>,
        abbr_table: AbbrTable<'a>,
        utt: UnicodeTransTable<'a>,
    ) -> Self {
        Self {
            alph_table,
            abbr_table,
            utt,
        }
    }
    pub fn zstr_to_zscii(&'a self, zstr: ZStr<'a>) -> impl Iterator<Item = Zscii> + 'a {
        let abbr_expander = AbbrExpander::new(zstr.zchars(), &self.abbr_table);
        abbr_expander.zscii(self.alph_table)
    }
    pub fn zscii_to_unicode<I: Iterator<Item = Zscii> + 'a>(
        &self,
        zscii: I,
    ) -> impl Iterator<Item = char> + 'a {
        zscii.to_unicode(self.utt)
    }
    pub fn zstr_to_unicode(&'a self, zstr: ZStr<'a>) -> impl Iterator<Item = char> + 'a {
        self.zscii_to_unicode(self.zstr_to_zscii(zstr))
    }
    pub fn unicode_to_zscii<I: Iterator<Item = char> + 'a>(
        &'a self,
        unicode: I,
    ) -> impl Iterator<Item = Zscii> + 'a {
        unicode.filter_map(move |c| Zscii::from_unicode(c, self.utt))
    }
}

// a ZChar is a 5-bit value
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ZChar(pub u8);

impl ZChar {
    pub const SPACE: Self = ZChar(0);
    pub const SHIFT_A1: Self = ZChar(4);
    pub const SHIFT_A2: Self = ZChar(5);
    pub const A2_COMPOSITE_START: Self = ZChar(6);
    pub const A2_NEWLINE: Self = ZChar(7);
}

#[derive(Debug, Clone)]
pub struct TextWord {
    pub zchars: ArrayVec<[ZChar; 3]>,
    pub is_end: bool,
}

impl From<u16> for TextWord {
    fn from(word: u16) -> TextWord {
        let bitmask: u16 = 0x1f;
        TextWord {
            zchars: ArrayVec::from([
                ZChar(((word >> 10) & bitmask) as u8),
                ZChar(((word >> 5) & bitmask) as u8),
                ZChar((word & bitmask) as u8),
            ]),
            is_end: (word >> 15) != 0,
        }
    }
}

impl From<TextWord> for u16 {
    fn from(word: TextWord) -> u16 {
        let text: u16 = word
            .zchars
            .iter()
            .fold(0, |acc, zc| (acc << 5) | (zc.0 as u16));
        if word.is_end {
            text | (1 << 15)
        } else {
            text
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct ZStr<'a> {
    contents: &'a [u8],
}

impl<'a> From<&'a [u8]> for ZStr<'a> {
    fn from(contents: &'a [u8]) -> Self {
        ZStr { contents }
    }
}

impl Deref for ZStr<'_> {
    type Target = [u8];

    fn deref(&self) -> &Self::Target {
        self.contents
    }
}

impl<'a> ZStr<'a> {
    fn text_words(&self) -> impl Iterator<Item = TextWord> + 'a {
        self.contents
            .chunks_exact(2)
            .map(|pair| u16::from_be_bytes([pair[0], pair[1]]))
            .map_into::<TextWord>()
            .take_until(|tw| tw.is_end)
    }
    #[must_use = "lazy iterator"]
    pub fn zchars(&self) -> impl Iterator<Item = ZChar> + 'a {
        self.text_words().flat_map(|tw| tw.zchars.into_iter())
    }
    pub fn len_bytes(&self) -> usize {
        2 * self.text_words().count()
    }
}

// Zscii chars are nominally 10bit but only the bottom 8 are used
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Zscii(pub u8);

impl Zscii {
    pub const NEWLINE: Self = Zscii(13);
    pub const SPACE: Self = Zscii(b' ');

    fn decompose(self) -> (ZChar, ZChar) {
        (ZChar(self.0 >> 5), ZChar(self.0 & 0x1f))
    }

    fn compose(zc1: ZChar, zc2: ZChar) -> Option<Self> {
        let high = zc1.0 as u16;
        let low = zc2.0 as u16;
        let byte = u8::try_from((high << 5) | low).ok()?;
        Some(Zscii(byte))
    }

    fn to_unicode(self, utt: UnicodeTransTable) -> ZsciiResult<char> {
        let zb = self.0;
        match zb {
            0 => Ok('\0'),
            8 => Err(ZsciiError::InputOnly(zb)), //delete
            13 => Ok('\n'),
            27 => Err(ZsciiError::InputOnly(zb)),        // esc
            32..=126 => Ok(zb as char),                  // standard ascii
            129..=154 => Err(ZsciiError::InputOnly(zb)), // arrow keys, function keys, keypad
            155..=251 => utt
                .zscii_to_unicode(self)
                .ok_or(ZsciiError::UnicodeTransMissing(zb)),
            253..=254 => Err(ZsciiError::InputOnly(zb)), // mouse clicks
            _ => Err(ZsciiError::Undefined(zb)),
        }
    }

    fn from_unicode(uni: char, utt: UnicodeTransTable) -> Option<Self> {
        match uni {
            // "interpreting $60 as a grave accent is to be avoided."
            // not sure how to implement this directive, leaving this commented
            // '`' => Some(Zscii(b'\'')),
            '\n' => Some(Self::NEWLINE),
            ' '..='~' => Some(Zscii(u8::try_from(uni as u32).unwrap())),
            _ => utt.unicode_to_zscii(uni),
        }
    }
}

#[derive(Debug)]
pub struct AbbrTable<'a> {
    // three rows, each contains 32 addresses, each composed of 2 bytes
    // looks complicated but makes indexing into the table really easy
    table: &'a [[[u8; 2]; 32]; 3],
    // abbr strings can be stored anywhere so we need to hold a ref to all of memory
    memory: &'a [u8],
}

impl<'a> AbbrTable<'a> {
    pub fn from_memory(memory: &'a [u8], addr: usize) -> Option<Self> {
        let table_in_mem = memory.get(addr..)?;
        let (_, table) = parse::abbr_table(table_in_mem).ok()?;
        Some(AbbrTable { table, memory })
    }

    pub fn lookup_expansion(&self, zc1: ZChar, zc2: ZChar) -> Option<ZStr<'a>> {
        let row_idx = (zc1.0 as usize) - 1;
        assert!(row_idx < 3);
        let col_idx = zc2.0 as usize;
        let str_addr = u16::from_be_bytes(self.table[row_idx][col_idx]) as usize;
        // TODO: the table contains word addrs rather than byte addrs??
        Some(ZStr::from(self.memory.get(str_addr..)?))
    }
}

struct AbbrExpander<'a, I> {
    zchar_iter: I,
    abbr_table: &'a AbbrTable<'a>,
    abbr_buf: VecDeque<ZChar>,
}

impl<'a, I> AbbrExpander<'a, I> {
    fn new(zchar_iter: I, abbr_table: &'a AbbrTable<'a>) -> Self {
        AbbrExpander {
            zchar_iter,
            abbr_table,
            abbr_buf: VecDeque::new(),
        }
    }
}

// current implementation eagerly buffers the string that the abbr expands into
// TODO: do this lazily if it can be done neatly
impl<'a, I> Iterator for AbbrExpander<'a, I>
where
    I: Iterator<Item = ZChar>,
{
    type Item = ZChar;

    fn next(&mut self) -> Option<Self::Item> {
        if !self.abbr_buf.is_empty() {
            return self.abbr_buf.pop_front();
        }

        let zc = self.zchar_iter.next()?;
        match zc {
            ZChar(1) | ZChar(2) | ZChar(3) => {
                let abbr_spec = self.zchar_iter.next()?;
                if let Some(zstr) = self.abbr_table.lookup_expansion(zc, abbr_spec) {
                    // TODO: log a warning/error if lookup_expansion() returns None
                    // TODO: some validity checking on the result
                    // ie, doesn't contain abbrs, doesn't end on incomplete multi-part char
                    self.abbr_buf = zstr.zchars().collect();
                }
                self.next()
            }
            _ => Some(zc),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Alph {
    A0,
    A1,
    A2,
}

#[derive(Debug, Clone, Copy)]
pub enum AlphTable<'a> {
    Default,
    Custom(CustomAlphTable<'a>),
}

#[derive(Debug, Clone, Copy)]
pub struct CustomAlphTable<'a> {
    table: [&'a [u8; 26]; 3],
}

static DEFAULT_ALPH_TABLE: [&[u8; 26]; 3] = [
    br#"abcdefghijklmnopqrstuvwxyz"#,
    br#"ABCDEFGHIJKLMNOPQRSTUVWXYZ"#,
    br#"XX0123456789.,!?_#'"/\-:()"#,
];

impl<'a> AlphTable<'a> {
    const ZCHAR_START: u8 = 6;

    pub fn from_memory(bytes: &'a [u8]) -> Option<Self> {
        let (_, custom_alph_table) = parse::custom_alph_table(bytes).ok()?;
        Some(AlphTable::Custom(custom_alph_table))
    }

    fn zchar_to_zscii(&self, zc: ZChar, alph: Alph) -> Option<Zscii> {
        if zc.0 < Self::ZCHAR_START {
            return None;
        }
        let index = (zc.0 - Self::ZCHAR_START) as usize;
        let table = match self {
            AlphTable::Default => DEFAULT_ALPH_TABLE,
            AlphTable::Custom(CustomAlphTable { table }) => *table,
        };
        let zscii_byte = match (alph, index) {
            (Alph::A0, _) => table[0][index],
            (Alph::A1, _) => table[1][index],
            (Alph::A2, 0) => return None, // compound char, can't handle here
            (Alph::A2, 1) => Zscii::NEWLINE.0, // permanently mapped to newline
            (Alph::A2, _) => table[2][index],
        };
        Some(Zscii(zscii_byte))
    }

    fn zscii_to_zchar(&self, zscii: Zscii) -> Option<(ZChar, Alph)> {
        match self {
            AlphTable::Default => Self::default_zscii_to_zchar(zscii),
            AlphTable::Custom(at) => Self::custom_zscii_to_zchar(*at, zscii),
        }
    }

    fn default_zscii_to_zchar(zscii: Zscii) -> Option<(ZChar, Alph)> {
        let Zscii(zb) = zscii;
        match zb {
            b'a'..=b'z' => Some((ZChar(zb - b'a' + Self::ZCHAR_START), Alph::A0)),
            b'A'..=b'Z' => Some((ZChar(zb - b'A' + Self::ZCHAR_START), Alph::A1)),
            _ => {
                let idx = DEFAULT_ALPH_TABLE[2]
                    .iter()
                    .skip(2)
                    .position(|&z| z == zb)?;
                let zc = ZChar(u8::try_from(idx + 2).unwrap() + Self::ZCHAR_START);
                Some((zc, Alph::A2))
            }
        }
    }

    fn custom_zscii_to_zchar(at: CustomAlphTable, zscii: Zscii) -> Option<(ZChar, Alph)> {
        let Zscii(zb) = zscii;
        let (idx, alph) = if let Some(idx) = at.table[0].iter().position(|&z| z == zb) {
            (idx, Alph::A0)
        } else if let Some(idx) = at.table[1].iter().position(|&z| z == zb) {
            (idx, Alph::A1)
        } else if let Some(idx) = at.table[2].iter().skip(2).position(|&z| z == zb) {
            // index is offset because we ignore the first 2 entries in the A2 row
            (idx + 2, Alph::A2)
        } else {
            return None;
        };
        let zc = ZChar(u8::try_from(idx).unwrap() + Self::ZCHAR_START);
        Some((zc, alph))
    }
}

pub struct ZsciiFromZChars<'a, I: Iterator<Item = ZChar>> {
    zchars: I,
    alph_table: AlphTable<'a>,
}

impl<'a, I: Iterator<Item = ZChar>> ZsciiFromZChars<'a, I> {
    fn new(zchars: I, alph_table: AlphTable<'a>) -> ZsciiFromZChars<I> {
        ZsciiFromZChars { zchars, alph_table }
    }
    fn next_h(&mut self, alph: Alph) -> Option<Zscii> {
        let zc = self.zchars.next()?;
        match (alph, zc.0) {
            (_, 0) => Some(Zscii(b' ')),
            (_, 1..=3) => panic!("Abbreviations not implemented yet"),
            (_, 4) => self.next_h(Alph::A1),
            (_, 5) => self.next_h(Alph::A2),
            (Alph::A2, 6) => {
                let first = self.zchars.next()?;
                let second = self.zchars.next()?;
                let comp = Zscii::compose(first, second);
                match comp {
                    Some(_) => comp,
                    // skip over invalid compound characters, without stopping the stream
                    // TODO: not sure if this is acceptable behavior
                    // if it's not, will have to change Zscii to be backed by u16
                    None => self.next_h(Alph::A0),
                }
            }
            (Alph::A2, 7) => Some(Zscii::NEWLINE),
            // this will always be Some(_) because we've handled all the other cases
            (_, _) => self.alph_table.zchar_to_zscii(zc, alph),
        }
    }
}

impl<'a, I: Iterator<Item = ZChar>> Iterator for ZsciiFromZChars<'a, I> {
    type Item = Zscii;
    fn next(&mut self) -> Option<Self::Item> {
        self.next_h(Alph::A0)
    }
}

pub trait ZsciiDecodable<'a> {
    type ZsciiIter: Iterator<Item = Zscii>;
    fn zscii(self, alph_table: AlphTable<'a>) -> Self::ZsciiIter;
}

impl<'a, I: IntoIterator<Item = ZChar>> ZsciiDecodable<'a> for I {
    type ZsciiIter = ZsciiFromZChars<'a, I::IntoIter>;
    fn zscii(self, alph_table: AlphTable<'a>) -> Self::ZsciiIter {
        ZsciiFromZChars::new(self.into_iter(), alph_table)
    }
}

#[derive(Debug, Clone, Copy)]
pub enum UnicodeTransTable<'a> {
    Default,
    Custom(CustomUtt<'a>),
}

#[derive(Debug, Clone, Copy)]
pub struct CustomUtt<'a> {
    table: &'a [u8],
}

lazy_static! {
    pub static ref DEFAULT_UTT: Vec<char> = {
        let table = "äöüÄÖÜß»«ëïÿËÏáéíóúýÁÉÍÓÚÝàèìòùÀÈÌÒÙ\
                     âêîôûÂÊÎÔÛåÅøØãñõÃÑÕæÆçÇþðÞÐ£œŒ¡¿"
            .chars()
            .collect::<Vec<char>>();
        assert!(table.len() == (223 - 155 + 1));
        table
    };
}

impl<'a> UnicodeTransTable<'a> {
    // range start/end are really zscii values ie u8's
    // but we add and subtract them to/from indexes into the table
    // so they're declared as usize's for convenience
    const ZSCII_RANGE_START: usize = 155;
    const ZSCII_RANGE_END: usize = 251;
    // in units of u16
    const MAX_SIZE: usize = Self::ZSCII_RANGE_END - Self::ZSCII_RANGE_START + 1;

    pub fn from_memory(bytes: &'a [u8]) -> Option<Self> {
        let (_, custom_utt) = parse::custom_utt(bytes).ok()?;
        if custom_utt.table.len() <= 2 * Self::MAX_SIZE {
            Some(UnicodeTransTable::Custom(custom_utt))
        } else {
            None
        }
    }

    pub fn zscii_to_unicode(&self, zscii: Zscii) -> Option<char> {
        let zcodepoint = zscii.0 as usize;
        if !(Self::ZSCII_RANGE_START..=Self::ZSCII_RANGE_END).contains(&zcodepoint) {
            return None;
        }
        match self {
            UnicodeTransTable::Default => {
                let index = zcodepoint - Self::ZSCII_RANGE_START;
                DEFAULT_UTT.get(index).copied()
            }
            UnicodeTransTable::Custom(utt) => {
                let index = 2 * (zcodepoint - Self::ZSCII_RANGE_START);
                let bytes = utt.table.get(index..=index + 1)?;
                let scalar_val = u16::from_be_bytes([bytes[0], bytes[1]]);
                char::try_from(scalar_val as u32).ok()
            }
        }
    }

    pub fn unicode_to_zscii(&self, unicode: char) -> Option<Zscii> {
        // don't know a better way than exhaustively searching...
        // can share the same index for both default and custom in this case
        // because we search the custom table in groups of 2 bytes
        let index: usize;
        match self {
            UnicodeTransTable::Default => {
                index = DEFAULT_UTT.iter().position(|&c| c == unicode)?;
            }
            UnicodeTransTable::Custom(utt) => {
                let target = u16::try_from(unicode as u32).ok()?;
                let mut codepoint_iter = util::bytes_to_words(utt.table.iter().copied());
                index = codepoint_iter.position(|dw| dw == target)?;
            }
        }
        let byte = u8::try_from(index + Self::ZSCII_RANGE_START).unwrap();
        Some(Zscii(byte))
    }
}

#[derive(Debug)]
pub enum ZsciiError {
    InputOnly(u8),
    OutputOnly(u8),
    Undefined(u8),
    UnicodeTransMissing(u8),
    NotImplemented(u8),
}

pub type ZsciiResult<T> = Result<T, ZsciiError>;

pub trait UnicodeFromZscii<'a> {
    type UnicodeIter: Iterator<Item = char>;

    fn to_unicode(self, utt: UnicodeTransTable<'a>) -> Self::UnicodeIter;
}

impl<'a, I: 'a + IntoIterator<Item = Zscii>> UnicodeFromZscii<'a> for I {
    type UnicodeIter = Box<dyn Iterator<Item = char> + 'a>;

    fn to_unicode(self, utt: UnicodeTransTable<'a>) -> Self::UnicodeIter {
        // TODO: log errors that occur during decoding
        Box::new(self.into_iter().filter_map(move |z| z.to_unicode(utt).ok()))
    }
}

struct ZsciiToZChars<'a, I> {
    zscii_chars: I,
    alph_table: AlphTable<'a>,
    buf: Vec<ZChar>,
}

impl<'a, I> Iterator for ZsciiToZChars<'a, I>
where
    I: Iterator<Item = Zscii>,
{
    type Item = ZChar;
    fn next(&mut self) -> Option<Self::Item> {
        if !self.buf.is_empty() {
            return self.buf.pop();
        }
        let zscii = self.zscii_chars.next()?;
        if zscii == Zscii::SPACE {
            Some(ZChar::SPACE)
        } else if zscii == Zscii::NEWLINE {
            self.buf.push(ZChar::A2_NEWLINE);
            Some(ZChar::SHIFT_A2)
        } else if let Some((zchar, alph)) = self.alph_table.zscii_to_zchar(zscii) {
            match alph {
                Alph::A0 => Some(zchar),
                Alph::A1 => {
                    self.buf.push(zchar);
                    Some(ZChar::SHIFT_A1)
                }
                Alph::A2 => {
                    self.buf.push(zchar);
                    Some(ZChar::SHIFT_A2)
                }
            }
        } else {
            // represent as composite character
            let (high, low) = zscii.decompose();
            self.buf.push(low);
            self.buf.push(high);
            self.buf.push(ZChar::A2_COMPOSITE_START);
            Some(ZChar::SHIFT_A2)
        }
    }
}

// defining a trait to make this a nice adapter is too much effort
pub fn zscii_to_zchars<'a>(
    zscii_chars: impl Iterator<Item = Zscii> + 'a,
    alph_table: AlphTable<'a>,
) -> impl Iterator<Item = ZChar> + 'a {
    ZsciiToZChars {
        zscii_chars,
        alph_table,
        buf: Vec::new(),
    }
}

fn zchars_to_textwords(zchars: impl Iterator<Item = ZChar>) -> impl Iterator<Item = TextWord> {
    zchars
        // append a bit of padding; tuples() will drop any extra
        .chain(iter::repeat(ZChar::SHIFT_A2).take(2))
        .tuples()
        .peekable()
        // batching() gives us access to peek() to know which element is the last
        .batching(|iter| {
            let (zc1, zc2, zc3) = iter.next()?;
            let is_end = iter.peek().is_none();
            Some(TextWord {
                zchars: ArrayVec::from([zc1, zc2, zc3]),
                is_end,
            })
        })
}

impl<'a> ZStr<'a> {
    #[must_use = "lazy iterator"]
    pub fn unicode_chars(
        self,
        alph_table: AlphTable<'a>,
        utt: UnicodeTransTable<'a>,
    ) -> impl Iterator<Item = char> + 'a {
        self.zchars()
            .zscii(alph_table)
            // TODO: log invalid characters instead of silently skipping
            .filter_map(move |z| z.to_unicode(utt).ok())
    }
}

pub trait ZTextEncodable<'a> {
    type ByteIter: Iterator<Item = u8>;

    fn encode_ztext(self, alph_table: AlphTable<'a>, utt: UnicodeTransTable<'a>) -> Self::ByteIter;
}

impl<'a, I> ZTextEncodable<'a> for I
where
    I: 'a + IntoIterator<Item = char>,
{
    type ByteIter = Box<dyn Iterator<Item = u8> + 'a>;

    fn encode_ztext(self, alph_table: AlphTable<'a>, utt: UnicodeTransTable<'a>) -> Self::ByteIter {
        // unicode -> Zscii
        let iter = self
            .into_iter()
            .filter_map(move |c| Zscii::from_unicode(c, utt));
        // Zscii -> ZChars
        let iter = zscii_to_zchars(iter, alph_table);
        // ZChars -> TextWords -> u16
        let iter = zchars_to_textwords(iter).map_into::<u16>();
        // u16 -> u8
        let iter = util::words_to_bytes(iter);

        Box::new(iter)
    }
}

pub fn zchars_to_dict_key(zchars: impl Iterator<Item = ZChar>) -> Vec<u8> {
    let padding = iter::once(ZChar::SHIFT_A2).cycle();
    let iter = zchars.chain(padding).take(3 * Dictionary::KEY_SIZE / 2);
    let iter = zchars_to_textwords(iter).map_into::<u16>();
    let iter = util::words_to_bytes(iter);
    iter.collect()
}

pub fn test() {
    let text = "«dolor»";
    let alph_table = AlphTable::Default;
    let utt = UnicodeTransTable::Default;
    let iter = text
        .chars()
        .filter_map(move |c| Zscii::from_unicode(c, utt));
    let iter = zscii_to_zchars(iter, alph_table);
    let iter = iter.zscii(alph_table);
    // let iter = zchars_to_textwords(iter)
    //     .map_into::<u16>();
    // let iter = util::words_to_bytes(iter);

    let result = iter.collect::<Vec<Zscii>>();
    println!("{:?}", result);
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::iter;

    #[test]
    fn test_zchar_decode() {
        let text: Vec<u8> = vec![0x35, 0x51, 0xc6, 0x85, 0x77, 0xdf];
        let mut zchar_iter = ZStr::from(&text[..]).zchars();
        assert_eq!(zchar_iter.next().unwrap(), ZChar(0x0d));
        assert_eq!(zchar_iter.next().unwrap(), ZChar(0x0a));
        assert_eq!(zchar_iter.next().unwrap(), ZChar(0x11));
        assert_eq!(zchar_iter.next().unwrap(), ZChar(0x11));
        assert_eq!(zchar_iter.next().unwrap(), ZChar(0x14));
        assert_eq!(zchar_iter.next().unwrap(), ZChar(0x05));
        assert!(zchar_iter.next().is_none());
    }

    #[test]
    fn test_decode() {
        let text: Vec<u8> = vec![0x35, 0x51, 0xc6, 0x85, 0x77, 0xdf];
        let decoded: String = ZStr::from(&text[..])
            .unicode_chars(AlphTable::Default, UnicodeTransTable::Default)
            .collect();
        assert_eq!(decoded, "hello");
    }

    #[test]
    fn test_round_trip() {
        let text = "lorem Ipsum,\n«DOLOR» amet?";
        let encoded: Vec<u8> = text
            .chars()
            .encode_ztext(AlphTable::Default, UnicodeTransTable::Default)
            .collect();
        let decoded: String = ZStr::from(&encoded[..])
            .unicode_chars(AlphTable::Default, UnicodeTransTable::Default)
            .collect();
        assert_eq!(text, decoded);
    }

    #[test]
    fn test_round_trip_custom_tables() {
        let _utt = UnicodeTransTable::from_memory(&utt_vec_from_str("åëø"));
        // TODO
    }

    #[test]
    fn test_default_alph_table() {
        let alph_table = AlphTable::Default;

        assert_eq!(
            alph_table.zchar_to_zscii(ZChar(22), Alph::A0).unwrap(),
            Zscii(b'q')
        );
        assert_eq!(
            alph_table.zscii_to_zchar(Zscii(b'q')).unwrap(),
            (ZChar(22), Alph::A0)
        );

        assert_eq!(
            alph_table.zchar_to_zscii(ZChar(22), Alph::A1).unwrap(),
            Zscii(b'Q')
        );
        assert_eq!(
            alph_table.zscii_to_zchar(Zscii(b'Q')).unwrap(),
            (ZChar(22), Alph::A1)
        );

        assert_eq!(
            alph_table.zchar_to_zscii(ZChar(22), Alph::A2).unwrap(),
            Zscii(b'_')
        );
        assert_eq!(
            alph_table.zscii_to_zchar(Zscii(b'_')).unwrap(),
            (ZChar(22), Alph::A2)
        );
    }

    #[test]
    fn test_custom_alph_table() {
        let mut alph_table = vec![0u8; 26 * 3];
        alph_table[0..26].copy_from_slice(DEFAULT_ALPH_TABLE[0]);
        alph_table[26..52].copy_from_slice(DEFAULT_ALPH_TABLE[1]);
        alph_table[52..78].copy_from_slice(DEFAULT_ALPH_TABLE[2]);
        alph_table[4] = 170; // e -> é (using default UTT)
        alph_table[26] = 202; // A -> Å
        alph_table[77] = 162; // ) -> »
        let alph_table = AlphTable::from_memory(&alph_table).unwrap();

        assert_eq!(
            alph_table.zchar_to_zscii(ZChar(10), Alph::A0).unwrap(),
            Zscii(170)
        );
        assert_eq!(
            alph_table.zscii_to_zchar(Zscii(170)).unwrap(),
            (ZChar(10), Alph::A0)
        );

        assert_eq!(
            alph_table.zchar_to_zscii(ZChar(6), Alph::A1).unwrap(),
            Zscii(202)
        );
        assert_eq!(
            alph_table.zscii_to_zchar(Zscii(202)).unwrap(),
            (ZChar(6), Alph::A1)
        );

        assert_eq!(
            alph_table.zchar_to_zscii(ZChar(31), Alph::A2).unwrap(),
            Zscii(162)
        );
        assert_eq!(
            alph_table.zscii_to_zchar(Zscii(162)).unwrap(),
            (ZChar(31), Alph::A2)
        );
    }

    // helper to construct custom UTT's
    fn utt_vec_from_str(utt_str: &str) -> Vec<u8> {
        let byte_iter = util::words_to_bytes(utt_str.encode_utf16());
        let mut utt_vec = iter::once(0).chain(byte_iter).collect::<Vec<u8>>();
        let len_words = (utt_vec.len() - 1) / 2;
        utt_vec[0] = u8::try_from(len_words).unwrap();
        utt_vec
    }

    #[test]
    fn test_default_utt() {
        let utt = UnicodeTransTable::Default;

        assert_eq!(utt.zscii_to_unicode(Zscii(155)).unwrap(), 'ä');
        assert_eq!(utt.unicode_to_zscii('ä').unwrap(), Zscii(155));

        assert_eq!(utt.zscii_to_unicode(Zscii(191)).unwrap(), 'â');
        assert_eq!(utt.unicode_to_zscii('â').unwrap(), Zscii(191));

        assert_eq!(utt.zscii_to_unicode(Zscii(223)).unwrap(), '¿');
        assert_eq!(utt.unicode_to_zscii('¿').unwrap(), Zscii(223));

        assert!(utt.zscii_to_unicode(Zscii(154)).is_none());
        assert!(utt.zscii_to_unicode(Zscii(224)).is_none());
    }

    #[test]
    fn test_custom_utt() {
        let utt_vec = utt_vec_from_str("äëïöü");
        let utt = UnicodeTransTable::from_memory(&utt_vec).unwrap();

        assert_eq!(utt.zscii_to_unicode(Zscii(155)).unwrap(), 'ä');
        assert_eq!(utt.unicode_to_zscii('ä').unwrap(), Zscii(155));

        assert_eq!(utt.zscii_to_unicode(Zscii(156)).unwrap(), 'ë');
        assert_eq!(utt.unicode_to_zscii('ë').unwrap(), Zscii(156));

        assert_eq!(utt.zscii_to_unicode(Zscii(159)).unwrap(), 'ü');
        assert_eq!(utt.unicode_to_zscii('ü').unwrap(), Zscii(159));

        assert!(utt.zscii_to_unicode(Zscii(154)).is_none());
        assert!(utt.zscii_to_unicode(Zscii(160)).is_none());

        assert!(utt.unicode_to_zscii('â').is_none());
    }

    #[test]
    fn test_utt_bounds() {
        // test for off-by-one errors in the custom UTT code
        // construct a big UTT
        // starting point picked so that Zscii(251) translates to '~'
        let chars = 30u8..;
        let zeroes = iter::once(0u8).cycle();

        // start by making it just a little too big
        let excessive_len = 1 + u8::try_from(UnicodeTransTable::MAX_SIZE).unwrap();
        let mut utt_vec = iter::once(excessive_len)
            .chain(zeroes.interleave(chars))
            .take(1 + (2 * (excessive_len as usize)))
            .collect::<Vec<u8>>();
        assert!(UnicodeTransTable::from_memory(&utt_vec).is_none());

        // now try just the right size
        utt_vec.pop();
        utt_vec.pop();
        utt_vec[0] = excessive_len - 1;
        let utt = UnicodeTransTable::from_memory(&utt_vec).unwrap();

        assert_eq!(utt.zscii_to_unicode(Zscii(251)).unwrap(), '~');
        assert_eq!(utt.unicode_to_zscii('~').unwrap(), Zscii(251));

        assert!(utt.zscii_to_unicode(Zscii(252)).is_none());
    }
}
