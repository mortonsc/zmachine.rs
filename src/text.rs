use itertools::Itertools;
use std::convert::{TryFrom, TryInto};

// a ZChar is a 5-bit value
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ZChar(u8);

impl ZChar {
    const SPACE: Self = ZChar(0);
    const SHIFT_A1: Self = ZChar(4);
    const SHIFT_A2: Self = ZChar(5);
    const A2_COMPOSITE_START: Self = ZChar(6);
    const A2_NEWLINE: Self = ZChar(7);
}

// Zscii chars are nominally 10bit but only the bottom 8 are used
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Zscii(u8);

impl Zscii {
    const NEWLINE: Self = Zscii(13);
    const SPACE: Self = Zscii(b' ');
}

impl Zscii {
    fn decompose(self) -> (ZChar, ZChar) {
        (ZChar(self.0 >> 5), ZChar(self.0 & 0x1f))
    }
}

#[derive(Debug, Clone, Copy)]
struct TextWord {
    zchars: [ZChar; 3],
    is_end: bool,
}

impl From<u16> for TextWord {
    fn from(word: u16) -> TextWord {
        let bitmask: u16 = 0x1f;
        TextWord {
            zchars: [
                ZChar(((word >> 10) & bitmask) as u8),
                ZChar(((word >> 5) & bitmask) as u8),
                ZChar((word & bitmask) as u8),
            ],
            is_end: (word >> 15) != 0,
        }
    }
}

impl From<TextWord> for u16 {
    fn from(word: TextWord) -> u16 {
        let text: u16 = word
            .zchars
            .iter()
            .fold(0, |acc, zc| (acc << 5) & (zc.0 as u16));
        if word.is_end {
            text & 0x80
        } else {
            text
        }
    }
}

pub struct ZCharsFromBytes<I> {
    text: I,
    current_word: Option<TextWord>,
    index: usize,
}

impl<'a, I: Iterator<Item = &'a u8>> ZCharsFromBytes<I> {
    fn new(iter: I) -> Self {
        ZCharsFromBytes {
            text: iter,
            current_word: None,
            index: 0,
        }
    }

    fn next_word(&mut self) -> Option<TextWord> {
        Some(TextWord::from(u16::from_be_bytes([
            *self.text.next()?,
            *self.text.next()?,
        ])))
    }
}

impl<'a, I: Iterator<Item = &'a u8>> Iterator for ZCharsFromBytes<I> {
    type Item = ZChar;
    fn next(&mut self) -> Option<Self::Item> {
        if self.current_word.is_none() {
            self.current_word = self.next_word();
        }
        let word = self.current_word?;
        match self.index {
            0..=2 => {
                let zc = word.zchars[self.index];
                self.index += 1;
                Some(zc)
            }
            3 => {
                if word.is_end {
                    None
                } else {
                    self.current_word = None;
                    self.index = 0;
                    self.next()
                }
            }
            _ => panic!(),
        }
    }
}

pub trait ZCharDecodable {
    type ZCharIter: Iterator<Item = ZChar>;
    fn zchars(self) -> Self::ZCharIter;
}

impl<'a, I: IntoIterator<Item = &'a u8>> ZCharDecodable for I {
    type ZCharIter = ZCharsFromBytes<I::IntoIter>;

    #[inline]
    fn zchars(self) -> Self::ZCharIter {
        ZCharsFromBytes::new(self.into_iter())
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
        if bytes.len() < (26 * 3) {
            return None;
        }
        let table = [
            // rust requires try_into() but we know it will succeed
            // because we select the right size with constant indices
            bytes[0..26].try_into().unwrap(),
            bytes[26..52].try_into().unwrap(),
            bytes[52..78].try_into().unwrap(),
        ];
        Some(AlphTable::Custom(CustomAlphTable { table }))
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
                let idx = DEFAULT_ALPH_TABLE[2].iter().position(|&z| z == zb)?;
                let zc = ZChar(u8::try_from(idx).unwrap() + Self::ZCHAR_START);
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
        } else if let Some(idx) = at.table[2][2..].iter().position(|&z| z == zb) {
            // index is offset because we ignore the first 2 entries in the A2 row
            (idx + 2, Alph::A2)
        } else {
            return None;
        };
        let zc = ZChar(u8::try_from(idx).unwrap() + Self::ZCHAR_START);
        Some((zc, alph))
    }
}

#[inline]
fn composite_zscii(zc1: ZChar, zc2: ZChar) -> Zscii {
    Zscii((zc1.0 << 5) | zc2.0)
}

pub struct ZsciiFromZChars<'a, I> {
    zchars: I,
    alph: Alph,
    alph_table: AlphTable<'a>,
}

impl<'a, I> ZsciiFromZChars<'a, I> {
    fn new(zchars: I, alph_table: AlphTable<'a>) -> ZsciiFromZChars<I> {
        ZsciiFromZChars {
            zchars,
            alph: Alph::A0,
            alph_table,
        }
    }
}

impl<'a, I: Iterator<Item = ZChar>> Iterator for ZsciiFromZChars<'a, I> {
    type Item = Zscii;
    fn next(&mut self) -> Option<Self::Item> {
        let zc = self.zchars.next()?;
        let zval = zc.0;
        let zscii = match (self.alph, zval) {
            (_, 0) => Zscii(b' '),
            (_, 1..=3) => panic!("Abbreviations not implemented yet"),
            (_, 4) => {
                self.alph = Alph::A1;
                self.next()?
            }
            (_, 5) => {
                self.alph = Alph::A2;
                self.next()?
            }
            (Alph::A2, 6) => composite_zscii(self.zchars.next()?, self.zchars.next()?),
            (Alph::A2, 7) => Zscii::NEWLINE,
            (_, _) => self.alph_table.zchar_to_zscii(zc, self.alph).unwrap(),
        };
        self.alph = Alph::A0;
        Some(zscii)
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
    // so they're defined as usize's for convenience
    const ZSCII_RANGE_START: usize = 155;
    const ZSCII_RANGE_END: usize = 251;
    // in units of u16
    const MAX_SIZE: usize = Self::ZSCII_RANGE_END - Self::ZSCII_RANGE_START + 1;

    pub fn from_memory(bytes: &'a [u8]) -> Option<Self> {
        // first byte contains length (in units of u16)
        let len_w = *bytes.get(0)? as usize;
        if (len_w > 0) && (len_w <= Self::MAX_SIZE) {
            let len_b = 2 * len_w;
            let table = bytes.get(1..=len_b)?;
            Some(UnicodeTransTable::Custom(CustomUtt { table }))
        } else {
            None
        }
    }

    pub fn zscii_to_unicode(&self, zscii: Zscii) -> Option<char> {
        let zcodepoint = zscii.0 as usize;
        if (zcodepoint < Self::ZSCII_RANGE_START) || (zcodepoint > Self::ZSCII_RANGE_END) {
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
                index = utt
                    .table
                    .iter()
                    .tuples()
                    .map(|(&high, &low)| u16::from_be_bytes([high, low]))
                    .position(|dw| dw == target)?;
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

impl Zscii {
    pub fn to_unicode(self, utt: UnicodeTransTable) -> ZsciiResult<char> {
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
            253..=254 => Err(ZsciiError::InputOnly(zb)), // mouse click
            _ => Err(ZsciiError::Undefined(zb)),
        }
    }
}

pub trait UnicodeFromZscii<'a> {
    type UnicodeIter: Iterator<Item = char>;

    fn unicode(self, utt: UnicodeTransTable<'a>) -> Self::UnicodeIter;
}

impl<'a, I: 'a + IntoIterator<Item = Zscii>> UnicodeFromZscii<'a> for I {
    type UnicodeIter = Box<dyn Iterator<Item = char> + 'a>;

    fn unicode(self, utt: UnicodeTransTable<'a>) -> Self::UnicodeIter {
        // TODO: log errors that occur during decoding
        Box::new(self.into_iter().filter_map(move |z| z.to_unicode(utt).ok()))
    }
}

impl Zscii {
    fn from_unicode(uni: char, utt: &UnicodeTransTable) -> Option<Zscii> {
        match uni {
            // "interpreting $60 as a grave accent is to be avoided."
            '`' => Some(Zscii(b'\'')),
            '\n' => Some(Zscii(0x0d)),
            ' '..='~' => Some(Zscii(u8::try_from(uni as u32).unwrap())),
            _ => utt.unicode_to_zscii(uni),
        }
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

#[cfg(test)]
mod tests {
    use super::*;
    use std::iter;

    #[test]
    fn test_zchar_decode() {
        let text: Vec<u8> = vec![0x35, 0x51, 0xc6, 0x85, 0x77, 0xdf];
        let mut zchar_iter = text.iter().zchars();
        assert_eq!(zchar_iter.next().unwrap(), ZChar(0x0d));
        assert_eq!(zchar_iter.next().unwrap(), ZChar(0x0a));
        assert_eq!(zchar_iter.next().unwrap(), ZChar(0x11));
        assert_eq!(zchar_iter.next().unwrap(), ZChar(0x11));
        assert_eq!(zchar_iter.next().unwrap(), ZChar(0x14));
        assert_eq!(zchar_iter.next().unwrap(), ZChar(0x05));
        assert!(zchar_iter.next().is_none());
    }

    #[test]
    fn test_zscii_decode() {
        let text: Vec<u8> = vec![0x35, 0x51, 0xc6, 0x85, 0x77, 0xdf];
        let decoded: String = text
            .zchars()
            .zscii(AlphTable::Default)
            .unicode(UnicodeTransTable::Default)
            .collect();
        assert_eq!(decoded, "hello");
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
        let high_byte_iter = utt_str
            .encode_utf16()
            .map(|w| u8::try_from(w >> 8).unwrap());
        let low_byte_iter = utt_str
            .encode_utf16()
            .map(|w| u8::try_from(w & 0xff).unwrap());
        let mut utt_vec = iter::once(0)
            .chain(high_byte_iter.interleave(low_byte_iter))
            .collect::<Vec<u8>>();
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
        let mut chars = 30u8..;
        let mut zeroes = iter::once(0u8).cycle();

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
