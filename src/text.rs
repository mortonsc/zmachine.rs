use std::convert::{TryInto, TryFrom};
use itertools::Itertools;

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
        let text: u16 = word.zchars.iter().fold(0, |acc, zc| (acc << 5) & (zc.0 as u16));
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
            },
            3 => {
                if word.is_end {
                    None
                } else {
                    self.current_word = None;
                    self.index = 0;
                    self.next()
                }
            },
            _ => panic!(),
        }
    }
}

pub trait ZCharDecodable {
    type ZCharIter: Iterator<Item=ZChar>;
    fn zchars(self) -> Self::ZCharIter;
}

impl<'a, I: IntoIterator<Item = &'a u8>> ZCharDecodable for I {
    type ZCharIter = ZCharsFromBytes<I::IntoIter>;

    #[inline]
    fn zchars(self) -> Self::ZCharIter {
        ZCharsFromBytes::new(self.into_iter())
    }
}

#[derive(Clone, Copy)]
enum Alph { A0, A1, A2, }

#[derive(Clone, Copy)]
pub struct AlphTable<'a> {
    rows: [&'a [u8; 26]; 3],
}

impl<'a> AlphTable<'a> {
    pub fn new(raw_table: &'a [u8; 26*3]) -> AlphTable {
        AlphTable {
            rows: [
                // rust requires try_into() but we know it will succeed
                // because we select the right size with constant indices
                raw_table[0..26].try_into().unwrap(),
                raw_table[26..52].try_into().unwrap(),
                raw_table[54..78].try_into().unwrap()
            ],
        }
    }
}

impl<'a> AlphTable<'a> {
    fn lookup_zscii(&self, zc: ZChar, alph: Alph) -> Zscii {
        const ZCHAR_MIN: u8 = 6;
        assert!(ZCHAR_MIN <= zc.0);
        let index = (zc.0 - ZCHAR_MIN) as usize;
        let zscii_byte = match (alph, index) {
            (Alph::A0, _) => self.rows[0][index],
            (Alph::A1, _) => self.rows[1][index],
            (Alph::A2, 0) => panic!(), // compound char; can't handle that here
            (Alph::A2, 1) => b'\n',    // permanently mapped to newline
            (Alph::A2, _) => self.rows[2][index],
        };
        Zscii(zscii_byte)
    }
}

pub static DEFAULT_ALPH_TABLE: AlphTable = AlphTable {
    rows: [
        br#"abcdefghijklmnopqrstuvwxyz"#,
        br#"ABCDEFGHIJKLMNOPQRSTUVWXYZ"#,
        br#"XX0123456789.,!?_#'"/\-:()"#,
    ]
};

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

impl<'a, I: Iterator<Item=ZChar>> Iterator for ZsciiFromZChars<'a, I> {
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
            },
            (_, 5) => {
                self.alph = Alph::A2;
                self.next()?
            },
            (Alph::A2, 6) =>  composite_zscii(self.zchars.next()?, self.zchars.next()?),
            (_, _) => self.alph_table.lookup_zscii(zc, self.alph),
        };
        self.alph = Alph::A0;
        Some(zscii)
    }
}

pub trait ZsciiDecodable<'a> {
    type ZsciiIter: Iterator<Item=Zscii>;
    fn zscii(self, alph_table: AlphTable<'a>) -> Self::ZsciiIter;
}

impl<'a, I: IntoIterator<Item=ZChar>> ZsciiDecodable<'a> for I {
    type ZsciiIter = ZsciiFromZChars<'a, I::IntoIter>;
    fn zscii(self, alph_table: AlphTable<'a>) -> Self::ZsciiIter {
        ZsciiFromZChars::new(self.into_iter(), alph_table)
    }
}

pub struct UnicodeTransTable {
    table: Vec<char>
}

lazy_static! {
    pub static ref DEFAULT_UTT: UnicodeTransTable = {
        let table = "äöüÄÖÜß»«ëïÿËÏáéíóúýÁÉÍÓÚÝ\
                     àèìòùÀÈÌÒÙâêîôûÂÊÎÔÛåÅøØ\
                     ãñõÃÑÕæÆçÇþðÞÐ£œŒ¡¿"
                     .chars().collect::<Vec<char>>();
        assert!(table.len() == (223 - 155 + 1));
        UnicodeTransTable {table}
    };
}

impl UnicodeTransTable {
    pub fn from_byte_array(spec: &[u8]) -> Option<Self> {
        assert!(spec.len() % 2 == 0);
        let table: Vec<char> = spec.iter()
            .batching(|it| {
                let high = *it.next()?;
                //TODO: handle as an error if missing
                let low = *it.next()?;
                Some(u32::from_be_bytes([0x00, 0x00, high, low]))
            })
            .map(|cp| char::try_from(cp).ok()) //TODO: better error handling
            .collect::<Option<Vec<_>>>()?;

        Some(UnicodeTransTable{table})
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
    pub fn to_unicode(self, utt: &UnicodeTransTable) -> ZsciiResult<char> {
        let zb = self.0;
        match zb {
            0 => Ok('\0'),
            8 => Err(ZsciiError::InputOnly(zb)), //delete
            9 => Ok('\t'),
            11 => Ok(' '), // "sentence space"
            13 => Ok('\n'),
            27 => Err(ZsciiError::InputOnly(zb)), // esc
            32..=126 => Ok(zb as char), // standard ascii
            129..=154 => Err(ZsciiError::InputOnly(zb)), // arrow keys, function keys, keypad
            155..=251 => {
                if let Some(trans) = utt.table.get((zb as usize) - 155) {
                    Ok(*trans)
                } else {
                    Err(ZsciiError::UnicodeTransMissing(zb))
                }
            }
            252..=254 => Err(ZsciiError::InputOnly(zb)), // mouse clicks
            _ => Err(ZsciiError::Undefined(zb)),
        }
    }
}

pub trait UnicodeFromZscii<'a> {
    type UnicodeIter: Iterator<Item=char>;

    fn unicode(self, utt: &'a UnicodeTransTable) -> Self::UnicodeIter;
}

impl<'a, I: 'a + IntoIterator<Item=Zscii>> UnicodeFromZscii<'a> for I {
    type UnicodeIter = Box<dyn Iterator<Item=char> + 'a>;

    fn unicode(self, utt: &'a UnicodeTransTable) -> Self::UnicodeIter {
        // TODO: log errors that occur during decoding
        Box::new(self.into_iter().filter_map(move |z| z.to_unicode(utt).ok()))
    }
}

impl UnicodeTransTable {
    fn find_zscii_trans(&self, uni: char) -> Option<Zscii> {
        self.table.iter()
            .position(|&c| c == uni)
            .map(|idx| Zscii(u8::try_from(idx + 155).unwrap()))
    }
}

impl Zscii {
    fn from_unicode(uni: char, utt: &UnicodeTransTable) -> Option<Zscii> {
        match uni {
            ' '..='~' => Some(Zscii(u8::try_from(uni as u32).unwrap())),
            '\n' => Some(Zscii(0x0d)),
            _ => utt.find_zscii_trans(uni),
        }
    }
}

struct ZsciiToZChars<'a, I> {
    zscii_chars: I,
    alph_table: AlphTable<'a>,
    buf: Vec<ZChar>,
}

impl<'a, I> Iterator for ZsciiToZChars<'a, I>
where I: Iterator<Item=Zscii>
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
        } else if let Some((zchar, alph)) = self.alph_table.find_zchar_trans(zscii) {
            match alph {
                Alph::A0 => Some(zchar),
                Alph::A1 => {
                    self.buf.push(zchar);
                    Some(ZChar::SHIFT_A1)
                },
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

impl<'a> AlphTable<'a> {
    fn find_zchar_trans(&self, zscii: Zscii) -> Option<(ZChar, Alph)> {
        if let Some(idx) = self.rows[0].iter().position(|&z| z == zscii.0) {
            Some((ZChar(u8::try_from(idx + 6).unwrap()), Alph::A0))
        } else if let Some(idx) = self.rows[1].iter().position(|&z| z == zscii.0) {
            Some((ZChar(u8::try_from(idx + 6).unwrap()), Alph::A1))
        } else if let Some(idx) = self.rows[2][2..].iter().position(|&z| z == zscii.0) {
            Some((ZChar(u8::try_from(idx + 6).unwrap()), Alph::A2))
        } else {
            None
        }

    }
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_zchar_decode() {
        let text: Vec<u8> = vec![0x35, 0x51, 0xc6, 0x85, 0x77, 0xdf];
        // let mut zchar_iter = text.iter().zchars();
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
        let decoded: String = text.zchars()
                                  .zscii(DEFAULT_ALPH_TABLE)
                                  .unicode(&DEFAULT_UTT)
                                  .collect();
        assert_eq!(decoded, "hello");
    }

}



