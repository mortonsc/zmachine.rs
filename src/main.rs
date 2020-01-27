#[macro_use]
extern crate lazy_static;

use std::iter::FromIterator;
use std::convert::{TryInto, TryFrom};

// a ZChar is a 5-bit value
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct ZChar(u8);

// Zscii chars are nominally 10bit but only the bottom 8 are used
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Zscii(u8);

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

struct ZChars<I> {
    text: I,
    current_word: Option<TextWord>,
    index: usize,
}

impl<I> ZChars<I> {
    fn new(iter: I) -> Self {
        ZChars {
            text: iter,
            current_word: None,
            index: 0,
        }
    }
}

impl<'a, I: Iterator<Item = &'a u16>> Iterator for ZChars<I> {
    type Item = ZChar;
    fn next(&mut self) -> Option<Self::Item> {
        let word = match self.current_word {
            Some(w) => w,
            None => {
                let w = TextWord::from(*self.text.next()?);
                self.current_word = Some(w);
                w
            }
        };
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

trait ZCharDecodable {
    type ZCharIter: Iterator<Item=ZChar>;
    fn zchars(self) -> Self::ZCharIter;
}

impl<'a, I: IntoIterator<Item = &'a u16>> ZCharDecodable for I {
    type ZCharIter = ZChars<I::IntoIter>;
    #[inline]
    fn zchars(self) -> Self::ZCharIter {
        ZChars::new(self.into_iter())
    }
}

#[derive(Clone, Copy)]
enum Alph { A0, A1, A2, }

#[derive(Clone, Copy)]
struct AlphTable<'a> {
    rows: [&'a [u8; 26]; 3],
}

impl<'a> AlphTable<'a> {
    fn new(raw_table: &'a [u8; 26*3]) -> AlphTable {
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

static DEFAULT_ALPH_TABLE: AlphTable = AlphTable {
    rows: [
        br#"abcdefghijklmnopqrstuvwxyz"#,
        br#"ABCDEFGHIJKLMNOPQRSTUVWXYZ"#,
        br#"XX0123456789.,!?_#'"/\-:()"#,
    ]
};

#[inline]
fn composite_zscii(zc1: ZChar, zc2: ZChar) -> Zscii {
    Zscii((zc1.0 << 5) & zc2.0)
}

struct ZsciiChars<'a, I> {
    zchars: I,
    alph: Alph,
    alph_table: AlphTable<'a>,
}


impl<'a, I> ZsciiChars<'a, I> {
    fn new(zchars: I, alph_table: AlphTable<'a>) -> ZsciiChars<I> {
        ZsciiChars {
            zchars,
            alph: Alph::A0,
            alph_table,
        }
    }
}

impl<'a, I: Iterator<Item=ZChar>> Iterator for ZsciiChars<'a, I> {
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

trait ZsciiDecodable<'a> {
    type ZsciiIter: Iterator<Item=Zscii>;
    fn zscii(self, alph_table: AlphTable<'a>) -> Self::ZsciiIter;
}

impl<'a, I: IntoIterator<Item=ZChar>> ZsciiDecodable<'a> for I {
    type ZsciiIter = ZsciiChars<'a, I::IntoIter>;
    fn zscii(self, alph_table: AlphTable<'a>) -> Self::ZsciiIter {
        ZsciiChars::new(self.into_iter(), alph_table)
    }
}

struct UnicodeTransTable {
    table: Vec<char>
}

lazy_static! {
    static ref DEFAULT_UTT: UnicodeTransTable = {
        let table = "äöüÄÖÜß»«ëïÿËÏáéíóúýÁÉÍÓÚÝ\
                     àèìòùÀÈÌÒÙâêîôûÂÊÎÔÛåÅøØ\
                     ãñõÃÑÕæÆçÇþðÞÐ£œŒ¡¿"
                     .chars().collect::<Vec<char>>();
        assert!(table.len() == (223 - 155 + 1));
        UnicodeTransTable {table}
    };
}

impl UnicodeTransTable {
    fn from_byte_array(spec: &[u8]) -> Option<Self> {
        assert!(spec.len() % 2 == 0);
        let high_bytes = spec.iter();
        let mut low_bytes = spec.iter();
        low_bytes.next();
        let mut table: Vec<char> = Vec::with_capacity(spec.len() / 2);
        for (high, low) in high_bytes.zip(low_bytes) {
                let high = (*high as u32) << 8;
                let low = *low as u32;
                let c = char::try_from(high & low).ok()?;
                table.push(c);
        }
        Some(UnicodeTransTable{table})
    }
}

#[derive(Debug)]
enum ZsciiError {
    InputOnly(u8),
    OutputOnly(u8),
    Undefined(u8),
    UnicodeTransMissing(u8),
    NotImplemented(u8),
}

type ZsciiResult<T> = Result<T, ZsciiError>;

impl Zscii {
    fn to_unicode(self, utt: &UnicodeTransTable) -> ZsciiResult<char>{
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

trait UnicodeFromZscii<'a> {
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

#[cfg(test)]
mod tests {
    use crate::*;

    #[test]
    fn test_zchar_decode() {
        let text: Vec<u16> = vec![0x3551, 0xc685, 0x77df];
        // let mut zchar_iter = text.iter().zchars();
        let mut zchar_iter = text.iter().zchars();
        assert!(zchar_iter.next().unwrap() == ZChar(0x0d));
        assert!(zchar_iter.next().unwrap() == ZChar(0x0a));
        assert!(zchar_iter.next().unwrap() == ZChar(0x11));
        assert!(zchar_iter.next().unwrap() == ZChar(0x11));
        assert!(zchar_iter.next().unwrap() == ZChar(0x14));
        assert!(zchar_iter.next().unwrap() == ZChar(0x05));
        assert!(zchar_iter.next().is_none());
    }

    #[test]
    fn test_zscii_decode() {
        let text: Vec<u16> = vec![0x3551, 0xc685, 0x77df];
        let decoded: String = text.zchars()
                                  .zscii(DEFAULT_ALPH_TABLE)
                                  .unicode(&DEFAULT_UTT)
                                  .collect();
        println!("{}", decoded);
        assert!(decoded == "hello");
    }

}



fn main() {
    let text: Vec<u16> = vec![0x3551, 0xc685, 0x77df];
    let mut zchar_iter = text.iter().zchars();
    // let mut zchar_iter = ZChars {
    //     text: text.iter().map(|x: &u16| *x),
    //     current_word: None,
    //     index: 0,
    // };
    println!("{:#04x}", zchar_iter.next().unwrap().0);
    println!("{:#04x}", zchar_iter.next().unwrap().0);
    println!("{:#04x}", zchar_iter.next().unwrap().0);
    println!("{:#04x}", zchar_iter.next().unwrap().0);
    println!("{:#04x}", zchar_iter.next().unwrap().0);
    println!("{:#04x}", zchar_iter.next().unwrap().0);
}
