// a ZChar is a 5-bit value
#[derive(Debug, Clone, Copy, PartialEq)]
struct ZChar(u8);

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

impl<I: Iterator<Item=u16>> Iterator for ZChars<I> {
    type Item = ZChar;
    fn next(&mut self) -> Option<Self::Item> {
        let word = match self.current_word {
            Some(w) => w,
            None => {
                let w = TextWord::from(self.text.next()?);
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

trait ZCharDecodable<I: Iterator<Item=ZChar>> {
    fn zchars(self) -> I;
}

impl<I: Iterator<Item=u16>> ZCharDecodable<ZChars<I>> for I {
    fn zchars(self) -> ZChars<I> {
        ZChars {
            text: self,
            current_word: None,
            index: 0,
        }
    }
}


#[test]
fn test_zchar_decode() {
    let text: Vec<u16> = vec![0x3551, 0xc685, 0x77df];
    // let mut zchar_iter = text.iter().zchars();
    let mut zchar_iter = ZChars {
        text: text.iter().map(|x: &u16| *x),
        current_word: None,
        index: 0,
    };
    assert!(zchar_iter.next().unwrap() == ZChar(0x0d));
    assert!(zchar_iter.next().unwrap() == ZChar(0x0a));
    assert!(zchar_iter.next().unwrap() == ZChar(0x11));
    assert!(zchar_iter.next().unwrap() == ZChar(0x11));
    assert!(zchar_iter.next().unwrap() == ZChar(0x14));
    assert!(zchar_iter.next().unwrap() == ZChar(0x05));
    assert!(zchar_iter.next().is_none());
}

// #[cfg(test)]
// mod tests {
//     use crate::*;

// }



fn main() {
    let text: Vec<u16> = vec![0x3551, 0xc685, 0x77df];
    // let mut zchar_iter = text.iter().zchars();
    let mut zchar_iter = ZChars {
        text: text.iter().map(|x: &u16| *x),
        current_word: None,
        index: 0,
    };
    println!("{:#04x}", zchar_iter.next().unwrap().0);
    println!("{:#04x}", zchar_iter.next().unwrap().0);
    println!("{:#04x}", zchar_iter.next().unwrap().0);
    println!("{:#04x}", zchar_iter.next().unwrap().0);
    println!("{:#04x}", zchar_iter.next().unwrap().0);
    println!("{:#04x}", zchar_iter.next().unwrap().0);
}
