use std::convert::TryFrom;
use itertools::Itertools;

pub fn bytes_to_words(bytes: impl Iterator<Item = u8>) -> impl Iterator<Item = u16> {
    bytes.tuples()
        .map(|(high, low)| u16::from_be_bytes([high, low]))
}

pub fn words_to_bytes(words: impl Iterator<Item = u16>) -> impl Iterator<Item = u8> {
    let (high_bytes, low_bytes) = words.tee();
    let high_bytes = high_bytes
        .map(|w| u8::try_from(w >> 8).unwrap());
    let low_bytes = low_bytes
        .map(|w| u8::try_from(w & 0xff).unwrap());
    high_bytes.interleave(low_bytes)
}

#[cfg(test)]
mod tests {
    use super::*;


}
