use nom::IResult;
use nom::bytes::complete::take;
use nom::number::complete::{be_u8, be_u16};
use nom::multi::count;

use super::*;
use crate::text::Zscii;

fn dict_entry(entry_size: usize) -> impl Fn(&[u8]) -> IResult<&[u8], DictEntry> {
    move |input| dict_entry_h(input, entry_size)
}

fn dict_entry_h(input: &[u8], entry_size: usize) -> IResult<&[u8], DictEntry> {
    let (input, word) = take(DICT_WORD_LEN_BYTES)(input)?;
    let word: &[u8; DICT_WORD_LEN_BYTES] = word.try_into().unwrap();

    let data_len = entry_size - DICT_WORD_LEN_BYTES;
    let (input, data) = take(data_len)(input)?;

    let dict_entry = DictEntry { word, data };
    Ok((input, dict_entry))
}

fn word_seps(input: &[u8]) -> IResult<&[u8], Vec<Zscii>> {
    let (input, len) = be_u8(input)?;
    let (input, seps) = take(len)(input)?;
    let word_seps = seps.iter().map(|&b| Zscii(b)).collect::<Vec<_>>();
    Ok((input, word_seps))
}

pub fn dict_table(input: &[u8]) -> IResult<&[u8], DictTable> {
    let (input, word_seps) = word_seps(input)?;
    let (input, entry_len) = be_u8(input)?;
    let (input, num_entries) = be_u16(input)?;
    let entry_len = entry_len as usize;
    let num_entries = num_entries as usize;
    let (input, entries) = count(dict_entry(entry_len), num_entries)(input)?;
    let dict_table = DictTable { word_seps, entries };
    Ok((input, dict_table))
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io;
    use std::io::prelude::*;
    use std::fs::File;

    #[test]
    fn test_parse_dict_table() {
        let mut f = File::open("assets/Alys.z5").unwrap();
        let mut src = Vec::new();
        f.read_to_end(&mut src).unwrap();

        let table_addr = u16::from_be_bytes([src[8], src[9]]) as usize;
        let (_, dict_table) = dict_table(&src[table_addr..]).unwrap();
    }

}
