use std::mem;
use nom;
use nom::IResult;
use nom::bytes::complete::take;
use nom::number::complete::{be_u8, be_u16};
use nom::combinator::map_res;

use super::*;

pub fn abbr_table(input: &[u8]) -> IResult<&[u8], &[[[u8; 2]; 32]; 3]> {
    const LEN: usize = 2 * 32 * 3;
    let (input, table_raw) = take(LEN)(input)?;
    let table: &[[[u8; 2]; 32]; 3];
    unsafe {
        // don't know an easy way to do this translation with safe code
        // it would be safer/nicer to first cast into [u8; 2*32*3]
        // but that conversion only works for arrays of size <= 32!
        let table_raw = table_raw.as_ptr();
        table = mem::transmute(table_raw);
    }
    Ok((input, table))
}

pub fn custom_alph_table(input: &[u8]) -> IResult<&[u8], CustomAlphTable> {
    let (input, row_a0) = take(26usize)(input)?;
    let (input, row_a1) = take(26usize)(input)?;
    let (input, row_a2) = take(26usize)(input)?;
    let custom_alph_table = CustomAlphTable {
        table: [
            row_a0.try_into().unwrap(),
            row_a1.try_into().unwrap(),
            row_a2.try_into().unwrap(),
        ]
    };
    Ok((input, custom_alph_table))
}

pub fn custom_utt(input: &[u8]) -> IResult<&[u8], CustomUtt> {
    let (input, num_entries) = be_u8(input)?;
    let num_entries = num_entries as usize;
    let (input, table) = take(2 * num_entries)(input)?;
    let custom_utt = CustomUtt { table };
    Ok((input, custom_utt))
}
