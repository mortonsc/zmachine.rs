use std::mem;
use nom::IResult;
use nom::bytes::complete::take;
use nom::number::complete::{be_u8, be_u16};

use super::*;

pub fn abbr_table(input: &[u8]) -> IResult<&[u8], [&[[u8; 2]; 32]; 3]> {
    let (input, row0) = abbr_table_row(input)?;
    let (input, row1) = abbr_table_row(input)?;
    let (input, row2) = abbr_table_row(input)?;
    let table = [ row0, row1, row2 ];
    Ok((input, table))
}

fn abbr_table_row(input: &[u8]) -> IResult<&[u8], &[[u8; 2]; 32]> {
    const LEN: usize = 2 * 32;
    let (input, row_raw) = take(LEN)(input)?;
    let row: &[[u8; 2]; 32];
    unsafe {
        // if there's a better way to do this I don't know what it is yet
        // cast into reference-to-array is a hacky way of getting a raw pointer
        // when I don't feel like looking up how raw pointers work in rust
        // I can't even cast it into &[u8; 64] because that only works up till N=32
        let row_raw: &[u8; 1] = row_raw.try_into().unwrap();
        row = mem::transmute(row_raw);
    }
    Ok((input, row))
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
