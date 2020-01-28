#[inline]
pub fn assemble_u16(high: u8, low: u8) -> u16 {
    let high = high as u16;
    let low = low as u16;
    (high << 8) | low
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_assemble_u16() {
        assert_eq!(assemble_u16(0x12, 0x34), 0x1234);
    }

}
