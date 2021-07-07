extern crate yaxpeax_arch;
extern crate yaxpeax_msp430;

use yaxpeax_arch::{Arch, Decoder, U8Reader};
use yaxpeax_msp430::{Opcode, MSP430};

#[test]
fn test_decode() {
    fn decode(bytes: &[u8]) -> yaxpeax_msp430::Instruction {
        let decoder = <MSP430 as Arch>::Decoder::default();
        let mut reader = U8Reader::new(bytes);
        decoder.decode(&mut reader).unwrap()
    }

    let instr = decode(&[0x02, 0x12][..]);
    assert!(instr.opcode == Opcode::PUSH);

    let instr = decode(&[0xb1, 0x92, 0x8d, 0x49][..]);
    assert!(instr.opcode == Opcode::CMP);

    let instr = decode(&[0x12, 0x00, 0x3f, 0x40][..]);
    assert!(instr.opcode == Opcode::RRC);

    let instr = decode(&[0x20, 0x0e][..]);
    assert!(instr.opcode == Opcode::PUSH);
}
