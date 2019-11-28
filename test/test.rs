extern crate yaxpeax_arch;
extern crate yaxpeax_msp430_mc;

use yaxpeax_arch::{Arch, Decoder};
use yaxpeax_msp430_mc::{Opcode, MSP430};

#[test]
fn test_decode() {
    let decoder = <MSP430 as Arch>::Decoder::default();

    let data = [0x02, 0x12];
    let instr = decoder.decode(data.iter().cloned()).unwrap();
    assert!(instr.opcode == Opcode::PUSH);

    let data = [0xb1, 0x92, 0x8d, 0x49];
    let instr = decoder.decode(data.iter().cloned()).unwrap();
    assert!(instr.opcode == Opcode::CMP);

    let data = [0x12, 0x00, 0x3f, 0x40];
    let instr = decoder.decode(data.iter().cloned()).unwrap();
    assert!(instr.opcode == Opcode::RRC);

    let data = [0x20, 0x0e];
    let instr = decoder.decode(data.iter().cloned()).unwrap();
    assert!(instr.opcode == Opcode::PUSH);
}
