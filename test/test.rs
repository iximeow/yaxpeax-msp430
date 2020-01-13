extern crate yaxpeax_arch;
extern crate yaxpeax_msp430_mc;

use yaxpeax_arch::Decodable;
use yaxpeax_msp430_mc::{Instruction, Opcode};

#[test]
fn test_decode() {
    let data = [0x02, 0x12];
    let mut instr = Instruction::blank();
    instr.decode_into(data.iter().map(|x| *x));
    assert!(instr.opcode == Opcode::PUSH);

    let data = [0xb1, 0x92, 0x8d, 0x49];
    let mut instr = Instruction::blank();
    instr.decode_into(data.iter().map(|x| *x));
    assert!(instr.opcode == Opcode::CMP);

    let data = [0x12, 0x00, 0x3f, 0x40];
    let mut instr = Instruction::blank();
    instr.decode_into(data.iter().map(|x| *x));
    assert!(instr.opcode == Opcode::RRC);

    let data = [0x20, 0x0e];
    let mut instr = Instruction::blank();
    instr.decode_into(data.iter().map(|x| *x));
    assert!(instr.opcode == Opcode::PUSH);
}
