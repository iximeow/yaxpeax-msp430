extern crate yaxpeax_arch;

use yaxpeax_arch::{Arch, Decodable, LengthedInstruction};

mod display;

pub struct MSP430;
impl Arch for MSP430 {
    type Address = u16;
    type Instruction = Instruction;
    type Operand = Operand;
}

#[derive(Debug, Copy, Clone)]
pub struct Instruction {
    pub opcode: Opcode,
    pub op_width: Width,
    pub operands: [Operand; 2]
}

#[derive(Debug, Copy, Clone)]
pub enum Width {
    W, B
}

impl Instruction {
    pub fn blank() -> Instruction {
        Instruction {
            opcode: Opcode::Invalid(0xffff),
            op_width: Width::W,
            operands: [Operand::Nothing, Operand::Nothing]
        }
    }
}

impl LengthedInstruction for Instruction {
    type Unit = <MSP430 as Arch>::Address;
    fn len(&self) -> Self::Unit {
        let mut size = 2;
        match self.operands[0] {
            Operand::Indexed(_, _) |
            Operand::Symbolic(_) |
            Operand::Immediate(_) |
            Operand::Absolute(_) => { size += 2; },
            _ => {}
        };
        match self.operands[1] {
            Operand::Indexed(_, _) |
            Operand::Symbolic(_) |
            Operand::Immediate(_) |
            Operand::Absolute(_) => { size += 2; },
            _ => {}
        };
        size
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Opcode {
    Invalid(u16),
    RRC,
    SWPB,
    RRA,
    SXT,
    PUSH,
    CALL,
    RETI,
    JNE,
    JEQ,
    JNC,
    JC,
    JN,
    JGE,
    JL,
    JMP,
    MOV,
    ADD,
    ADDC,
    SUBC,
    SUB,
    CMP,
    DADD,
    BIT,
    BIC,
    BIS,
    XOR,
    AND
}

#[derive(Debug, Copy, Clone)]
pub enum Operand {
    Register(u8),
    Indexed(u8, u16),
    RegisterIndirect(u8),
    IndirectAutoinc(u8),
    Symbolic(u16),
    Immediate(u16),
    Absolute(u16),
    Offset(i16),
    Const4,
    Const8,
    Const0,
    Const1,
    Const2,
    ConstNeg1,
    Nothing
}

impl Decodable for Instruction {
    fn decode<'a, T: IntoIterator<Item=&'a u8>>(bytes: T) -> Option<Self> {
        let mut instr = Instruction::blank();
        match instr.decode_into(bytes) {
            Some(_) => Some(instr),
            None => None
        }
    }
    fn decode_into<'a, T: IntoIterator<Item=&'a u8>>(&mut self, bytes: T) -> Option<()> {
        let mut bytes_iter = bytes.into_iter();
        let word: Vec<&'a u8> = bytes_iter.by_ref().take(2).collect();

        let fullword = match word[..] {
            [] | [_] => { return None; },
            [low, high] => (*high as u16) << 8 | (*low as u16),
            _ => unreachable!()
        };

        fn decode_operand<'a, T: Iterator<Item=&'a u8>>(bytes: &mut T, reg: u8, mode: u8, oper: &mut Operand) -> bool {
            *oper = match reg {
                0 => {
                    if mode == 0 {
                        Operand::Register(reg)
                    } else if mode == 1 {
                        let next = match bytes.take(2).collect::<Vec<&u8>>()[..] {
                            [] | [_] => { return false; },
                            [low, high] => { ((*high as u16) << 8) | (*low as u16) },
                            _ => { unreachable!() }
                        };
                        Operand::Symbolic(next)
                    } else if mode == 2 {
                        Operand::RegisterIndirect(reg)
                    } else if mode == 3 {
                        let next = match bytes.take(2).collect::<Vec<&u8>>()[..] {
                            [] | [_] => { return false; },
                            [low, high] => { ((*high as u16) << 8) | (*low as u16) },
                            _ => { unreachable!() }
                        };
                        Operand::Immediate(next)
                    } else {
                        return false;
                    }
                },
                2 => {
                    match mode {
                        0 => { Operand::Register(reg) },
                        1 => {
                            let next = match bytes.take(2).collect::<Vec<&u8>>()[..] {
                                [] | [_] => { return false; },
                                [low, high] => { ((*high as u16) << 8) | (*low as u16) },
                                _ => { unreachable!() }
                            };
                            Operand::Absolute(next)
                        },
                        2 => { Operand::Const8 },
                        3 => { Operand::Const4 },
                        _ => { unreachable!() }
                    }
                },
                3 => {
                    match mode {
                        0 => { Operand::Const0 },
                        1 => { Operand::Const1 },
                        2 => { Operand::Const2 },
                        3 => { Operand::ConstNeg1 },
                        _ => { unreachable!() }
                    }
                },
                _ => {
                    match mode {
                        0 => { Operand::Register(reg) },
                        1 => {
                            let next = match bytes.take(2).collect::<Vec<&u8>>()[..] {
                                [] | [_] => { return false; },
                                [low, high] => { ((*high as u16) << 8) | (*low as u16) },
                                _ => { unreachable!() }
                            };
                            Operand::Indexed(reg, next)
                        },
                        2 => { Operand::RegisterIndirect(reg) },
                        3 => { Operand::IndirectAutoinc(reg) },
                        _ => { unreachable!() }
                    }
                }
            };
            return true;
        }

        self.op_width = Width::W;

        match fullword {
            /*
            instrword if instrword < 0x1000 => {
                // MSP430X instructions go here
                self.opcode = Opcode::Invalid(instrword);
                self.operands[0] = Operand::Nothing;
                self.operands[1] = Operand::Nothing;
                return None;
            }, */
            instrword if instrword < 0x2000 => {
                // microcorruption msp430 is non-standard and accepts invalid instructions..
                let (opcode_idx, operands) = ((instrword & 0x0380) >> 7, instrword & 0x7f);
                match opcode_idx {
                    x if x < 6 => {
                        self.opcode = [
                            Opcode::RRC,
                            Opcode::SWPB,
                            Opcode::RRA,
                            Opcode::SXT,
                            Opcode::PUSH,
                            Opcode::CALL
                        ][x as usize];
                        self.op_width = if operands & 0b01000000 == 0 {
                            Width::W
                        } else {
                            if x == 1 || x == 3 || x == 5 {
                                self.opcode = Opcode::Invalid(instrword);
                                return None;
                            }
                            Width:: B
                        };
                        #[allow(non_snake_case)]
                        let (As, source) = (
                            ((instrword & 0x0030) >> 4) as u8,
                            (instrword & 0x000f) as u8
                        );
                        if !decode_operand(&mut bytes_iter, source, As, &mut self.operands[0]) {
                            self.opcode = Opcode::Invalid(instrword);
                            return None;
                        };
                        self.operands[1] = Operand::Nothing;
                        Some(())
                    },
                    6 => {
                        if operands == 0 {
                            self.opcode = Opcode::RETI;
                            self.operands[0] = Operand::Nothing;
                            self.operands[1] = Operand::Nothing;
                            Some(())
                        } else {
                            self.opcode = Opcode::Invalid(instrword);
                            return None;
                        }
                    }
                    7 => {
                        self.opcode = Opcode::Invalid(instrword);
                        return None;
                    }
                    _ => {
                        unreachable!();
                    }
                }
            },
            instrword if instrword < 0x4000 => {
                let (opcode_idx, offset) = ((instrword & 0x1c00) >> 10, instrword & 0x3ff);
                self.opcode = [
                    Opcode::JNE,
                    Opcode::JEQ,
                    Opcode::JNC,
                    Opcode::JC,
                    Opcode::JN,
                    Opcode::JGE,
                    Opcode::JL,
                    Opcode::JMP
                ][opcode_idx as usize];
                self.operands[0] = Operand::Offset(((offset as i16) << 6) >> 6);
                self.operands[1] = Operand::Nothing;
                Some(())
            },
            instrword @ _ => {
                let (opcode_idx, operands) = ((instrword & 0xf000) >> 12, instrword & 0x0fff);
                self.opcode = [
                    Opcode::MOV,
                    Opcode::ADD,
                    Opcode::ADDC,
                    Opcode::SUBC,
                    Opcode::SUB,
                    Opcode::CMP,
                    Opcode::DADD,
                    Opcode::BIT,
                    Opcode::BIC,
                    Opcode::BIS,
                    Opcode::XOR,
                    Opcode::AND
                ][(opcode_idx - 4) as usize];
                self.op_width = if operands & 0b01000000 == 0 { Width::W } else { Width:: B };
                #[allow(non_snake_case)]
                let (source, Ad, As, dest) = (
                    ((instrword & 0x0f00) >> 8) as u8,
                    ((instrword & 0x0080) >> 7) as u8,
                    ((instrword & 0x0030) >> 4) as u8,
                    (instrword & 0x000f) as u8
                );
                if !decode_operand(&mut bytes_iter, source, As, &mut self.operands[0]) {
                    self.opcode = Opcode::Invalid(instrword);
                    return None;
                }
                if !decode_operand(&mut bytes_iter, dest, Ad, &mut self.operands[1]) {
                    self.opcode = Opcode::Invalid(instrword);
                    return None;
                }
                Some(())
            }
        }
    }
}

