#[cfg(feature="use-serde")]
#[macro_use] extern crate serde_derive;
#[cfg(feature="use-serde")]
extern crate serde;
//#[cfg(feature="use-serde")]
//use serde::{Serialize, Deserialize};

extern crate yaxpeax_arch;

use yaxpeax_arch::{Arch, AddressDiff, Decoder, LengthedInstruction};

mod display;
pub use display::NoContext;

#[cfg(feature="use-serde")]
#[derive(Debug, Serialize, Deserialize)]
pub struct MSP430;

#[cfg(not(feature="use-serde"))]
#[derive(Debug)]
pub struct MSP430;

impl Arch for MSP430 {
    type Address = u16;
    type Instruction = Instruction;
    type DecodeError = DecodeError;
    type Decoder = InstDecoder;
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

impl Default for Instruction {
    fn default() -> Instruction {
        Instruction {
            opcode: Opcode::Invalid(0xffff),
            op_width: Width::W,
            operands: [Operand::Nothing, Operand::Nothing]
        }
    }
}

impl LengthedInstruction for Instruction {
    type Unit = AddressDiff<<MSP430 as Arch>::Address>;
    fn min_size() -> Self::Unit {
        AddressDiff::from_const(2)
    }
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
        AddressDiff::from_const(size)
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

#[derive(Debug, PartialEq)]
pub enum DecodeError {
    ExhaustedInput,
    InvalidOpcode,
    InvalidOperand,
}

impl yaxpeax_arch::DecodeError for DecodeError {
    fn data_exhausted(&self) -> bool { self == &DecodeError::ExhaustedInput }
    fn bad_opcode(&self) -> bool { self == &DecodeError::InvalidOpcode }
    fn bad_operand(&self) -> bool { self == &DecodeError::InvalidOperand }
}

impl yaxpeax_arch::Instruction for Instruction {
    // TODO: this is wrong!!
    fn well_defined(&self) -> bool { true }
}

#[derive(Debug)]
pub struct InstDecoder {
    flags: u8
}

impl InstDecoder {
    pub fn minimal() -> Self {
        InstDecoder {
            flags: 0
        }
    }

    pub fn with_microcorruption(mut self) -> Self {
        self.flags |= 1;
        self
    }

    pub fn microcorruption_quirks(&self) -> bool {
        (self.flags & 1) != 0
    }
}

impl Default for InstDecoder {
    fn default() -> Self {
        InstDecoder {
            flags: 0xff
        }
    }
}

impl Decoder<Instruction> for InstDecoder {
    type Error = DecodeError;

    fn decode_into<T: IntoIterator<Item=u8>>(&self, inst: &mut Instruction, bytes: T) -> Result<(), Self::Error> {
        let mut bytes_iter = bytes.into_iter();
        let word: Vec<u8> = bytes_iter.by_ref().take(2).collect();

        let fullword = match word[..] {
            [] | [_] => { return Err(DecodeError::ExhaustedInput); },
            [low, high] => (high as u16) << 8 | (low as u16),
            _ => unreachable!()
        };

        fn decode_operand<T: Iterator<Item=u8>>(bytes: &mut T, reg: u8, mode: u8, oper: &mut Operand) -> bool {
            *oper = match reg {
                0 => {
                    if mode == 0 {
                        Operand::Register(reg)
                    } else if mode == 1 {
                        let next = match bytes.take(2).collect::<Vec<u8>>()[..] {
                            [] | [_] => { return false; },
                            [low, high] => { ((high as u16) << 8) | (low as u16) },
                            _ => { unreachable!() }
                        };
                        Operand::Symbolic(next)
                    } else if mode == 2 {
                        Operand::RegisterIndirect(reg)
                    } else if mode == 3 {
                        let next = match bytes.take(2).collect::<Vec<u8>>()[..] {
                            [] | [_] => { return false; },
                            [low, high] => { ((high as u16) << 8) | (low as u16) },
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
                            let next = match bytes.take(2).collect::<Vec<u8>>()[..] {
                                [] | [_] => { return false; },
                                [low, high] => { ((high as u16) << 8) | (low as u16) },
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
                            let next = match bytes.take(2).collect::<Vec<u8>>()[..] {
                                [] | [_] => { return false; },
                                [low, high] => { ((high as u16) << 8) | (low as u16) },
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

        inst.op_width = Width::W;

        match fullword {
            /*
            instrword if instrword < 0x1000 => {
                // MSP430X instructions go here
                inst.opcode = Opcode::Invalid(instrword);
                inst.operands[0] = Operand::Nothing;
                inst.operands[1] = Operand::Nothing;
                return None;
            }, */
            instrword if instrword < 0x2000 => {
                // microcorruption msp430 is non-standard and accepts invalid instructions..
                if !self.microcorruption_quirks() {
                    return Err(DecodeError::InvalidOpcode);
                }

                let (opcode_idx, operands) = ((instrword & 0x0380) >> 7, instrword & 0x7f);
                match opcode_idx {
                    x if x < 6 => {
                        inst.opcode = [
                            Opcode::RRC,
                            Opcode::SWPB,
                            Opcode::RRA,
                            Opcode::SXT,
                            Opcode::PUSH,
                            Opcode::CALL
                        ][x as usize];
                        inst.op_width = if operands & 0b01000000 == 0 {
                            Width::W
                        } else {
                            if x == 1 || x == 3 || x == 5 {
                                inst.opcode = Opcode::Invalid(instrword);
                                return Err(DecodeError::InvalidOpcode);
                            }
                            Width:: B
                        };
                        #[allow(non_snake_case)]
                        let (As, source) = (
                            ((instrword & 0x0030) >> 4) as u8,
                            (instrword & 0x000f) as u8
                        );
                        if !decode_operand(&mut bytes_iter, source, As, &mut inst.operands[0]) {
                            inst.opcode = Opcode::Invalid(instrword);
                            return Err(DecodeError::InvalidOperand);
                        };
                        inst.operands[1] = Operand::Nothing;
                        Ok(())
                    },
                    6 => {
                        if operands == 0 {
                            inst.opcode = Opcode::RETI;
                            inst.operands[0] = Operand::Nothing;
                            inst.operands[1] = Operand::Nothing;
                            Ok(())
                        } else {
                            inst.opcode = Opcode::Invalid(instrword);
                            return Err(DecodeError::InvalidOperand);
                        }
                    }
                    7 => {
                        inst.opcode = Opcode::Invalid(instrword);
                        return Err(DecodeError::InvalidOpcode);
                    }
                    _ => {
                        unreachable!();
                    }
                }
            },
            instrword if instrword < 0x4000 => {
                let (opcode_idx, offset) = ((instrword & 0x1c00) >> 10, instrword & 0x3ff);
                inst.opcode = [
                    Opcode::JNE,
                    Opcode::JEQ,
                    Opcode::JNC,
                    Opcode::JC,
                    Opcode::JN,
                    Opcode::JGE,
                    Opcode::JL,
                    Opcode::JMP
                ][opcode_idx as usize];
                inst.operands[0] = Operand::Offset(((offset as i16) << 6) >> 6);
                inst.operands[1] = Operand::Nothing;
                Ok(())
            },
            instrword @ _ => {
                let (opcode_idx, operands) = ((instrword & 0xf000) >> 12, instrword & 0x0fff);
                inst.opcode = [
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
                inst.op_width = if operands & 0b01000000 == 0 { Width::W } else { Width:: B };
                #[allow(non_snake_case)]
                let (source, Ad, As, dest) = (
                    ((instrword & 0x0f00) >> 8) as u8,
                    ((instrword & 0x0080) >> 7) as u8,
                    ((instrword & 0x0030) >> 4) as u8,
                    (instrword & 0x000f) as u8
                );
                if !decode_operand(&mut bytes_iter, source, As, &mut inst.operands[0]) {
                    inst.opcode = Opcode::Invalid(instrword);
                    return Err(DecodeError::InvalidOperand);
                }
                if !decode_operand(&mut bytes_iter, dest, Ad, &mut inst.operands[1]) {
                    inst.opcode = Opcode::Invalid(instrword);
                    return Err(DecodeError::InvalidOperand);
                }
                Ok(())
            }
        }
    }
}
