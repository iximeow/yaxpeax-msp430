#[cfg(feature="use-serde")]
#[macro_use] extern crate serde_derive;
#[cfg(feature="use-serde")]
extern crate serde;
//#[cfg(feature="use-serde")]
//use serde::{Serialize, Deserialize};

extern crate yaxpeax_arch;

use yaxpeax_arch::{Arch, AddressDiff, Decoder, LengthedInstruction, Reader, StandardDecodeError, U16le};
use yaxpeax_arch::{AnnotatingDecoder, DescriptionSink, NullSink};

mod display;
pub use display::NoContext;

#[cfg_attr(feature="use-serde", derive(Serialize, Deserialize))]
#[derive(Debug)]
pub struct MSP430;

impl Arch for MSP430 {
    type Address = u16;
    type Word = U16le;
    type Instruction = Instruction;
    type DecodeError = StandardDecodeError;
    type Decoder = InstDecoder;
    type Operand = Operand;
}

#[derive(Debug, Copy, Clone)]
pub struct Instruction {
    pub opcode: Opcode,
    pub op_width: Width,
    pub operands: [Operand; 2]
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Width {
    W, B
}

impl fmt::Display for Width {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Width::W => {
                f.write_str("w")
            }
            Width::B => {
                f.write_str("b")
            }
        }
    }
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

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
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

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
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

/*
        let mut bytes_iter = bytes.into_iter();
        let word: Vec<u8> = bytes_iter.by_ref().take(2).collect();

        let fullword = match word[..] {
            [] | [_] => { return Err(DecodeError::ExhaustedInput); },
            [low, high] => (high as u16) << 8 | (low as u16),
            _ => unreachable!()
        };
        */

impl Decoder<MSP430> for InstDecoder {
    fn decode_into<T: Reader<<MSP430 as Arch>::Address, <MSP430 as Arch>::Word>>(&self, inst: &mut Instruction, words: &mut T) -> Result<(), <MSP430 as Arch>::DecodeError> {
        self.decode_with_annotation(inst, words, &mut NullSink)
    }
}

#[derive(Clone, PartialEq, Eq)]
pub struct MSP430FieldDescription {
    desc: Option<&'static str>,
    value: MSP430Field,
    // an opaque identifier to link different parts of FieldDescription at different offsets.
    id: u32,
}

#[derive(Clone, PartialEq, Eq)]
pub enum MSP430Field {
    Opcode(Opcode),
    Operand(Operand),
    Width(Width),
    None,
}

use std::fmt;
impl fmt::Display for MSP430FieldDescription {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.value {
            MSP430Field::Opcode(opc) => {
                write!(f, "opcode: {}", opc)?;
            }
            MSP430Field::Operand(operand) => {
                write!(f, "operand: {}", operand)?;
            }
            MSP430Field::Width(width) => {
                write!(f, "width: {}", width)?;
            }
            MSP430Field::None => {
            }
        }

        if let Some(desc) = self.desc {
            if let MSP430Field::None = &self.value {
                // no preceeding text, we can get right into the description
                write!(f, "{}", desc)?;
            } else {
                // we printed something, add a separator for the description
                write!(f, ", {}", desc)?;
            }
        } else {
            // all done, no description to follow
        }

        Ok(())
    }
}

impl AnnotatingDecoder<MSP430> for InstDecoder {
    type FieldDescription = MSP430FieldDescription;

    fn decode_with_annotation<
        T: Reader<<MSP430 as Arch>::Address, <MSP430 as Arch>::Word>,
        S: DescriptionSink<Self::FieldDescription>
    >(&self, inst: &mut Instruction, words: &mut T, sink: &mut S) -> Result<(), <MSP430 as Arch>::DecodeError> {
        let fullword = words.next()?.0;

        fn decode_operand<
            T: Reader<<MSP430 as Arch>::Address, <MSP430 as Arch>::Word>,
            S: DescriptionSink<MSP430FieldDescription>
        >(words: &mut T, sink: &mut S, reg: u8, reg_addr: (u32, u32), mode: u8, mode_addr: (u32, u32), oper: &mut Operand) -> Result<(), StandardDecodeError> {
            let id = reg_addr.0 + 256;
            *oper = match reg {
                0 => {
                    if mode == 0 {
                        sink.record(reg_addr.0, reg_addr.1, MSP430FieldDescription {
                            desc: Some("register number"),
                            value: MSP430Field::Operand(Operand::Register(reg)),
                            id
                        });
                        sink.record(mode_addr.0, mode_addr.1, MSP430FieldDescription {
                            desc: Some("operand mode (register)"),
                            value: MSP430Field::Operand(Operand::Register(reg)),
                            id
                        });
                        Operand::Register(reg)
                    } else if mode == 1 {
                        let disp_start = words.offset() as u32 * 16;
                        let disp = words.next()?.0;
                        sink.record(reg_addr.0, reg_addr.1, MSP430FieldDescription {
                            desc: Some("operand mode (symbolic)"),
                            value: MSP430Field::Operand(Operand::Symbolic(disp)),
                            id
                        });
                        sink.record(mode_addr.0, mode_addr.1, MSP430FieldDescription {
                            desc: Some("operand mode (symbolic)"),
                            value: MSP430Field::Operand(Operand::Symbolic(disp)),
                            id
                        });
                        sink.record(disp_start, disp_start + 15, MSP430FieldDescription {
                            desc: Some("pc-relative offst"),
                            value: MSP430Field::Operand(Operand::Symbolic(disp)),
                            id
                        });
                        Operand::Symbolic(disp)
                    } else if mode == 2 {
                        sink.record(reg_addr.0, reg_addr.1, MSP430FieldDescription {
                            desc: Some("register number"),
                            value: MSP430Field::Operand(Operand::Register(reg)),
                            id
                        });
                        sink.record(mode_addr.0, mode_addr.1, MSP430FieldDescription {
                            desc: Some("operand mode (register)"),
                            value: MSP430Field::Operand(Operand::Register(reg)),
                            id
                        });
                        Operand::RegisterIndirect(reg)
                    } else if mode == 3 {
                        let imm_start = words.offset() as u32 * 16;
                        let imm = words.next()?.0;
                        sink.record(reg_addr.0, reg_addr.1, MSP430FieldDescription {
                            desc: Some("operand mode (immediate)"),
                            value: MSP430Field::Operand(Operand::Immediate(imm)),
                            id
                        });
                        sink.record(mode_addr.0, mode_addr.1, MSP430FieldDescription {
                            desc: Some("operand mode (immediate)"),
                            value: MSP430Field::Operand(Operand::Immediate(imm)),
                            id
                        });
                        sink.record(imm_start, imm_start + 15, MSP430FieldDescription {
                            desc: Some("immediate"),
                            value: MSP430Field::Operand(Operand::Immediate(imm)),
                            id
                        });
                        Operand::Immediate(imm)
                    } else {
                        return Err(StandardDecodeError::InvalidOperand);
                    }
                },
                2 => {
                    match mode {
                        0 => {
                            sink.record(reg_addr.0, reg_addr.1, MSP430FieldDescription {
                                desc: Some("register number"),
                                value: MSP430Field::Operand(Operand::Register(reg)),
                                id
                            });
                            sink.record(mode_addr.0, mode_addr.1, MSP430FieldDescription {
                                desc: Some("operand mode (register)"),
                                value: MSP430Field::Operand(Operand::Register(reg)),
                                id
                            });
                            Operand::Register(reg)
                        },
                        1 => {
                            let abs_start = words.offset() as u32 * 16;
                            let abs = words.next()?.0;
                            sink.record(reg_addr.0, reg_addr.1, MSP430FieldDescription {
                                desc: Some("operand mode (absolute)"),
                                value: MSP430Field::Operand(Operand::Absolute(abs)),
                                id
                            });
                            sink.record(mode_addr.0, mode_addr.1, MSP430FieldDescription {
                                desc: Some("operand mode (absolute)"),
                                value: MSP430Field::Operand(Operand::Absolute(abs)),
                                id
                            });
                            sink.record(abs_start, abs_start + 15, MSP430FieldDescription {
                                desc: Some("absolute address"),
                                value: MSP430Field::Operand(Operand::Absolute(abs)),
                                id
                            });
                            Operand::Absolute(abs)
                        },
                        2 => {
                            sink.record(reg_addr.0, reg_addr.1, MSP430FieldDescription {
                                desc: Some("constant (8)"),
                                value: MSP430Field::Operand(Operand::Const8),
                                id
                            });
                            sink.record(mode_addr.0, mode_addr.1, MSP430FieldDescription {
                                desc: Some("constant (8)"),
                                value: MSP430Field::Operand(Operand::Const8),
                                id
                            });
                            Operand::Const8
                        },
                        3 => {
                            sink.record(reg_addr.0, reg_addr.1, MSP430FieldDescription {
                                desc: Some("constant (4)"),
                                value: MSP430Field::Operand(Operand::Const4),
                                id
                            });
                            sink.record(mode_addr.0, mode_addr.1, MSP430FieldDescription {
                                desc: Some("constant (4)"),
                                value: MSP430Field::Operand(Operand::Const4),
                                id
                            });
                            Operand::Const4
                        },
                        _ => { unreachable!() }
                    }
                },
                3 => {
                    match mode {
                        0 => {
                            sink.record(reg_addr.0, reg_addr.1, MSP430FieldDescription {
                                desc: Some("constant (0)"),
                                value: MSP430Field::Operand(Operand::Const0),
                                id
                            });
                            sink.record(mode_addr.0, mode_addr.1, MSP430FieldDescription {
                                desc: Some("constant (0)"),
                                value: MSP430Field::Operand(Operand::Const0),
                                id
                            });
                            Operand::Const0
                        },
                        1 => {
                            sink.record(reg_addr.0, reg_addr.1, MSP430FieldDescription {
                                desc: Some("constant (1)"),
                                value: MSP430Field::Operand(Operand::Const1),
                                id
                            });
                            sink.record(mode_addr.0, mode_addr.1, MSP430FieldDescription {
                                desc: Some("constant (1)"),
                                value: MSP430Field::Operand(Operand::Const1),
                                id
                            });
                            Operand::Const1
                        },
                        2 => {
                            sink.record(reg_addr.0, reg_addr.1, MSP430FieldDescription {
                                desc: Some("constant (2)"),
                                value: MSP430Field::Operand(Operand::Const2),
                                id
                            });
                            sink.record(mode_addr.0, mode_addr.1, MSP430FieldDescription {
                                desc: Some("constant (2)"),
                                value: MSP430Field::Operand(Operand::Const2),
                                id
                            });
                            Operand::Const2
                        },
                        3 => {
                            sink.record(reg_addr.0, reg_addr.1, MSP430FieldDescription {
                                desc: Some("constant (-1)"),
                                value: MSP430Field::Operand(Operand::ConstNeg1),
                                id
                            });
                            sink.record(mode_addr.0, mode_addr.1, MSP430FieldDescription {
                                desc: Some("constant (-1)"),
                                value: MSP430Field::Operand(Operand::ConstNeg1),
                                id
                            });
                            Operand::ConstNeg1
                        },
                        _ => { unreachable!() }
                    }
                },
                _ => {
                    match mode {
                        0 => {
                            sink.record(reg_addr.0, reg_addr.1, MSP430FieldDescription {
                                desc: Some("register number"),
                                value: MSP430Field::Operand(Operand::Register(reg)),
                                id
                            });
                            sink.record(mode_addr.0, mode_addr.1, MSP430FieldDescription {
                                desc: Some("operand mode (register)"),
                                value: MSP430Field::Operand(Operand::Register(reg)),
                                id
                            });
                            Operand::Register(reg)
                        },
                        1 => {
                            let offset_start = words.offset() as u32 * 16;
                            let offset = words.next()?.0;
                            sink.record(reg_addr.0, reg_addr.1, MSP430FieldDescription {
                                desc: Some("operand mode (indexed)"),
                                value: MSP430Field::Operand(Operand::Indexed(reg, offset)),
                                id
                            });
                            sink.record(mode_addr.0, mode_addr.1, MSP430FieldDescription {
                                desc: Some("operand mode (indexed)"),
                                value: MSP430Field::Operand(Operand::Indexed(reg, offset)),
                                id
                            });
                            sink.record(offset_start, offset_start + 15, MSP430FieldDescription {
                                desc: Some("reg-relative offset"),
                                value: MSP430Field::Operand(Operand::Indexed(reg, offset)),
                                id
                            });
                            Operand::Indexed(reg, offset)
                        },
                        2 => {
                            sink.record(reg_addr.0, reg_addr.1, MSP430FieldDescription {
                                desc: Some("register number"),
                                value: MSP430Field::Operand(Operand::Register(reg)),
                                id
                            });
                            sink.record(mode_addr.0, mode_addr.1, MSP430FieldDescription {
                                desc: Some("operand mode (register indirect)"),
                                value: MSP430Field::Operand(Operand::Register(reg)),
                                id
                            });
                            Operand::RegisterIndirect(reg)
                        },
                        3 => {
                            sink.record(reg_addr.0, reg_addr.1, MSP430FieldDescription {
                                desc: Some("register number"),
                                value: MSP430Field::Operand(Operand::Register(reg)),
                                id
                            });
                            sink.record(mode_addr.0, mode_addr.1, MSP430FieldDescription {
                                desc: Some("operand mode (register indirect, autoinc)"),
                                value: MSP430Field::Operand(Operand::Register(reg)),
                                id
                            });
                            Operand::IndirectAutoinc(reg)
                        },
                        _ => { unreachable!() }
                    }
                }
            };
            return Ok(());
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
                    sink.record(13, 15, MSP430FieldDescription {
                        desc: Some("unallocated opcode space (error)"),
                        value: MSP430Field::None,
                        id: 0,
                    });
                    return Err(StandardDecodeError::InvalidOpcode);
                } else {
                    sink.record(13, 15, MSP430FieldDescription {
                        desc: Some("unallocated opcode space (microcorruption ignores)"),
                        value: MSP430Field::None,
                        id: 0,
                    });
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
                        sink.record(7, 9, MSP430FieldDescription {
                            desc: None,
                            value: MSP430Field::Opcode(inst.opcode),
                            id: 0,
                        });
                        inst.op_width = if operands & 0b01000000 == 0 {
                            Width::W
                        } else {
                            Width::B
                        };
                        sink.record(6, 6, MSP430FieldDescription {
                            desc: None,
                            value: MSP430Field::Width(inst.op_width),
                            id: 1,
                        });
                        if inst.op_width == Width::B && (x == 1 || x == 3 || x == 5) {
                            sink.record(6, 9, MSP430FieldDescription {
                                desc: Some("swpb, sxt, and call all require width=W (error)"),
                                value: MSP430Field::None,
                                id: 0,
                            });
                            inst.opcode = Opcode::Invalid(instrword);
                            return Err(StandardDecodeError::InvalidOpcode);
                        }

                        #[allow(non_snake_case)]
                        let (As, source) = (
                            ((instrword & 0x0030) >> 4) as u8,
                            (instrword & 0x000f) as u8
                        );
                        decode_operand(words, sink, source, (0, 3), As, (4, 5), &mut inst.operands[0])?;
                        inst.operands[1] = Operand::Nothing;
                        Ok(())
                    },
                    6 => {
                        sink.record(7, 9, MSP430FieldDescription {
                            desc: None,
                            value: MSP430Field::Opcode(Opcode::RETI),
                            id: 0,
                        });
                        if operands == 0 {
                            inst.opcode = Opcode::RETI;
                            inst.operands[0] = Operand::Nothing;
                            inst.operands[1] = Operand::Nothing;
                            Ok(())
                        } else {
                            sink.record(0, 6, MSP430FieldDescription {
                                desc: Some("reti requires all-zero operands (error)"),
                                value: MSP430Field::None,
                                id: 0,
                            });
                            inst.opcode = Opcode::Invalid(instrword);
                            return Err(StandardDecodeError::InvalidOperand);
                        }
                    }
                    7 => {
                        sink.record(7, 9, MSP430FieldDescription {
                            desc: Some("invalid opcode"),
                            value: MSP430Field::None,
                            id: 0,
                        });
                        inst.opcode = Opcode::Invalid(instrword);
                        return Err(StandardDecodeError::InvalidOpcode);
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
                sink.record(13, 15, MSP430FieldDescription {
                    desc: Some("Jcc or JMP"),
                    value: MSP430Field::Opcode(inst.opcode),
                    id: 0,
                });
                sink.record(10, 12, MSP430FieldDescription {
                    desc: None,
                    value: MSP430Field::Opcode(inst.opcode),
                    id: 0,
                });

                let offset = ((offset as i16) << 6) >> 6;
                sink.record(0, 9, MSP430FieldDescription {
                    desc: Some("relative offset (sign-extended to i16)"),
                    value: MSP430Field::Operand(Operand::Offset(offset)),
                    id: 2,
                });
                inst.operands[0] = Operand::Offset(offset);
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
                sink.record(12, 15, MSP430FieldDescription {
                    desc: None,
                    value: MSP430Field::Opcode(inst.opcode),
                    id: 0,
                });

                inst.op_width = if operands & 0b01000000 == 0 { Width::W } else { Width:: B };
                sink.record(6, 6, MSP430FieldDescription {
                    desc: None,
                    value: MSP430Field::Width(inst.op_width),
                    id: 1,
                });

                #[allow(non_snake_case)]
                let (source, Ad, As, dest) = (
                    ((instrword & 0x0f00) >> 8) as u8,
                    ((instrword & 0x0080) >> 7) as u8,
                    ((instrword & 0x0030) >> 4) as u8,
                    (instrword & 0x000f) as u8
                );
                decode_operand(words, sink, source, (8, 11), As, (4, 5), &mut inst.operands[0])?;
                decode_operand(words, sink, dest, (0, 3), Ad, (7, 7), &mut inst.operands[1])?;
                Ok(())
            }
        }
    }
}

