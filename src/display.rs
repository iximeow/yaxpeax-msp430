use ::{MSP430, Operand, Opcode, Instruction, Width};

use std::fmt::{Display, Formatter};
use std;
use yaxpeax_arch::{Arch, Colorize, ColorSettings, ShowContextual};

impl Display for Instruction {
    fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
        let mut s = String::new();
        self.contextualize(None, 0, None, &mut s).unwrap();
        write!(f, "{}", s)
    }
}

impl <T: std::fmt::Write> ShowContextual<<MSP430 as Arch>::Address, [Option<String>], T> for Instruction {
    fn contextualize(&self, _colors: Option<&ColorSettings>, _address: <MSP430 as Arch>::Address, _context: Option<&[Option<String>]>, out: &mut T) -> std::fmt::Result {
        write!(out, "{}", self.opcode)?;
        match self.op_width {
            Width::B => { write!(out, ".b")? },
            Width::W => { }
        };
        match self.operands[0] {
            Operand::Nothing => return Ok(()),
            x @ _ => {
                write!(out, " {}", x)?;
            }
        };
        match self.operands[1] {
            Operand::Nothing => return Ok(()),
            x @ _ => {
                write!(out, ", {}", x)?;
            }
        };
        Ok(())
    }
}

impl Display for Opcode {
    fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
        match self {
            Opcode::Invalid(a) => { write!(f, "invalid({:04x})", a) },
            Opcode::RRC => { write!(f, "rrc") },
            Opcode::SWPB => { write!(f, "swpb") },
            Opcode::RRA => { write!(f, "rra") },
            Opcode::SXT => { write!(f, "sxt") },
            Opcode::PUSH => { write!(f, "push") },
            Opcode::CALL => { write!(f, "call") },
            Opcode::RETI => { write!(f, "reti") },
            Opcode::JNE => { write!(f, "jne") },
            Opcode::JEQ => { write!(f, "jeq") },
            Opcode::JNC => { write!(f, "jnc") },
            Opcode::JC => { write!(f, "jc") },
            Opcode::JN => { write!(f, "jn") },
            Opcode::JGE => { write!(f, "jge") },
            Opcode::JL => { write!(f, "jl") },
            Opcode::JMP => { write!(f, "jmp") },
            Opcode::MOV => { write!(f, "mov") },
            Opcode::ADD => { write!(f, "add") },
            Opcode::ADDC => { write!(f, "addc") },
            Opcode::SUBC => { write!(f, "subc") },
            Opcode::SUB => { write!(f, "sub") },
            Opcode::CMP => { write!(f, "cmp") },
            Opcode::DADD => { write!(f, "dadd") },
            Opcode::BIT => { write!(f, "bit") },
            Opcode::BIC => { write!(f, "bic") },
            Opcode::BIS => { write!(f, "bis") },
            Opcode::XOR => { write!(f, "xor") },
            Opcode::AND => { write!(f, "and") }
        }
    }
}


impl Display for Operand {
    fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
        let mut s = String::new();
        self.colorize(None, &mut s).unwrap();
        write!(f, "{}", s)
    }
}

impl <T: std::fmt::Write> Colorize<T> for Operand {
    fn colorize(&self, _colors: Option<&ColorSettings>, out: &mut T) -> std::fmt::Result {
        fn signed_hex(num: i16) -> String {
            if num >= 0 {
                format!("+{:#x}", num)
            } else {
                format!("-{:#x}", -num)
            }
        }
        match self {
            Operand::Register(reg) => {
                write!(out, "R{}", reg)
            },
            Operand::Indexed(reg, offset) => {
                write!(out, "{}(R{})", signed_hex(*offset as i16), reg)
            },
            Operand::RegisterIndirect(reg) => {
                write!(out, "@R{}", reg)
            },
            Operand::IndirectAutoinc(reg) => {
                write!(out, "@R{}+", reg)
            },
            Operand::Offset(offset) => {
                write!(out, "${}", signed_hex(*offset as i16))
            },
            Operand::Symbolic(offset) => {
                write!(out, "{}(PC)", signed_hex(*offset as i16))
            },
            Operand::Immediate(imm) => {
                write!(out, "#0x{:x}", imm)
            },
            Operand::Absolute(offset) => {
                write!(out, "&0x{:x}", offset)
            },
            Operand::Const4 => {
                write!(out, "4")
            },
            Operand::Const8 => {
                write!(out, "8")
            },
            Operand::Const0 => {
                write!(out, "0")
            },
            Operand::Const1 => {
                write!(out, "1")
            },
            Operand::Const2 => {
                write!(out, "2")
            },
            Operand::ConstNeg1 => {
                write!(out, "-1")
            },
            Operand::Nothing => {
                write!(out, "<No Operand>")
            }
        }
    }
}
