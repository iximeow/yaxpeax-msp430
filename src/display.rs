use std;
use std::fmt::{Display, Formatter};

use ::{Operand, Opcode, Instruction, Width};

impl Display for Instruction {
    fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
        write!(f, "{}", self.opcode)?;
        match self.op_width {
            Width::B => { write!(f, ".b")? },
            Width::W => { }
        };
        match self.operands[0] {
            Operand::Nothing => return Ok(()),
            x @ _ => {
                write!(f, " {}", x)?;
            }
        };
        match self.operands[1] {
            Operand::Nothing => return Ok(()),
            x @ _ => {
                write!(f, ", {}", x)?;
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
        fn signed_hex(num: i16) -> String {
            if num >= 0 {
                format!("+{:#x}", num)
            } else {
                format!("-{:#x}", -num)
            }
        }
        match self {
            Operand::Register(reg) => {
                write!(f, "R{}", reg)
            },
            Operand::Indexed(reg, offset) => {
                write!(f, "{}(R{})", signed_hex(*offset as i16), reg)
            },
            Operand::RegisterIndirect(reg) => {
                write!(f, "@R{}", reg)
            },
            Operand::IndirectAutoinc(reg) => {
                write!(f, "@R{}+", reg)
            },
            Operand::Offset(offset) => {
                write!(f, "${}", signed_hex(*offset as i16))
            },
            Operand::Symbolic(offset) => {
                write!(f, "{}(PC)", signed_hex(*offset as i16))
            },
            Operand::Immediate(imm) => {
                write!(f, "#0x{:x}", imm)
            },
            Operand::Absolute(offset) => {
                write!(f, "&0x{:x}", offset)
            },
            Operand::Const4 => {
                write!(f, "4")
            },
            Operand::Const8 => {
                write!(f, "8")
            },
            Operand::Const0 => {
                write!(f, "0")
            },
            Operand::Const1 => {
                write!(f, "1")
            },
            Operand::Const2 => {
                write!(f, "2")
            },
            Operand::ConstNeg1 => {
                write!(f, "-1")
            },
            Operand::Nothing => {
                write!(f, "<No Operand>")
            }
        }
    }
}

