use std::ptr::NonNull;

use crate::region::Header;

pub enum Instruction {
    /// Load and yield current position as a header pointer,
    /// then advance the instr pointer by 1.
    Field,
    /// Load current position as a usize variant index,
    /// advance the instr pointer by (index + 1).
    Variant,
    /// Scanner ended.
    End,
    /// Advance the cursor by the given number of bytes.
    Advance(u32),
}

const END_TAG: u32 = 0b00;
const VARIANT_TAG: u32 = 0b01;
const FIELD_TAG: u32 = 0b10;
const ADVANCE_TAG: u32 = 0b11;

#[repr(C)]
pub struct PackedInstr(u32);

impl From<PackedInstr> for Instruction {
    fn from(value: PackedInstr) -> Self {
        match value.0 & 0b11 {
            END_TAG => Instruction::End,
            VARIANT_TAG => Instruction::Variant,
            FIELD_TAG => Instruction::Field,
            ADVANCE_TAG => Instruction::Advance(value.0 >> 2),
            _ => unreachable!(),
        }
    }
}

impl From<Instruction> for PackedInstr {
    fn from(value: Instruction) -> Self {
        value.pack()
    }
}

impl Instruction {
    pub const fn pack(self) -> PackedInstr {
        match self {
            Instruction::End => PackedInstr(END_TAG),
            Instruction::Variant => PackedInstr(VARIANT_TAG),
            Instruction::Field => PackedInstr(FIELD_TAG),
            Instruction::Advance(bytes) => PackedInstr((bytes << 2) | ADVANCE_TAG),
        }
    }
}

pub struct Scanner {
    instr: NonNull<PackedInstr>,
    cursor: NonNull<u8>,
}

impl Scanner {
    fn next(&mut self) -> Option<NonNull<Header>> {
        unsafe {
            loop {
                let instr: Instruction = self.instr.read().into();
                match instr {
                    Instruction::End => return None,
                    Instruction::Field => {
                        let header_ptr = self.cursor.cast::<*mut Header>();
                        self.instr = self.instr.add(1);
                        if let Some(child) = NonNull::new(header_ptr.read()) {
                            return Some(child);
                        }
                    }
                    Instruction::Variant => {
                        let index = self.cursor.cast::<usize>().read();
                        self.instr = self.instr.add(index + 1);
                    }
                    Instruction::Advance(bytes) => {
                        self.cursor = self.cursor.byte_add(bytes as usize);
                        self.instr = self.instr.add(1);
                    }
                }
            }
        }
    }
}

impl Iterator for Scanner {
    type Item = NonNull<Header>;
    fn next(&mut self) -> Option<Self::Item> {
        self.next()
    }
}

impl Scanner {
    pub unsafe fn new(object: NonNull<Header>) -> Option<Self> {
        let vtable = unsafe { &*object.as_ref().vtable };
        let instr = NonNull::new(vtable.scan_instrs as *mut PackedInstr)?;
        let cursor = unsafe { Header::get_object_ptr(object) };
        Some(Self { instr, cursor })
    }
}
