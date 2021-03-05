use std::convert::TryInto;

#[derive(Clone, Copy)]
pub enum Opcode {
  OpConstant,
  OpAdd,
  OpPop,
  OpSub,
  OpMul,
  OpDiv,
  OpTrue,
  OpFalse,
  OpEqual,
  OpNotEqual,
  OpGreaterThan,
  OpMinus,
  OpBang,
  OpJumpNotTruthy,
  OpJump,
  OpNull,
  OpGetGlobal,
  OpSetGlobal,
  OpArray,
  OpHash,
  OpIndex,
  OpCall,
  OpReturnValue,
  OpReturn,
  OpSetLocal,
  OpGetLocal,
  OpGetBuiltIn
}

#[derive(Debug)]
pub struct Definition {
  name: String,
  operand_widths: Vec<u32>
}

impl Opcode {
  pub fn definition(&self) -> Definition {
    match *self {
      Opcode::OpConstant => Definition {name: "OpConstant".to_string(), operand_widths: vec![2]},
      Opcode::OpAdd => Definition {name: "OpAdd".to_string(), operand_widths: vec![]},
      Opcode::OpPop => Definition {name: "OpPop".to_string(), operand_widths: vec![]},
      Opcode::OpSub => Definition {name: "OpSub".to_string(), operand_widths: vec![]},
      Opcode::OpMul => Definition {name: "OpMul".to_string(), operand_widths: vec![]},
      Opcode::OpDiv => Definition {name: "OpDiv".to_string(), operand_widths: vec![]},
      Opcode::OpTrue => Definition {name: "OpTrue".to_string(), operand_widths: vec![]},
      Opcode::OpFalse => Definition {name: "OpFalse".to_string(), operand_widths: vec![]},
      Opcode::OpEqual => Definition {name: "OpEqual".to_string(), operand_widths: vec![]},
      Opcode::OpNotEqual => Definition {name: "OpNotEqual".to_string(), operand_widths: vec![]},
      Opcode::OpGreaterThan => Definition {name: "OpGreaterThan".to_string(), operand_widths: vec![]},
      Opcode::OpMinus => Definition {name: "OpMinus".to_string(), operand_widths: vec![]},
      Opcode::OpBang => Definition {name: "OpBang".to_string(), operand_widths: vec![]},
      Opcode::OpJumpNotTruthy => Definition {name: "OpJumpNotTruthy".to_string(), operand_widths: vec![2]},
      Opcode::OpJump => Definition {name: "OpJump".to_string(), operand_widths: vec![2]},
      Opcode::OpNull => Definition {name: "OpNull".to_string(), operand_widths: vec![]},
      Opcode::OpGetGlobal => Definition {name: "OpGetGlobal".to_string(), operand_widths: vec![2]},
      Opcode::OpSetGlobal => Definition {name: "OpSetGlobal".to_string(), operand_widths: vec![2]},
      Opcode::OpArray => Definition {name: "OpArray".to_string(), operand_widths: vec![2]},
      Opcode::OpHash => Definition {name: "OpHash".to_string(), operand_widths: vec![2]},
      Opcode::OpIndex => Definition {name: "OpIndex".to_string(), operand_widths: vec![]},
      Opcode::OpCall => Definition {name: "OpCall".to_string(), operand_widths: vec![1]},
      Opcode::OpReturnValue => Definition {name: "OpReturnValue".to_string(), operand_widths: vec![]},
      Opcode::OpReturn => Definition {name: "OpReturn".to_string(), operand_widths: vec![]},
      Opcode::OpSetLocal => Definition {name: "OpSetLocal".to_string(), operand_widths: vec![1]},
      Opcode::OpGetLocal => Definition {name: "OpGetLocal".to_string(), operand_widths: vec![1]},
      Opcode::OpGetBuiltIn => Definition {name: "OpGetBuiltIn".to_string(), operand_widths: vec![1]}

    }
  }
  pub fn lookup(op: u8) -> Opcode {
    match op as u32 {
      0 => Opcode::OpConstant,
      1 => Opcode::OpAdd,
      2 => Opcode::OpPop,
      3 => Opcode::OpSub,
      4 => Opcode::OpMul,
      5 => Opcode::OpDiv,
      6 => Opcode::OpTrue,
      7 => Opcode::OpFalse,
      8 => Opcode::OpEqual,
      9 => Opcode::OpNotEqual,
      10 => Opcode::OpGreaterThan,
      11 => Opcode::OpMinus,
      12 => Opcode::OpBang,
      13 => Opcode::OpJumpNotTruthy,
      14 => Opcode::OpJump,
      15 => Opcode::OpNull,
      16 => Opcode::OpGetGlobal,
      17 => Opcode::OpSetGlobal,
      18 => Opcode::OpArray,
      19 => Opcode::OpHash,
      20 => Opcode::OpIndex,
      21 => Opcode::OpCall,
      22 => Opcode::OpReturnValue,
      23 => Opcode::OpReturn,
      24 => Opcode::OpSetLocal,
      25 => Opcode::OpGetLocal,
      26 => Opcode::OpGetBuiltIn,
      _ => panic!("unknown op code")
    }
  }
}

pub type Instructions = Vec<u8>;

pub trait InstructionsExt {
  fn string(&self) -> String;
}

impl InstructionsExt for Instructions {
  fn string(&self) -> String {
    let mut output = "".to_owned();
    let mut index = 0;
    while index < self.len() {
      let definition = Opcode::lookup(self[index]).definition();
      let (operands, read_bytes) = read_operands(&definition, self[index + 1..].to_vec());
      match definition.operand_widths.len() {
        0 => output += &format!("{:0>4} {}\n", index, definition.name),
        1 => output += &format!("{:0>4} {}\n", index, format_instruction(&definition, operands)).to_string(),
        _ => panic!("invalid operand width")
      }
      index += 1 + read_bytes;
    }
    output
  }
}

fn format_instruction(definition: &Definition, operands: Vec<u32>) -> String {
  let operand_count = operands.len();
  match operand_count {
    1 => format!("{} {}", definition.name, operands[0]),
    _ => panic!("invalid operand count")
  }
}

pub fn make(op: Opcode, operands: &[u32]) -> Instructions {
  let def = op.definition();
  let mut instruction = Vec::new();
  instruction.push(op as u8);
  for (i, operand) in operands.iter().enumerate() {
    let width = def.operand_widths[i];
    match width {
      2 => {
        let operand_as_u16 = *operand as u16;
        instruction.extend_from_slice(&operand_as_u16.to_be_bytes());
      }
      1 => {
        instruction.push(operands[i] as u8)
      }
      _ => {
        panic!("unhandled operand width");
      }
    }
  }
  instruction
}

fn read_operands(def: &Definition, instructions: Instructions) -> (Vec<u32>, usize) {
  let mut operands = Vec::new();
  let mut offset = 0;

  for width in def.operand_widths.iter() {
    match width {
      2 => {
        operands.push(u16::from_be_bytes(instructions[offset..offset + *width as usize].try_into().expect("slice with incorrect length")) as u32)
      },
      1 => {
        operands.push(instructions[offset] as u32)
      }
      _ => {
        panic!("unhandled operand width")
      }
    }
    offset += *width as usize
  }
  (operands, offset)
}

#[cfg(test)]
mod tests {
  use super::*;
  struct TestOp {
    op: Opcode,
    operands: Vec<u32>,
    expected: Vec<u8>
  }
  #[test]
  fn test_make() {
    let mut tests = Vec::new();
    tests.push(TestOp {
      op: Opcode::OpConstant,
      operands: vec![65534],
      expected: vec![Opcode::OpConstant as u8, 255, 254]
    });
    tests.push(TestOp {
      op: Opcode::OpAdd,
      operands: vec![],
      expected: vec![Opcode::OpAdd as u8]
    });
    tests.push(TestOp {
      op: Opcode::OpGetLocal,
      operands: vec![255],
      expected: vec![Opcode::OpGetLocal as u8, 255]
    });
    for test in tests {
      let instruction = make(test.op, &test.operands);
      assert_eq!(instruction.len(), test.expected.len());
      for (i, byte) in test.expected.iter().enumerate() {
        assert_eq!(byte, &instruction[i])
      }
    }
  }
  struct TestRead {
    op: Opcode,
    operands: Vec<u32>,
    bytes_read: usize
  }

  #[test]
  fn test_instruction_string() {
    let mut instructions = Vec::new();
    instructions.append(&mut make(Opcode::OpAdd, &vec![]));
    instructions.append(&mut make(Opcode::OpGetLocal, &vec![1]));
    instructions.append(&mut make(Opcode::OpConstant, &vec![2]));
    instructions.append(&mut make(Opcode::OpConstant, &vec![65535]));

    let expected = "0000 OpAdd
0001 OpGetLocal 1
0003 OpConstant 2
0006 OpConstant 65535
";

    assert_eq!(expected, instructions.string())
  }

  #[test]
  fn test_read_operands() {
    let mut tests = Vec::new();
    tests.push(TestRead {
      op: Opcode::OpConstant,
      operands: vec![65535],
      bytes_read: 2
    });
    tests.push(TestRead {
      op: Opcode::OpGetLocal,
      operands: vec![255],
      bytes_read: 1
    });
    for test in tests.iter() {
      let instructions = make(test.op, &test.operands);
      let (operands_read, offset) = read_operands(&test.op.definition(), instructions[1..].to_vec());
      assert_eq!(offset, test.bytes_read);
      for (index, operand_wanted) in test.operands.iter().enumerate() {
        assert_eq!(*operand_wanted, operands_read[index])
      }
    }
  }
}