use crate::code::Instructions;
use crate::code::Opcode;
use crate::code;
use crate::eval::Object;
use crate::ast::Program;
use crate::ast::Statement;
use crate::ast::BlockStatement;
use crate::ast::Expression;
use std::convert::TryInto;

#[derive(Clone)]
struct EmittedInstruction {
  opcode: Opcode,
  position: u32
}

pub struct Compiler {
  instructions: Instructions,
  constants: Vec<Object>,
  last_instruction: Option<EmittedInstruction>,
  previous_instruction: Option<EmittedInstruction>
}
impl Default for Compiler {
  fn default() -> Self {
    Self::new()
  }
}

impl Compiler {
  pub fn new() -> Compiler {
    Compiler {
      instructions: Vec::new(),
      constants: Vec::new(),
      last_instruction: None,
      previous_instruction: None
    }
  }
  pub fn compile(&mut self, program: &Program) {
    for statement in program.statements.iter() {
      self.compile_statement(statement);
    }
  }
  fn compile_statement(&mut self, statement: &Statement) {
    match statement {
      Statement::Expression(expr) => {
        self.compile_expression(expr);
        self.emit(Opcode::OpPop, vec![]);
      },
      _ => panic!("unhandled statement type")
    };
  }
  fn compile_blockstatement(&mut self, block_statement: &BlockStatement) {
    for s in &block_statement.statements {
      self.compile_statement(s);
    }
  }
  fn compile_expression(&mut self, expression: &Expression) {
    match expression {
      Expression::If{condition, consequence, alternative} => {
        self.compile_expression(condition);
        // emit op with bogus value and will update later
        let jump_not_truthy_pos = self.emit(Opcode::OpJumpNotTruthy, vec![9999]);
        self.compile_blockstatement(consequence);
        if let Some(instruction) = &self.last_instruction {
          if let Opcode::OpPop = instruction.opcode {
            self.instructions.pop();
            self.last_instruction = self.previous_instruction.clone();
          }
        }
        let jump_pos = self.emit(Opcode::OpJump, vec![9999]);
        self.change_operand(jump_not_truthy_pos, self.instructions.len() as u32);
        if let Some(alt) = alternative {
          self.compile_blockstatement(alt);
        } else {
          self.emit(Opcode::OpNull, vec![]);
        }
        if let Some(instruction) = &self.last_instruction {
          if let Opcode::OpPop = instruction.opcode {
            self.instructions.pop();
            self.last_instruction = self.previous_instruction.clone();
          }
          self.change_operand(jump_pos, self.instructions.len() as u32)
        }
      }
      Expression::Infix{operator, left, right} => {
        let op = if operator == "<" {
          self.compile_expression(right);
          self.compile_expression(left);
          ">"
        } else {
          self.compile_expression(left);
          self.compile_expression(right);
          operator.as_str()
        };

        match op {
          "+" => self.emit(Opcode::OpAdd, vec![]),
          "-" => self.emit(Opcode::OpSub, vec![]),
          "*" => self.emit(Opcode::OpMul, vec![]),
          "/" => self.emit(Opcode::OpDiv, vec![]),
          ">" => self.emit(Opcode::OpGreaterThan, vec![]),
          "==" => self.emit(Opcode::OpEqual, vec![]),
          "!=" => self.emit(Opcode::OpNotEqual, vec![]),
          _ => panic!("invalid infix operator")
        };
      }
      Expression::Prefix{operator, right} => {
        self.compile_expression(right);
        match operator.as_str() {
          "-" => self.emit(Opcode::OpMinus, vec![]),
          "!" => self.emit(Opcode::OpBang, vec![]),
          _ => panic!("invalid prefix operator")
        };
      }
      Expression::IntegerLiteral(val) => {
        let constant = &self.add_constant(Object::Integer(*val));
        self.emit(Opcode::OpConstant, vec![*constant]);
      }
      Expression::Boolean(val) => {
        if *val {
          self.emit(Opcode::OpTrue, vec![]);
        } else {
          self.emit(Opcode::OpFalse, vec![]);
        }
      }
      _ => panic!("unhandled expression")
    };
  }

  fn replace_instruction(&mut self, position: usize, new_instruction: Vec<u8>) {
    for (i, byte) in new_instruction.iter().enumerate() {
      self.instructions[i + position] = *byte
    }
  }

  fn change_operand(&mut self, position: usize, operand: u32) {
    let op_code = Opcode::lookup(self.instructions[position]);
    let new_instruction = code::make(op_code, &[operand]);
    self.replace_instruction(position, new_instruction);
  }

  fn add_constant(&mut self, obj: Object) -> u32 {
    self.constants.push(obj);
    let position: u32 = (&self.constants.len() - 1).try_into().unwrap();
    position
  }

  fn emit(&mut self, op: Opcode, operands: Vec<u32>) -> usize {
    let mut instruction = code::make(op, &operands);
    let pos = self.add_instruction(&mut instruction);
    let previous = self.last_instruction.clone();
    let last = EmittedInstruction{
      opcode: op,
      position: pos as u32
    };
    self.previous_instruction = previous;
    self.last_instruction = Some(last);
    pos
  }

  fn add_instruction(&mut self, instruction: &mut Vec<u8>) -> usize {
    let position = &self.instructions.len();
    self.instructions.append(instruction);
    *position
  }

  pub fn bytecode(&self) -> Bytecode {
    Bytecode {
      instructions: self.instructions.clone(),
      constants: self.constants.clone()
    }
  }
}

pub struct Bytecode {
  pub instructions: Instructions,
  pub constants: Vec<Object>
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::lexer;
  use crate::ast;
  use crate::code::InstructionsExt;
  struct CompilerTestCase {
    input: String,
    expected_constants: Vec<u32>,
    expected_instructions: Vec<Instructions>
  }
  #[test]
  fn test_integer_arithmetic() {
    let test = vec![
      CompilerTestCase {
        input: "1 + 2".to_string(),
        expected_constants: vec![1, 2],
        expected_instructions: vec![
          code::make(Opcode::OpConstant, &vec![0]),
          code::make(Opcode::OpConstant, &vec![1]),
          code::make(Opcode::OpAdd, &vec![]),
          code::make(Opcode::OpPop, &vec![])
        ]
      },
      CompilerTestCase {
        input: "1; 2".to_string(),
        expected_constants: vec![1, 2],
        expected_instructions: vec![
          code::make(Opcode::OpConstant, &vec![0]),
          code::make(Opcode::OpPop, &vec![]),
          code::make(Opcode::OpConstant, &vec![1]),
          code::make(Opcode::OpPop, &vec![])
        ]
      },
      CompilerTestCase {
        input: "1 - 2".to_string(),
        expected_constants: vec![1, 2],
        expected_instructions: vec![
          code::make(Opcode::OpConstant, &vec![0]),
          code::make(Opcode::OpConstant, &vec![1]),
          code::make(Opcode::OpSub, &vec![]),
          code::make(Opcode::OpPop, &vec![])
        ]
      },
      CompilerTestCase {
        input: "1 * 2".to_string(),
        expected_constants: vec![1, 2],
        expected_instructions: vec![
          code::make(Opcode::OpConstant, &vec![0]),
          code::make(Opcode::OpConstant, &vec![1]),
          code::make(Opcode::OpMul, &vec![]),
          code::make(Opcode::OpPop, &vec![])
        ]
      },
      CompilerTestCase {
        input: "2 / 1".to_string(),
        expected_constants: vec![2, 1],
        expected_instructions: vec![
          code::make(Opcode::OpConstant, &vec![0]),
          code::make(Opcode::OpConstant, &vec![1]),
          code::make(Opcode::OpDiv, &vec![]),
          code::make(Opcode::OpPop, &vec![])
        ]
      },
      CompilerTestCase {
        input: "true".to_string(),
        expected_constants: vec![],
        expected_instructions: vec![
          code::make(Opcode::OpTrue, &vec![]),
          code::make(Opcode::OpPop, &vec![])
        ]
      },
      CompilerTestCase {
        input: "false".to_string(),
        expected_constants: vec![],
        expected_instructions: vec![
          code::make(Opcode::OpFalse, &vec![]),
          code::make(Opcode::OpPop, &vec![])
        ]
      },
      CompilerTestCase {
        input: "1 > 2".to_string(),
        expected_constants: vec![1, 2],
        expected_instructions: vec![
          code::make(Opcode::OpConstant, &vec![0]),
          code::make(Opcode::OpConstant, &vec![1]),
          code::make(Opcode::OpGreaterThan, &vec![]),
          code::make(Opcode::OpPop, &vec![])
        ]
      },
      CompilerTestCase {
        input: "1 < 2".to_string(),
        expected_constants: vec![2, 1],
        expected_instructions: vec![
          code::make(Opcode::OpConstant, &vec![0]),
          code::make(Opcode::OpConstant, &vec![1]),
          code::make(Opcode::OpGreaterThan, &vec![]),
          code::make(Opcode::OpPop, &vec![])
        ]
      },
      CompilerTestCase {
        input: "1 == 2".to_string(),
        expected_constants: vec![1, 2],
        expected_instructions: vec![
          code::make(Opcode::OpConstant, &vec![0]),
          code::make(Opcode::OpConstant, &vec![1]),
          code::make(Opcode::OpEqual, &vec![]),
          code::make(Opcode::OpPop, &vec![])
        ]
      },
      CompilerTestCase {
        input: "1 != 2".to_string(),
        expected_constants: vec![1, 2],
        expected_instructions: vec![
          code::make(Opcode::OpConstant, &vec![0]),
          code::make(Opcode::OpConstant, &vec![1]),
          code::make(Opcode::OpNotEqual, &vec![]),
          code::make(Opcode::OpPop, &vec![])
        ]
      },
      CompilerTestCase {
        input: "true == false".to_string(),
        expected_constants: vec![],
        expected_instructions: vec![
          code::make(Opcode::OpTrue, &vec![]),
          code::make(Opcode::OpFalse, &vec![]),
          code::make(Opcode::OpEqual, &vec![]),
          code::make(Opcode::OpPop, &vec![])
        ]
      },
      CompilerTestCase {
        input: "true != false".to_string(),
        expected_constants: vec![],
        expected_instructions: vec![
          code::make(Opcode::OpTrue, &vec![]),
          code::make(Opcode::OpFalse, &vec![]),
          code::make(Opcode::OpNotEqual, &vec![]),
          code::make(Opcode::OpPop, &vec![])
        ]
      },
      CompilerTestCase {
        input: "-1".to_string(),
        expected_constants: vec![1],
        expected_instructions: vec![
          code::make(Opcode::OpConstant, &vec![0]),
          code::make(Opcode::OpMinus, &vec![]),
          code::make(Opcode::OpPop, &vec![])
        ]
      },
      CompilerTestCase {
        input: "!true".to_string(),
        expected_constants: vec![],
        expected_instructions: vec![
          code::make(Opcode::OpTrue, &vec![]),
          code::make(Opcode::OpBang, &vec![]),
          code::make(Opcode::OpPop, &vec![])
        ]
      },
      CompilerTestCase {
        input: "if (true) { 10 }; 3333;".to_string(),
        expected_constants: vec![10, 3333],
        expected_instructions: vec![
          code::make(Opcode::OpTrue, &vec![]),
          code::make(Opcode::OpJumpNotTruthy, &vec![10]),
          code::make(Opcode::OpConstant, &vec![0]),
          code::make(Opcode::OpJump, &vec![11]),
          code::make(Opcode::OpNull, &vec![]),
          code::make(Opcode::OpPop, &vec![]),
          code::make(Opcode::OpConstant, &vec![1]),
          code::make(Opcode::OpPop, &vec![])
        ]
      },
      CompilerTestCase {
        input: "if (true) { 10 } else { 20 }; 3333;".to_string(),
        expected_constants: vec![10, 20, 3333],
        expected_instructions: vec![
          code::make(Opcode::OpTrue, &vec![]),
          code::make(Opcode::OpJumpNotTruthy, &vec![10]),
          code::make(Opcode::OpConstant, &vec![0]),
          code::make(Opcode::OpJump, &vec![13]),
          code::make(Opcode::OpConstant, &vec![1]),
          code::make(Opcode::OpPop, &vec![]),
          code::make(Opcode::OpConstant, &vec![2]),
          code::make(Opcode::OpPop, &vec![])
        ]
      }
    ];
    run_tests(test)
  }
  fn parse(program: &str) -> Program {
    let lexer = lexer::Lexer::new(program);
    ast::Parser::new(lexer).parse()
  }

  fn run_tests(tests: Vec<CompilerTestCase>) {
    for test in tests.iter() {
      let program = parse(&test.input);
      let mut compiler = Compiler::new();
      compiler.compile(&program);
      let bytecode = compiler.bytecode();
      assert_eq!(bytecode.instructions.string(), test.expected_instructions.clone().into_iter().flatten().collect::<Instructions>().string());
      for (i, expected_instruction) in test.expected_instructions.iter().flatten().enumerate() {
        assert_eq!(bytecode.instructions[i], *expected_instruction);
      }
      for (i, constant) in test.expected_constants.iter().enumerate() {
        match bytecode.constants[i] {
          Object::Integer(val) => assert_eq!(val, (*constant) as i64),
          _ => panic!("unhandle constant")
        }
      }
    }
  }
}