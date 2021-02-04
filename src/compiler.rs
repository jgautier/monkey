use crate::code::Instructions;
use crate::code::Opcode;
use crate::code;
use crate::eval::Object;
use crate::ast::Program;
use crate::ast::Statement;
use crate::ast::BlockStatement;
use crate::ast::Expression;
use std::collections::HashMap;
use std::convert::TryInto;

#[derive(Clone)]
struct EmittedInstruction {
  opcode: Opcode,
  position: u32
}

pub enum SymbolScope {
  Global
}

pub struct Symbol {
  index: usize
}

pub type SymbolTable = HashMap<String, Symbol>;

trait SymbolTableExt {
  fn define(&mut self, name: &str) -> &Symbol;
  fn resolve(&mut self, name: &str) -> Option<&Symbol>;
}

impl SymbolTableExt for SymbolTable {
  fn define(&mut self, name: &str) -> &Symbol {
    let symbol = Symbol{index: self.len()};
    self.insert(name.to_string(), symbol);
    self.get(name).unwrap()
  }
  fn resolve(&mut self, name: &str) -> Option<&Symbol> {
    self.get(name)
  }
}

pub struct Compiler {
  instructions: Instructions,
  pub constants: Vec<Object>,
  pub symbol_table: SymbolTable,
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
      symbol_table: SymbolTable::new(),
      last_instruction: None,
      previous_instruction: None
    }
  }
  pub fn new_with_state(symbol_table: SymbolTable, constants: Vec<Object>) -> Compiler {
    Compiler {
      instructions: Vec::new(),
      constants,
      symbol_table,
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
      Statement::Let{identifier, value} => {
        self.compile_expression(value);
        let index = self.symbol_table.define(identifier).index;
        self.emit(Opcode::OpSetGlobal, vec![index as u32]);
      }
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
      Expression::StringLiteral(val) => {
        let constant = &self.add_constant(Object::String(val.to_string()));
        self.emit(Opcode::OpConstant, vec![*constant]);
      }
      Expression::Boolean(val) => {
        if *val {
          self.emit(Opcode::OpTrue, vec![]);
        } else {
          self.emit(Opcode::OpFalse, vec![]);
        }
      }
      Expression::ArrayLiteral(expressions) => {
        for expr in expressions {
          self.compile_expression(expr)
        }
        self.emit(Opcode::OpArray, vec![expressions.len() as u32]);
      }
      Expression::HashLiteral(hash) => {
        for pair in &hash.pairs {
          self.compile_expression(pair.0);
          self.compile_expression(pair.1);
        }
        self.emit(Opcode::OpHash, vec![(hash.pairs.len() * 2) as u32]);
      }
      Expression::Index{left, index} => {
        self.compile_expression(left);
        self.compile_expression(index);
        self.emit(Opcode::OpIndex, vec![]);
      }
      Expression::Identifier(id) => {
        let index = if let Some(sym) = self.symbol_table.get(id) {
          sym.index
        } else {
          panic!("undefined variable {}", id);
        };
        self.emit(Opcode::OpGetGlobal, vec![index as u32]);
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
    expected_constants: Vec<Object>,
    expected_instructions: Vec<Instructions>
  }
  #[test]
  fn test_integer_arithmetic() {
    let test = vec![
      CompilerTestCase {
        input: "1 + 2".to_string(),
        expected_constants: vec![Object::Integer(1), Object::Integer(2)],
        expected_instructions: vec![
          code::make(Opcode::OpConstant, &vec![0]),
          code::make(Opcode::OpConstant, &vec![1]),
          code::make(Opcode::OpAdd, &vec![]),
          code::make(Opcode::OpPop, &vec![])
        ]
      },
      CompilerTestCase {
        input: "1; 2".to_string(),
        expected_constants: vec![Object::Integer(1), Object::Integer(2)],
        expected_instructions: vec![
          code::make(Opcode::OpConstant, &vec![0]),
          code::make(Opcode::OpPop, &vec![]),
          code::make(Opcode::OpConstant, &vec![1]),
          code::make(Opcode::OpPop, &vec![])
        ]
      },
      CompilerTestCase {
        input: "1 - 2".to_string(),
        expected_constants: vec![Object::Integer(1), Object::Integer(2)],
        expected_instructions: vec![
          code::make(Opcode::OpConstant, &vec![0]),
          code::make(Opcode::OpConstant, &vec![1]),
          code::make(Opcode::OpSub, &vec![]),
          code::make(Opcode::OpPop, &vec![])
        ]
      },
      CompilerTestCase {
        input: "1 * 2".to_string(),
        expected_constants: vec![Object::Integer(1), Object::Integer(2)],
        expected_instructions: vec![
          code::make(Opcode::OpConstant, &vec![0]),
          code::make(Opcode::OpConstant, &vec![1]),
          code::make(Opcode::OpMul, &vec![]),
          code::make(Opcode::OpPop, &vec![])
        ]
      },
      CompilerTestCase {
        input: "2 / 1".to_string(),
        expected_constants: vec![Object::Integer(2), Object::Integer(1)],
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
        expected_constants: vec![Object::Integer(1), Object::Integer(2)],
        expected_instructions: vec![
          code::make(Opcode::OpConstant, &vec![0]),
          code::make(Opcode::OpConstant, &vec![1]),
          code::make(Opcode::OpGreaterThan, &vec![]),
          code::make(Opcode::OpPop, &vec![])
        ]
      },
      CompilerTestCase {
        input: "1 < 2".to_string(),
        expected_constants: vec![Object::Integer(2), Object::Integer(1)],
        expected_instructions: vec![
          code::make(Opcode::OpConstant, &vec![0]),
          code::make(Opcode::OpConstant, &vec![1]),
          code::make(Opcode::OpGreaterThan, &vec![]),
          code::make(Opcode::OpPop, &vec![])
        ]
      },
      CompilerTestCase {
        input: "1 == 2".to_string(),
        expected_constants: vec![Object::Integer(1), Object::Integer(2)],
        expected_instructions: vec![
          code::make(Opcode::OpConstant, &vec![0]),
          code::make(Opcode::OpConstant, &vec![1]),
          code::make(Opcode::OpEqual, &vec![]),
          code::make(Opcode::OpPop, &vec![])
        ]
      },
      CompilerTestCase {
        input: "1 != 2".to_string(),
        expected_constants: vec![Object::Integer(1), Object::Integer(2)],
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
        expected_constants: vec![Object::Integer(1)],
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
        expected_constants: vec![Object::Integer(10), Object::Integer(3333)],
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
        expected_constants: vec![Object::Integer(10), Object::Integer(20), Object::Integer(3333)],
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
      },
      CompilerTestCase {
        input: "
          let one = 1;
          let two = 2;
        ".to_string(),
        expected_constants: vec![Object::Integer(1), Object::Integer(2)],
        expected_instructions: vec![
          code::make(Opcode::OpConstant, &vec![0]),
          code::make(Opcode::OpSetGlobal, &vec![0]),
          code::make(Opcode::OpConstant, &vec![1]),
          code::make(Opcode::OpSetGlobal, &vec![1])
        ]
      },
      CompilerTestCase {
        input: "
          let one = 1;
          one;
          ".to_string(),
        expected_constants: vec![Object::Integer(1)],
        expected_instructions: vec![
          code::make(Opcode::OpConstant, &vec![0]),
          code::make(Opcode::OpSetGlobal, &vec![0]),
          code::make(Opcode::OpGetGlobal, &vec![0]),
          code::make(Opcode::OpPop, &vec![])
        ]
      },
      CompilerTestCase {
        input: "
          let one = 1;
          let two = one;
          two;
          ".to_string(),
        expected_constants: vec![Object::Integer(1)],
        expected_instructions: vec![
          code::make(Opcode::OpConstant, &vec![0]),
          code::make(Opcode::OpSetGlobal, &vec![0]),
          code::make(Opcode::OpGetGlobal, &vec![0]),
          code::make(Opcode::OpSetGlobal, &vec![1]),
          code::make(Opcode::OpGetGlobal, &vec![1]),
          code::make(Opcode::OpPop, &vec![])
        ]
      },
      CompilerTestCase {
        input: "\"monkey\"".to_string(),
        expected_constants: vec![Object::String("monkey".to_string())],
        expected_instructions: vec![
          code::make(Opcode::OpConstant, &vec![0]),
          code::make(Opcode::OpPop, &vec![])
        ]
      },
      CompilerTestCase {
        input: "\"mon\" + \"key\"".to_string(),
        expected_constants: vec![Object::String("mon".to_string()), Object::String("key".to_string())],
        expected_instructions: vec![
          code::make(Opcode::OpConstant, &vec![0]),
          code::make(Opcode::OpConstant, &vec![1]),
          code::make(Opcode::OpAdd, &vec![]),
          code::make(Opcode::OpPop, &vec![])
        ]
      },
      CompilerTestCase {
        input: "[]".to_string(),
        expected_constants: vec![],
        expected_instructions: vec![
          code::make(Opcode::OpArray, &vec![0]),
          code::make(Opcode::OpPop, &vec![])
        ]
      },
      CompilerTestCase {
        input: "[1, 2, 3]".to_string(),
        expected_constants: vec![Object::Integer(1), Object::Integer(2), Object::Integer(3)],
        expected_instructions: vec![
          code::make(Opcode::OpConstant, &vec![0]),
          code::make(Opcode::OpConstant, &vec![1]),
          code::make(Opcode::OpConstant, &vec![2]),
          code::make(Opcode::OpArray, &vec![3]),
          code::make(Opcode::OpPop, &vec![])
        ]
      },
      CompilerTestCase {
        input: "[1 + 2, 3 - 4, 5 * 6]".to_string(),
        expected_constants: vec![Object::Integer(1), Object::Integer(2), Object::Integer(3), Object::Integer(4), Object::Integer(5), Object::Integer(6)],
        expected_instructions: vec![
          code::make(Opcode::OpConstant, &vec![0]),
          code::make(Opcode::OpConstant, &vec![1]),
          code::make(Opcode::OpAdd, &vec![]),
          code::make(Opcode::OpConstant, &vec![2]),
          code::make(Opcode::OpConstant, &vec![3]),
          code::make(Opcode::OpSub, &vec![]),
          code::make(Opcode::OpConstant, &vec![4]),
          code::make(Opcode::OpConstant, &vec![5]),
          code::make(Opcode::OpMul, &vec![]),
          code::make(Opcode::OpArray, &vec![3]),
          code::make(Opcode::OpPop, &vec![])
        ]
      },
      CompilerTestCase {
        input: "{}".to_string(),
        expected_constants: vec![],
        expected_instructions: vec![
          code::make(Opcode::OpHash, &vec![0]),
          code::make(Opcode::OpPop, &vec![])
        ]
      },
      CompilerTestCase {
        input: "{1: 2, 3: 4, 5: 6}".to_string(),
        expected_constants: vec![Object::Integer(1), Object::Integer(2), Object::Integer(3), Object::Integer(4), Object::Integer(5), Object::Integer(6)],
        expected_instructions: vec![
          code::make(Opcode::OpConstant, &vec![0]),
          code::make(Opcode::OpConstant, &vec![1]),
          code::make(Opcode::OpConstant, &vec![2]),
          code::make(Opcode::OpConstant, &vec![3]),
          code::make(Opcode::OpConstant, &vec![4]),
          code::make(Opcode::OpConstant, &vec![5]),
          code::make(Opcode::OpHash, &vec![6]),
          code::make(Opcode::OpPop, &vec![])
        ]
      },
      CompilerTestCase {
        input: "{1: 2 + 3, 4: 5 * 6}".to_string(),
        expected_constants: vec![Object::Integer(1), Object::Integer(2), Object::Integer(3), Object::Integer(4), Object::Integer(5), Object::Integer(6)],
        expected_instructions: vec![
          code::make(Opcode::OpConstant, &vec![0]),
          code::make(Opcode::OpConstant, &vec![1]),
          code::make(Opcode::OpConstant, &vec![2]),
          code::make(Opcode::OpAdd, &vec![]),
          code::make(Opcode::OpConstant, &vec![3]),
          code::make(Opcode::OpConstant, &vec![4]),
          code::make(Opcode::OpConstant, &vec![5]),
          code::make(Opcode::OpMul, &vec![]),
          code::make(Opcode::OpHash, &vec![4]),
          code::make(Opcode::OpPop, &vec![])
        ]
      },
      CompilerTestCase {
        input: "[1, 2, 3][1 + 1]".to_string(),
        expected_constants: vec![Object::Integer(1), Object::Integer(2), Object::Integer(3), Object::Integer(1), Object::Integer(1)],
        expected_instructions: vec![
          code::make(Opcode::OpConstant, &vec![0]),
          code::make(Opcode::OpConstant, &vec![1]),
          code::make(Opcode::OpConstant, &vec![2]),
          code::make(Opcode::OpArray, &vec![3]),
          code::make(Opcode::OpConstant, &vec![3]),
          code::make(Opcode::OpConstant, &vec![4]),
          code::make(Opcode::OpAdd, &vec![]),
          code::make(Opcode::OpIndex, &vec![]),
          code::make(Opcode::OpPop, &vec![])
        ]
      },
      CompilerTestCase {
        input: "{1: 2}[2 - 1]".to_string(),
        expected_constants: vec![Object::Integer(1), Object::Integer(2), Object::Integer(2), Object::Integer(1)],
        expected_instructions: vec![
          code::make(Opcode::OpConstant, &vec![0]),
          code::make(Opcode::OpConstant, &vec![1]),
          code::make(Opcode::OpHash, &vec![2]),
          code::make(Opcode::OpConstant, &vec![2]),
          code::make(Opcode::OpConstant, &vec![3]),
          code::make(Opcode::OpSub, &vec![]),
          code::make(Opcode::OpIndex, &vec![]),
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
        match (&bytecode.constants[i], constant) {
          (Object::Integer(val), Object::Integer(val2)) => assert_eq!(val, val2),
          (Object::String(str1), Object::String(str2)) => assert_eq!(str1, str2),
          _ => panic!("unhandle constant")
        }
      }
    }
  }
}