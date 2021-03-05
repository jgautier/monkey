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

#[derive(Debug, PartialEq, Clone)]
pub enum SymbolScope {
  Global,
  Local
}

#[derive(Clone)]
pub struct Symbol {
  index: usize,
  scope: SymbolScope
}

#[derive(Clone)]
pub struct SymbolTable {
  store: HashMap<String, Symbol>,
  outer: Option<Box<SymbolTable>>
}

impl Default for SymbolTable {
  fn default() -> Self {
    Self::new()
  }
}
impl SymbolTable {
  pub fn new() -> SymbolTable {
    SymbolTable {
      store: HashMap::new(),
      outer: None
    }
  }
  fn new_enclosed(outer: SymbolTable) -> SymbolTable {
    SymbolTable {
      store: HashMap::new(),
      outer: Some(Box::new(outer))
    }
  }
  fn define(&mut self, name: &str) -> &Symbol {
    let symbol = if self.outer.is_some() {
      Symbol{index: self.store.len(), scope: SymbolScope::Local}
    } else {
      Symbol{index: self.store.len(), scope: SymbolScope::Global}
    };
    self.store.insert(name.to_string(), symbol);
    self.store.get(name).unwrap()
  }
  fn resolve(&self, name: &str) -> Option<&Symbol> {
    if let Some(symbol) = self.store.get(name) {
      Some(symbol)
    } else if let Some(outer) = &self.outer {
      outer.resolve(name)
    } else {
      None
    }
  }
}

struct CompilationScope {
  instructions: Instructions,
  last_instruction: Option<EmittedInstruction>,
  previous_instruction: Option<EmittedInstruction>
}

pub struct Compiler {
  pub constants: Vec<Object>,
  pub symbol_table: SymbolTable,
  scopes: Vec<CompilationScope>,
  scope_index: usize
}
impl Default for Compiler {
  fn default() -> Self {
    Self::new()
  }
}

impl Compiler {
  pub fn new() -> Compiler {
    Compiler {
      scopes: vec![
        CompilationScope {
          instructions: Vec::new(),
          last_instruction: None,
          previous_instruction: None
        }
      ],
      constants: Vec::new(),
      symbol_table: SymbolTable::default(),
      scope_index: 0
    }
  }
  pub fn new_with_state(symbol_table: SymbolTable, constants: Vec<Object>) -> Compiler {
    Compiler {
      scopes: vec![
        CompilationScope {
          instructions: Vec::new(),
          last_instruction: None,
          previous_instruction: None
        }
      ],
      constants,
      symbol_table,
      scope_index: 0
    }
  }
  pub fn current_instructions(&mut self) -> &mut Instructions {
    &mut self.scopes[self.scope_index].instructions
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
        let symbol = self.symbol_table.define(identifier);
        let index = symbol.index;
        let scope = &symbol.scope;
        if let SymbolScope::Global = scope {
          self.emit(Opcode::OpSetGlobal, vec![index as u32]);
        } else {
          self.emit(Opcode::OpSetLocal, vec![index as u32]);
        }
      },
      Statement::Return(expr) => {
        self.compile_expression(expr);
        self.emit(Opcode::OpReturnValue, vec![]);
      }
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

        if let Some(instruction) = &self.scopes[self.scope_index].last_instruction {
          if let Opcode::OpPop = instruction.opcode {
            self.current_instructions().pop();
            self.scopes[self.scope_index].last_instruction = self.scopes[self.scope_index].previous_instruction.clone();
          }
        }
        let jump_pos = self.emit(Opcode::OpJump, vec![9999]);
        let current_instructions_len = self.current_instructions().len();
        self.change_operand(jump_not_truthy_pos, current_instructions_len as u32);
        if let Some(alt) = alternative {
          self.compile_blockstatement(alt);
        } else {
          self.emit(Opcode::OpNull, vec![]);
        }
        if let Some(instruction) = &self.scopes[self.scope_index].last_instruction {
          if let Opcode::OpPop = instruction.opcode {
            self.current_instructions().pop();
            self.scopes[self.scope_index].last_instruction = self.scopes[self.scope_index].previous_instruction.clone();
          }
          let current_instructions_len = self.current_instructions().len();
          self.change_operand(jump_pos, current_instructions_len as u32)
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
        let symbol = if let Some(sym) = self.symbol_table.resolve(id) {
          sym
        } else {
          panic!("undefined variable {}", id);
        };
        let index = symbol.index;
        let scope = &symbol.scope;
        if let SymbolScope::Global = scope {
          self.emit(Opcode::OpGetGlobal, vec![index as u32]);
        } else {
          self.emit(Opcode::OpGetLocal, vec![index as u32]);
        }
      }
      Expression::Fn{params, body} => {
        self.enter_scope();
        for param in params {
          self.symbol_table.define(param);
        }
        self.compile_blockstatement(body);
        let last_instruction = self.scopes[self.scope_index].last_instruction.clone();
        if let Some(instruction) = last_instruction {
          if let Opcode::OpPop = instruction.opcode {
            let return_value = code::make(Opcode::OpReturnValue, &[]);
            self.replace_instruction(instruction.position as usize, return_value);
            self.scopes[self.scope_index].last_instruction = Some(EmittedInstruction {
              position: instruction.position,
              opcode: Opcode::OpReturnValue
            });
          }
        } else {
          self.emit(Opcode::OpReturn, vec![]);
        }
        let num_locals = self.symbol_table.store.len();
        let instructions = self.leave_scope();
        let compiled_fn = Object::CompiledFunction(instructions, num_locals, params.len());
        let constant_index = self.add_constant(compiled_fn);
        self.emit(Opcode::OpConstant, vec![constant_index]);
      },
      Expression::Call{function, args} => {
        self.compile_expression(function);
        for arg in args {
          self.compile_expression(arg);
        }
        self.emit(Opcode::OpCall, vec![args.len() as u32]);
      }
    };
  }

  fn replace_instruction(&mut self, position: usize, new_instruction: Vec<u8>) {
    let instructions = self.current_instructions();
    for (i, byte) in new_instruction.iter().enumerate() {
      instructions[i + position] = *byte
    }
  }

  fn change_operand(&mut self, position: usize, operand: u32) {
    let op_code = Opcode::lookup(self.current_instructions()[position]);
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
    let previous = self.scopes[self.scope_index].last_instruction.clone();
    let last = EmittedInstruction{
      opcode: op,
      position: pos as u32
    };
    self.scopes[self.scope_index].previous_instruction = previous;
    self.scopes[self.scope_index].last_instruction = Some(last);
    pos
  }

  fn add_instruction(&mut self, instruction: &mut Vec<u8>) -> usize {
    let instructions = self.current_instructions();
    let position = instructions.len();
    instructions.append(instruction);
    position
  }

  fn enter_scope(&mut self) {
    let scope = CompilationScope {
      instructions: Vec::new(),
      last_instruction: None,
      previous_instruction: None
    };
    self.scopes.push(scope);
    self.scope_index += 1;
    let table = self.symbol_table.clone();
    self.symbol_table = SymbolTable::new_enclosed(table);
  }

  fn leave_scope(&mut self) -> Instructions {
    let insructions = self.current_instructions().clone();
    self.scopes.pop();
    self.scope_index -= 1;
    let table = self.symbol_table.outer.take().unwrap();
    self.symbol_table = *table;
    insructions
  }

  pub fn bytecode(&mut self) -> Bytecode {
    Bytecode {
      instructions: self.current_instructions().clone(),
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
      },
      CompilerTestCase {
        input: "fn () { return 5 + 10 }".to_string(),
        expected_constants: vec![
          Object::Integer(5),
          Object::Integer(10),
          Object::CompiledFunction(vec![
            code::make(Opcode::OpConstant, &vec![0]),
            code::make(Opcode::OpConstant, &vec![1]),
            code::make(Opcode::OpAdd, &vec![]),
            code::make(Opcode::OpReturnValue, &vec![])
          ].into_iter().flatten().collect(), 0, 0)
        ],
        expected_instructions: vec![
            code::make(Opcode::OpConstant, &vec![2]),
            code::make(Opcode::OpPop, &vec![])
        ]
      },
      CompilerTestCase {
        input: "fn () { 5 + 10 }".to_string(),
        expected_constants: vec![
          Object::Integer(5),
          Object::Integer(10),
          Object::CompiledFunction(vec![
            code::make(Opcode::OpConstant, &vec![0]),
            code::make(Opcode::OpConstant, &vec![1]),
            code::make(Opcode::OpAdd, &vec![]),
            code::make(Opcode::OpReturnValue, &vec![])
          ].into_iter().flatten().collect(), 0, 0)
        ],
        expected_instructions: vec![
            code::make(Opcode::OpConstant, &vec![2]),
            code::make(Opcode::OpPop, &vec![])
        ]
      },
      CompilerTestCase {
        input: "fn () {}".to_string(),
        expected_constants: vec![
          Object::CompiledFunction(vec![
            code::make(Opcode::OpReturn, &vec![])
          ].into_iter().flatten().collect(), 0, 0)
        ],
        expected_instructions: vec![
            code::make(Opcode::OpConstant, &vec![0]),
            code::make(Opcode::OpPop, &vec![])
        ]
      },
      CompilerTestCase {
        input: "fn() { 24 }();".to_string(),
        expected_constants: vec![
          Object::Integer(24),
          Object::CompiledFunction(vec![
            code::make(Opcode::OpConstant, &vec![0]),
            code::make(Opcode::OpReturnValue, &vec![]),
          ].into_iter().flatten().collect(), 0, 0)
        ],
        expected_instructions: vec![
            code::make(Opcode::OpConstant, &vec![1]),
            code::make(Opcode::OpCall, &vec![0]),
            code::make(Opcode::OpPop, &vec![])
        ]
      },
      CompilerTestCase {
        input: "
          let noArg = fn() { 24 };
          noArg();
        ".to_string(),
        expected_constants: vec![
          Object::Integer(24),
          Object::CompiledFunction(vec![
            code::make(Opcode::OpConstant, &vec![0]),
            code::make(Opcode::OpReturnValue, &vec![]),
          ].into_iter().flatten().collect(), 0, 0)
        ],
        expected_instructions: vec![
            code::make(Opcode::OpConstant, &vec![1]),
            code::make(Opcode::OpSetGlobal, &vec![0]),
            code::make(Opcode::OpGetGlobal, &vec![0]),
            code::make(Opcode::OpCall, &vec![0]),
            code::make(Opcode::OpPop, &vec![])
        ]
      },
      CompilerTestCase {
        input: "
          let num = 55;
          fn() { num }
        ".to_string(),
        expected_constants: vec![
          Object::Integer(55),
          Object::CompiledFunction(vec![
            code::make(Opcode::OpGetGlobal, &vec![0]),
            code::make(Opcode::OpReturnValue, &vec![])
          ].into_iter().flatten().collect(), 0, 0)
        ],
        expected_instructions: vec![
          code::make(Opcode::OpConstant, &vec![0]),
          code::make(Opcode::OpSetGlobal, &vec![0]),
          code::make(Opcode::OpConstant, &vec![1]),
          code::make(Opcode::OpPop, &vec![])
        ]
      },
      CompilerTestCase {
        input: "
          fn() {
            let num = 55;
            num
        }".to_string(),
        expected_constants: vec![
          Object::Integer(55),
          Object::CompiledFunction(vec![
            code::make(Opcode::OpConstant, &vec![0]),
            code::make(Opcode::OpSetLocal, &vec![0]),
            code::make(Opcode::OpGetLocal, &vec![0]),
            code::make(Opcode::OpReturnValue, &vec![])
          ].into_iter().flatten().collect(), 1, 0)
        ],
        expected_instructions: vec![
          code::make(Opcode::OpConstant, &vec![1]),
          code::make(Opcode::OpPop, &vec![])
        ]
      },
      CompilerTestCase {
        input: "
          fn() {
            let a = 55;
            let b = 77;
            a+b
          }".to_string(),
        expected_constants: vec![
          Object::Integer(55),
          Object::Integer(77),
          Object::CompiledFunction(vec![
            code::make(Opcode::OpConstant, &vec![0]),
            code::make(Opcode::OpSetLocal, &vec![0]),
            code::make(Opcode::OpConstant, &vec![1]),
            code::make(Opcode::OpSetLocal, &vec![1]),
            code::make(Opcode::OpGetLocal, &vec![0]),
            code::make(Opcode::OpGetLocal, &vec![1]),
            code::make(Opcode::OpAdd, &vec![]),
            code::make(Opcode::OpReturnValue, &vec![])
          ].into_iter().flatten().collect(), 2, 0)
        ],
        expected_instructions: vec![
          code::make(Opcode::OpConstant, &vec![2]),
          code::make(Opcode::OpPop, &vec![])
        ]
      },
      CompilerTestCase {
        input: "
          let oneArg = fn(a) { a }; oneArg(24);
        ".to_string(),
        expected_constants: vec![
          Object::CompiledFunction(vec![
            code::make(Opcode::OpGetLocal, &vec![0]),
            code::make(Opcode::OpReturnValue, &vec![])
          ].into_iter().flatten().collect(), 0, 1),
          Object::Integer(24)
        ],
        expected_instructions: vec![
          code::make(Opcode::OpConstant, &vec![0]),
          code::make(Opcode::OpSetGlobal, &vec![0]),
          code::make(Opcode::OpGetGlobal, &vec![0]),
          code::make(Opcode::OpConstant, &vec![1]),
          code::make(Opcode::OpCall, &vec![1]),
          code::make(Opcode::OpPop, &vec![])
        ]
      },
      CompilerTestCase {
        input: "
          let manyArg = fn(a, b, c) { a; b; c; }; manyArg(24, 25, 26);
        ".to_string(),
        expected_constants: vec![
          Object::CompiledFunction(vec![
            code::make(Opcode::OpGetLocal, &vec![0]),
            code::make(Opcode::OpPop, &vec![]),
            code::make(Opcode::OpGetLocal, &vec![1]),
            code::make(Opcode::OpPop, &vec![]),
            code::make(Opcode::OpGetLocal, &vec![2]),
            code::make(Opcode::OpReturnValue, &vec![])
          ].into_iter().flatten().collect(), 0, 3),
          Object::Integer(24),
          Object::Integer(25),
          Object::Integer(26)
        ],
        expected_instructions: vec![
          code::make(Opcode::OpConstant, &vec![0]),
          code::make(Opcode::OpSetGlobal, &vec![0]),
          code::make(Opcode::OpGetGlobal, &vec![0]),
          code::make(Opcode::OpConstant, &vec![1]),
          code::make(Opcode::OpConstant, &vec![2]),
          code::make(Opcode::OpConstant, &vec![3]),
          code::make(Opcode::OpCall, &vec![3]),
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
          (Object::CompiledFunction(ins1, _, _), Object::CompiledFunction(ins2, _, _)) => {
            for (index, ins) in ins1.iter().enumerate() {
              assert_eq!(*ins, ins2[index])
            }
          }
          _ => panic!("unhandle constant {:?} {:?}", constant, &bytecode.constants[i])
        }
      }
    }
  }
  #[test]
  fn test_resolve_local() {
    let mut global = SymbolTable::new();
    global.define("a");
    global.define("b");

    let mut local = SymbolTable::new_enclosed(global);
    local.define("c");
    local.define("d");

    let expected = vec![
      ("a", SymbolScope::Global, 0),
      ("b", SymbolScope::Global, 1),
      ("c", SymbolScope::Local, 0),
      ("d", SymbolScope::Local, 1)
    ];

    for symbol in expected {
      let resolved = local.resolve(symbol.0).unwrap();
      assert_eq!(resolved.scope, symbol.1);
      assert_eq!(resolved.index, symbol.2);
    }
  }
  #[test]
  fn test_nested_resolve() {
    let mut global = SymbolTable::new();
    global.define("a");
    global.define("b");

    let mut first_local = SymbolTable::new_enclosed(global);
    first_local.define("c");
    first_local.define("d");

    let expected = vec![
      ("a", SymbolScope::Global, 0),
      ("b", SymbolScope::Global, 1),
      ("c", SymbolScope::Local, 0),
      ("d", SymbolScope::Local, 1)
    ];

    for symbol in expected {
      let resolved = first_local.resolve(symbol.0).unwrap();
      assert_eq!(resolved.scope, symbol.1);
      assert_eq!(resolved.index, symbol.2);
    }

    let mut second_local = SymbolTable::new_enclosed(first_local);
    second_local.define("e");
    second_local.define("f");

    let expected = vec![
      ("a", SymbolScope::Global, 0),
      ("b", SymbolScope::Global, 1),
      ("e", SymbolScope::Local, 0),
      ("f", SymbolScope::Local, 1)
    ];

    for symbol in expected {
      let resolved = second_local.resolve(symbol.0).unwrap();
      assert_eq!(resolved.scope, symbol.1);
      assert_eq!(resolved.index, symbol.2);
    }
  }
}