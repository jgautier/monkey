use crate::code::Instructions;
use crate::code::Opcode;
use crate::code;
use crate::object::Object;
use crate::object::get_built_in_vec;
use crate::ast::Program;
use crate::ast::Statement;
use crate::ast::BlockStatement;
use crate::ast::Expression;
use std::collections::HashMap;
use std::convert::TryInto;
use std::rc::Rc;

#[derive(Clone)]
struct EmittedInstruction {
  opcode: Opcode,
  position: u32
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum SymbolScope {
  Global,
  Local,
  BuiltIn,
  Free,
  Function
}

#[derive(Debug, Clone)]
pub struct Symbol {
  index: usize,
  scope: SymbolScope
}

#[derive(Clone)]
pub struct SymbolTable {
  store: HashMap<String, Symbol>,
  outer: Option<Box<SymbolTable>>,
  free_symbols: Vec<Symbol>,
  num_defs: usize
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
      outer: None,
      free_symbols: vec![],
      num_defs: 0
    }
  }
  fn new_enclosed(outer: SymbolTable) -> SymbolTable {
    SymbolTable {
      store: HashMap::new(),
      outer: Some(Box::new(outer)),
      free_symbols: vec![],
      num_defs: 0
    }
  }
  pub fn define_builtin(&mut self, name: &str, index: usize) -> &Symbol {
    self.store.insert(name.to_string(), Symbol{index, scope: SymbolScope::BuiltIn});
    self.store.get(name).unwrap()
  }

  pub fn define_function_name(&mut self, name: &str) -> &Symbol {
    self.store.insert(name.to_string(), Symbol{index: 0, scope: SymbolScope::Function});
    self.store.get(name).unwrap()
  }

  pub fn define_free(&mut self, name: &str, original: Symbol) -> &Symbol {
    self.free_symbols.push(original);
    let symbol = Symbol{index: self.free_symbols.len() - 1, scope: SymbolScope::Free};
    self.store.insert(name.to_string(), symbol);
    self.store.get(name).unwrap()
  }

  fn define(&mut self, name: &str) -> &Symbol {
    let symbol = if self.outer.is_some() {
      Symbol{index: self.num_defs, scope: SymbolScope::Local}
    } else {
      Symbol{index: self.num_defs, scope: SymbolScope::Global}
    };
    self.store.insert(name.to_string(), symbol);
    self.num_defs += 1;
    self.store.get(name).unwrap()
  }
  fn resolve(&mut self, name: &str) -> Option<Symbol> {
    let result = self.store.get(name);
    if let Some(sym) = result {
      return Some(sym.clone());
    }
    let result = if let Some(outer) = &mut self.outer {
      outer.resolve(name)
    } else {
      None
    };
    if let Some(sym) = result {
      if let SymbolScope::Global | SymbolScope::BuiltIn = sym.scope {
        Some(sym)
      } else {
        Some(self.define_free(name,sym).clone())
      }
    } else {
      result
    }
  }
}

struct CompilationScope {
  instructions: Instructions,
  last_instruction: Option<EmittedInstruction>,
  previous_instruction: Option<EmittedInstruction>
}

pub struct Compiler {
  pub constants: Vec<Rc<Object>>,
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
    let mut symbol_table = SymbolTable::default();
    let built_ins = get_built_in_vec();
    for (i, built_in) in built_ins.into_iter().enumerate() {
      symbol_table.define_builtin(&built_in.0, i);
    }
    Compiler {
      scopes: vec![
        CompilationScope {
          instructions: Vec::new(),
          last_instruction: None,
          previous_instruction: None
        }
      ],
      constants: Vec::new(),
      symbol_table,
      scope_index: 0
    }
  }
  pub fn new_with_state(symbol_table: SymbolTable, constants: Vec<Rc<Object>>) -> Compiler {
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
        let symbol = self.symbol_table.define(identifier);
        let index = symbol.index;
        let scope = symbol.scope;
        self.compile_expression(value);
        if let SymbolScope::Global = scope {
          self.emit(Opcode::OpSetGlobal, vec![index as u32]);
        } else if let SymbolScope::BuiltIn = scope {
          self.emit(Opcode::OpGetBuiltIn, vec![index as u32]);
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
        self.load_symbol(&symbol);
      }
      Expression::Fn{params, body, name} => {
        self.enter_scope();
        self.symbol_table.define_function_name(name);
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
        let free_symbols = self.symbol_table.free_symbols.clone();
        let instructions = self.leave_scope();
        for symbol in &free_symbols {
          self.load_symbol(symbol);
        }
        let compiled_fn = Object::CompiledFunction(instructions, num_locals, params.len());
        let constant_index = self.add_constant(compiled_fn);
        self.emit(Opcode::OpClosure, vec![constant_index, free_symbols.len() as u32]);
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
  fn load_symbol(&mut self, symbol: &Symbol) {
    if let SymbolScope::Global = symbol.scope {
      self.emit(Opcode::OpGetGlobal, vec![symbol.index as u32]);
    } else if let SymbolScope::BuiltIn = symbol.scope {
      self.emit(Opcode::OpGetBuiltIn, vec![symbol.index as u32]);
    } else if let SymbolScope::Free = symbol.scope {
      self.emit(Opcode::OpGetFree, vec![symbol.index as u32]);
    } else if let SymbolScope::Function = symbol.scope {
      self.emit(Opcode::OpCurrentClosure, vec![]);
    } else {
      self.emit(Opcode::OpGetLocal, vec![symbol.index as u32]);
    }
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
    self.constants.push(Rc::new(obj));
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
  pub constants: Vec<Rc<Object>>
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
            code::make(Opcode::OpClosure, &vec![2, 0]),
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
            code::make(Opcode::OpClosure, &vec![2, 0]),
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
            code::make(Opcode::OpClosure, &vec![0, 0]),
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
            code::make(Opcode::OpClosure, &vec![1, 0]),
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
            code::make(Opcode::OpClosure, &vec![1, 0]),
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
          code::make(Opcode::OpClosure, &vec![1, 0]),
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
          code::make(Opcode::OpClosure, &vec![1, 0]),
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
          code::make(Opcode::OpClosure, &vec![2, 0]),
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
          code::make(Opcode::OpClosure, &vec![0, 0]),
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
          code::make(Opcode::OpClosure, &vec![0, 0]),
          code::make(Opcode::OpSetGlobal, &vec![0]),
          code::make(Opcode::OpGetGlobal, &vec![0]),
          code::make(Opcode::OpConstant, &vec![1]),
          code::make(Opcode::OpConstant, &vec![2]),
          code::make(Opcode::OpConstant, &vec![3]),
          code::make(Opcode::OpCall, &vec![3]),
          code::make(Opcode::OpPop, &vec![])
        ]
      },
      CompilerTestCase {
        input: "
          len([]);
          push([], 1);
        ".to_string(),
        expected_constants: vec![Object::Integer(1)],
        expected_instructions: vec![
          code::make(Opcode::OpGetBuiltIn, &vec![0]),
          code::make(Opcode::OpArray, &vec![0]),
          code::make(Opcode::OpCall, &vec![1]),
          code::make(Opcode::OpPop, &vec![]),
          code::make(Opcode::OpGetBuiltIn, &vec![4]),
          code::make(Opcode::OpArray, &vec![0]),
          code::make(Opcode::OpConstant, &vec![0]),
          code::make(Opcode::OpCall, &vec![2]),
          code::make(Opcode::OpPop, &vec![])
        ]
      },
      CompilerTestCase {
        input: "fn() { len([]) }".to_string(),
        expected_constants: vec![
          Object::CompiledFunction(vec![
            code::make(Opcode::OpGetBuiltIn, &vec![0]),
            code::make(Opcode::OpArray, &vec![0]),
            code::make(Opcode::OpCall, &vec![1]),
            code::make(Opcode::OpReturnValue, &vec![])
          ].into_iter().flatten().collect(), 0, 0)
        ],
        expected_instructions: vec![
          code::make(Opcode::OpClosure, &vec![0, 0]),
          code::make(Opcode::OpPop, &vec![])
        ]
      },
      CompilerTestCase {
        input: "fn(a) {
          fn(b) {
            a + b
          }
        }".to_string(),
        expected_constants: vec![
          Object::CompiledFunction(vec![
            code::make(Opcode::OpGetFree, &vec![0]),
            code::make(Opcode::OpGetLocal, &vec![0]),
            code::make(Opcode::OpAdd, &vec![]),
            code::make(Opcode::OpReturnValue, &vec![])
          ].into_iter().flatten().collect(), 0, 0),
          Object::CompiledFunction(vec![
            code::make(Opcode::OpGetLocal, &vec![0]),
            code::make(Opcode::OpClosure, &vec![0, 1]),
            code::make(Opcode::OpReturnValue, &vec![])
          ].into_iter().flatten().collect(), 0, 0)
        ],
        expected_instructions: vec![
          code::make(Opcode::OpClosure, &vec![1, 0]),
          code::make(Opcode::OpPop, &vec![])
        ]
      },
      CompilerTestCase {
        input: "
          fn(a) {
            fn(b) {
              fn(c) {
                a + b + c
              };
            }
          }".to_string(),
        expected_constants: vec![
          Object::CompiledFunction(vec![
            code::make(Opcode::OpGetFree, &vec![0]),
            code::make(Opcode::OpGetFree, &vec![1]),
            code::make(Opcode::OpAdd, &vec![]),
            code::make(Opcode::OpGetLocal, &vec![0]),
            code::make(Opcode::OpAdd, &vec![]),
            code::make(Opcode::OpReturnValue, &vec![])
          ].into_iter().flatten().collect(), 0, 0),
          Object::CompiledFunction(vec![
            code::make(Opcode::OpGetFree, &vec![0]),
            code::make(Opcode::OpGetLocal, &vec![0]),
            code::make(Opcode::OpClosure, &vec![0, 2]),
            code::make(Opcode::OpReturnValue, &vec![])
          ].into_iter().flatten().collect(), 0, 0),
          Object::CompiledFunction(vec![
            code::make(Opcode::OpGetLocal, &vec![0]),
            code::make(Opcode::OpClosure, &vec![1, 1]),
            code::make(Opcode::OpReturnValue, &vec![])
          ].into_iter().flatten().collect(), 0, 0)
        ],
        expected_instructions: vec![
          code::make(Opcode::OpClosure, &vec![2, 0]),
          code::make(Opcode::OpPop, &vec![])
        ]
      },
      CompilerTestCase {
        input: "
          let global = 55;
          fn() {
            let a = 66;
            fn() {
              let b = 77;
              fn() {
                let c = 88;
                global + a + b + c;
              };
            }
          }".to_string(),
        expected_constants: vec![
          Object::Integer(55),
          Object::Integer(66),
          Object::Integer(77),
          Object::Integer(88),
          Object::CompiledFunction(vec![
            code::make(Opcode::OpConstant, &vec![3]),
            code::make(Opcode::OpSetLocal, &vec![0]),
            code::make(Opcode::OpGetGlobal, &vec![0]),
            code::make(Opcode::OpGetFree, &vec![0]),
            code::make(Opcode::OpAdd, &vec![]),
            code::make(Opcode::OpGetFree, &vec![1]),
            code::make(Opcode::OpAdd, &vec![]),
            code::make(Opcode::OpGetLocal, &vec![0]),
            code::make(Opcode::OpAdd, &vec![]),
            code::make(Opcode::OpReturnValue, &vec![])
          ].into_iter().flatten().collect(), 0, 0),
          Object::CompiledFunction(vec![
            code::make(Opcode::OpConstant, &vec![2]),
            code::make(Opcode::OpSetLocal, &vec![0]),
            code::make(Opcode::OpGetFree, &vec![0]),
            code::make(Opcode::OpGetLocal, &vec![0]),
            code::make(Opcode::OpClosure, &vec![4, 2]),
            code::make(Opcode::OpReturnValue, &vec![])
          ].into_iter().flatten().collect(), 0, 0),
          Object::CompiledFunction(vec![
            code::make(Opcode::OpConstant, &vec![1]),
            code::make(Opcode::OpSetLocal, &vec![0]),
            code::make(Opcode::OpGetLocal, &vec![0]),
            code::make(Opcode::OpClosure, &vec![5, 1]),
            code::make(Opcode::OpReturnValue, &vec![])
          ].into_iter().flatten().collect(), 0, 0)
        ],
        expected_instructions: vec![
          code::make(Opcode::OpConstant, &vec![0]),
          code::make(Opcode::OpSetGlobal, &vec![0]),
          code::make(Opcode::OpClosure, &vec![6, 0]),
          code::make(Opcode::OpPop, &vec![])
        ]
      },
      CompilerTestCase {
        input: "let countDown = fn(x) { countDown(x - 1); }; countDown(1);".to_string(),
        expected_constants: vec![
          Object::Integer(1),
          Object::CompiledFunction(vec![
            code::make(Opcode::OpCurrentClosure, &vec![]),
            code::make(Opcode::OpGetLocal, &vec![0]),
            code::make(Opcode::OpConstant, &vec![0]),
            code::make(Opcode::OpSub, &vec![]),
            code::make(Opcode::OpCall, &vec![1]),
            code::make(Opcode::OpReturnValue, &vec![])
          ].into_iter().flatten().collect(), 0, 1)
        ],
        expected_instructions: vec![
          code::make(Opcode::OpClosure, &vec![1, 0]),
          code::make(Opcode::OpSetGlobal, &vec![0]),
          code::make(Opcode::OpGetGlobal, &vec![0]),
          code::make(Opcode::OpConstant, &vec![2]),
          code::make(Opcode::OpCall, &vec![1]),
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
      assert_eq!(bytecode.instructions.string(), test.expected_instructions.clone().into_iter().flatten().collect::<Instructions>().string(), "{}", &test.input);
      for (i, expected_instruction) in test.expected_instructions.iter().flatten().enumerate() {
        assert_eq!(bytecode.instructions[i], *expected_instruction);
      }
      for (i, constant) in test.expected_constants.iter().enumerate() {
        match (&*bytecode.constants[i], constant) {
          (Object::Integer(val), Object::Integer(val2)) => assert_eq!(val, val2),
          (Object::String(str1), Object::String(str2)) => assert_eq!(str1, str2),
          (Object::CompiledFunction(ins1, _, _), Object::CompiledFunction(ins2, _, _)) => {
            for (index, ins) in ins1.iter().enumerate() {
              assert_eq!(*ins, ins2[index], "{}\n{}", ins1.string(), ins2.string())
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
  #[test]
  fn resolve_free() {
    let mut global = SymbolTable::new();
    global.define("a");
    global.define("b");

    let mut first_local = SymbolTable::new_enclosed(global);
    first_local.define("c");
    first_local.define("d");

    let mut second_local = SymbolTable::new_enclosed(first_local);
    second_local.define("e");
    second_local.define("f");

    let expected = vec![
      (&mut second_local,
       vec![
        ("a", SymbolScope::Global, 0),
        ("b", SymbolScope::Global, 1),
        ("c", SymbolScope::Free, 0),
        ("d", SymbolScope::Free, 1),
        ("e", SymbolScope::Local, 0),
        ("f", SymbolScope::Local, 1)
       ],
       vec![
        ("c", SymbolScope::Local, 0),
        ("d", SymbolScope::Local, 1)
       ]
      )
    ];
    for test in expected {
      for symbol in test.1 {
        let result = test.0.resolve(symbol.0).unwrap();
        assert_eq!(result.scope, symbol.1);
        assert_eq!(result.index, symbol.2);
      }
      if test.0.free_symbols.len() != test.2.len() {
        panic!("Wrong number of free symbols")
      }
      for (i, symbol) in test.2.iter().enumerate() {
        let result = &test.0.free_symbols[i];
        assert_eq!(result.scope, symbol.1);
      }
    }
  }
  #[test]
  fn test_define_and_resolve_function_name() {
    let mut global = SymbolTable::new();
    global.define_function_name("a");
    let sym = global.resolve("a").unwrap();
    assert_eq!(sym.scope, SymbolScope::Function);
  }
  #[test]
  fn test_shadow_function_name() {
    let mut global = SymbolTable::new();
    global.define_function_name("a");
    global.define("a");
    let sym = global.resolve("a").unwrap();
    assert_eq!(sym.scope, SymbolScope::Global);
  }
  #[test]
  fn test_unresolvable() {
    let mut global = SymbolTable::new();
    global.define("a");
    let mut first_local = SymbolTable::new_enclosed(global);
    first_local.define("c");
    let mut second_local = SymbolTable::new_enclosed(first_local);
    second_local.define("e");
    second_local.define("f");

    assert_eq!(second_local.resolve("b").is_none(), true);
  }
}