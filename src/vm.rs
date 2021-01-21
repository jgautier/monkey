use crate::eval::Object;
use crate::code::Instructions;
use crate::compiler::Bytecode;
use crate::code::Opcode;
use std::convert::TryInto;
use std::rc::Rc;

const STACK_SIZE:usize = 2048;
const TRUE:Object = Object::Boolean(true);
const FALSE:Object = Object::Boolean(false);

pub struct VM {
  constants: Vec<Object>,
  instructions: Instructions,
  stack: Vec<Rc<Object>>,
  sp: usize
}

impl VM {
  pub fn new(bytecode: Bytecode) -> VM {
    return VM {
      instructions: bytecode.instructions,
      constants: bytecode.constants,
      stack: Vec::with_capacity(STACK_SIZE),
      sp: 0
    }
  }
  pub fn last_popped_stack_elem(&self) -> Rc<Object>{
    self.stack.get(self.sp).unwrap().clone()
  }

  pub fn run(&mut self) {
    let mut ip = 0;
    while ip < self.instructions.len() {
      let op = Opcode::lookup(self.instructions[ip]);
      ip += 1;
      match op {
        Opcode::OpConstant => {
          let const_index = u16::from_be_bytes(self.instructions[ip..ip + 2].try_into().expect("invalid slice")) as usize;
          self.push(self.constants[const_index].clone());
          ip += 2;
        }
        Opcode::OpAdd | Opcode::OpSub | Opcode::OpDiv | Opcode::OpMul => {
          self.execute_binary_operation(op)
        }
        Opcode::OpEqual | Opcode::OpNotEqual | Opcode::OpGreaterThan => {
          self.execute_comparison_operation(op)
        }
        Opcode::OpBang => {
          let operand = self.pop();
          match *operand {
            Object::Boolean(val) => {
              self.push(self.boolean_to_const(!val))
            }
            _ => {
              self.push(self.boolean_to_const(false))
            }
          }
        }
        Opcode::OpMinus => {
          let operand = self.pop();
          match *operand {
            Object::Integer(val) => {
              self.push(Object::Integer(val * -1))
            }
            _ => {
              panic!("unsupported type for negation")
            }
          }
        }
        Opcode::OpPop => {
          self.pop();
        }
        Opcode::OpTrue => {
          self.push(TRUE);
        }
        Opcode::OpFalse => {
          self.push(FALSE);
        }
        _ => panic!("unhandled op")
      }
    }
  }
  fn boolean_to_const(&self, val: bool) -> Object {
    if val {
      TRUE
    } else {
      FALSE
    }
  }
  fn execute_comparison_operation(&mut self, op: Opcode) {
    let right = self.pop();
    let left = self.pop();
    self.push(match (&*left, &*right) {
      (Object::Integer(val1), Object::Integer(val2)) => {
        match op {
          Opcode::OpGreaterThan => {
            self.boolean_to_const(val1 > val2)
          }
          Opcode::OpEqual => {
            self.boolean_to_const(val1 == val2)
          }
          Opcode::OpNotEqual => {
            self.boolean_to_const(val1 != val2)
          },
          _ => panic!("unsupported comparison operator on integers")
        }
      },
      (Object::Boolean(val1), Object::Boolean(val2)) => {
        match op {
          Opcode::OpEqual => {
            self.boolean_to_const(val1 == val2)
          }
          Opcode::OpNotEqual => {
            self.boolean_to_const(val1 != val2)
          }
          _ => panic!("unsupported comparion operator on booleans")
        }
      },
      _ => panic!("trying to add non integer values")
    });
  }
  fn execute_binary_operation(&mut self, op: Opcode) {
    let right = self.pop();
    let left = self.pop();
    let (left_value, right_value) = match (&*left, &*right) {
      (Object::Integer(val1), Object::Integer(val2)) => (val1,  val2),
      _ => panic!("trying to add non integer values")
    };
    let result = match op {
      Opcode::OpAdd => left_value + right_value,
      Opcode::OpSub => left_value - right_value,
      Opcode::OpMul => left_value * right_value,
      Opcode::OpDiv => left_value / right_value,
      _ => panic!("unsupported binary operator {:?}", op.definition())
    };
    self.push(Object::Integer(result))
  }
  fn push(&mut self, object: Object) {
    if self.stack.len() <= self.sp {
      self.stack.push(Rc::new(object))
    } else {
      self.stack[self.sp] = Rc::new(object)
    }
    self.sp += 1;
  }
  fn pop(&mut self) -> Rc<Object>{
    let frame = self.stack.get(self.sp - 1);
    self.sp -= 1;
    return frame.unwrap().clone()
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::lexer;
  use crate::ast;
  use crate::compiler::Compiler;
  use crate::ast::Program;
  #[derive(Debug)]
  struct VMTestCase {
    input: String,
    expected: Object,
  }
  fn parse(program: &str) -> Program {
    let lexer = lexer::Lexer::new(program);
    ast::Parser::new(lexer).parse()
  }
  #[test]
  fn run_tests() {
    let tests = vec![
      VMTestCase {
        input: "1".to_string(),
        expected: Object::Integer(1)
      },
      VMTestCase {
        input: "2".to_string(),
        expected: Object::Integer(2)
      },
      VMTestCase {
        input: "1 + 2".to_string(),
        expected: Object::Integer(3)
      },
      VMTestCase {
        input: "1 - 2".to_string(),
        expected: Object::Integer(-1)
      },
      VMTestCase {
        input: "1 * 2".to_string(),
        expected: Object::Integer(2)
      },
      VMTestCase {
        input: "4 / 2".to_string(),
        expected: Object::Integer(2)
      },
      VMTestCase {
        input: "50 / 2 * 2 + 10 - 5".to_string(),
        expected: Object::Integer(55)
      },
      VMTestCase {
        input: "5 + 5 + 5 + 5 - 10".to_string(),
        expected: Object::Integer(10)
      },
      VMTestCase {
        input: "2 * 2 * 2 * 2 * 2".to_string(),
        expected: Object::Integer(32)
      },
      VMTestCase {
        input: "5 * 2 + 10".to_string(),
        expected: Object::Integer(20)
      },
      VMTestCase {
        input: "5 + 2 * 10".to_string(),
        expected: Object::Integer(25)
      },
      VMTestCase {
        input: "5 * (2 + 10)".to_string(),
        expected: Object::Integer(60)
      },
      VMTestCase {
        input: "true".to_string(),
        expected: Object::Boolean(true)
      },
      VMTestCase {
        input: "false".to_string(),
        expected: Object::Boolean(false)
      },
      VMTestCase {
        input: "1 < 2".to_string(),
        expected: Object::Boolean(true)
      },
      VMTestCase {
        input: "1 > 2".to_string(),
        expected: Object::Boolean(false)
      },
      VMTestCase {
        input: "1 < 1".to_string(),
        expected: Object::Boolean(false)
      },
      VMTestCase {
        input: "1 > 1".to_string(),
        expected: Object::Boolean(false)
      },
      VMTestCase {
        input: "1 == 1".to_string(),
        expected: Object::Boolean(true)
      },
      VMTestCase {
        input: "1 != 1".to_string(),
        expected: Object::Boolean(false)
      },
      VMTestCase {
        input: "1 == 2".to_string(),
        expected: Object::Boolean(false)
      },
      VMTestCase {
        input: "1 != 2".to_string(),
        expected: Object::Boolean(true)
      },
      VMTestCase {
        input: "true == true".to_string(),
        expected: Object::Boolean(true)
      },
      VMTestCase {
        input: "false == false".to_string(),
        expected: Object::Boolean(true)
      },
      VMTestCase {
        input: "true == false".to_string(),
        expected: Object::Boolean(false)
      },
      VMTestCase {
        input: "true != false".to_string(),
        expected: Object::Boolean(true)
      },
      VMTestCase {
        input: "false != true".to_string(),
        expected: Object::Boolean(true)
      },
      VMTestCase {
        input: "(1 < 2) == true".to_string(),
        expected: Object::Boolean(true)
      },
      VMTestCase {
        input: "(1 < 2) == false".to_string(),
        expected: Object::Boolean(false)
      },
      VMTestCase {
        input: "(1 > 2) == true".to_string(),
        expected: Object::Boolean(false)
      },
      VMTestCase {
        input: "(1 > 2) == false".to_string(),
        expected: Object::Boolean(true)
      },
      VMTestCase {
        input: "-5".to_string(),
        expected: Object::Integer(-5)
      },
      VMTestCase {
        input: "-10".to_string(),
        expected: Object::Integer(-10)
      },
      VMTestCase {
        input: "-50 + 100 + -50".to_string(),
        expected: Object::Integer(0)
      },
      VMTestCase {
        input: "(5 + 10 * 2 + 15 / 3) * 2 + -10".to_string(),
        expected: Object::Integer(50)
      },
      VMTestCase {
        input: "!true".to_string(),
        expected: Object::Boolean(false)
      },
      VMTestCase {
        input: "!false".to_string(),
        expected: Object::Boolean(true)
      },
      VMTestCase {
        input: "!5".to_string(),
        expected: Object::Boolean(false)
      },
      VMTestCase {
        input: "!!true".to_string(),
        expected: Object::Boolean(true)
      },
      VMTestCase {
        input: "!!false".to_string(),
        expected: Object::Boolean(false)
      },
      VMTestCase {
        input: "!!5".to_string(),
        expected: Object::Boolean(true)
      }
    ];
    for test in tests {
      let program = parse(&test.input);
      let mut compiler = Compiler::new();
      compiler.compile(&program);
      let mut vm = VM::new(compiler.bytecode());
      vm.run();
      let stack_elem = vm.last_popped_stack_elem();
      match (test.expected, &*stack_elem) {
        (Object::Integer(val1), Object::Integer(val2)) => {
          assert_eq!(val1, *val2, "{:?}", test.input);
        }
        (Object::Boolean(val1), Object::Boolean(val2)) => {
          assert_eq!(val1, *val2, "{:?}", test.input);
        }
        _ => panic!("unhandled object type")
      }
    }
  }
}