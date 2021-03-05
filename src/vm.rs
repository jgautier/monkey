use crate::object::Object;
use crate::code::Instructions;
use crate::compiler::Bytecode;
use crate::code::Opcode;
use crate::object::HashKey;
use std::convert::TryInto;
use std::rc::Rc;
use std::collections::HashMap;

const STACK_SIZE:usize = 2048;
const GLOBALS_SIZE:usize = 65536;
const MAX_FRAMES:usize = 1024;
const TRUE:Object = Object::Boolean(true);
const FALSE:Object = Object::Boolean(false);
const NULL:Object = Object::Null;

struct Frame {
  func: Rc<Object>,
  ip: usize,
  base_pointer: usize
}

impl Frame {
  fn new(func: Rc<Object>, base_pointer: usize) -> Frame {
    Frame {
      func,
      ip: 0,
      base_pointer
    }
  }
  fn instructions(&self) -> Instructions {
    match &*self.func {
      Object::CompiledFunction(ins, _, _) => {
        ins.to_vec()
      },
      _ => {
        panic!("function not stored in frame")
      }
    }
  }
}

pub struct VM {
  constants: Vec<Object>,
  pub globals: Vec<Rc<Object>>,
  stack: Vec<Rc<Object>>,
  sp: usize,
  frames: Vec<Frame>,
  frames_index: usize
}

impl VM {
  pub fn new(bytecode: Bytecode) -> VM {
    let main_func = Object::CompiledFunction(bytecode.instructions, 0, 0);
    let frame = Frame::new(Rc::new(main_func), 0);
    let mut frames = Vec::<Frame>::with_capacity(MAX_FRAMES);
    frames.push(frame);
    VM {
      constants: bytecode.constants,
      globals: Vec::with_capacity(GLOBALS_SIZE),
      stack: vec![Rc::new(NULL); STACK_SIZE],
      sp: 0,
      frames,
      frames_index: 0
    }
  }
  fn push_frame(&mut self, frame: Frame) {
    self.frames.push(frame);
    self.frames_index += 1;
  }
  fn pop_frame(&mut self) {
    self.frames.pop();
    self.frames_index -= 1;
  }
  fn current_frame(&self) -> &Frame {
    &self.frames[self.frames_index]
  }
  fn set_ip(&mut self, ip: usize) {
    self.frames[self.frames_index].ip = ip
  }
  pub fn new_with_global_store(bytecode: Bytecode, globals: Vec<Rc<Object>>) -> VM {
    VM {
      constants: bytecode.constants,
      globals,
      stack: Vec::with_capacity(STACK_SIZE),
      sp: 0,
      frames: Vec::with_capacity(MAX_FRAMES),
      frames_index: 0
    }
  }
  pub fn last_popped_stack_elem(&self) -> Rc<Object>{
    self.stack.get(self.sp).unwrap().clone()
  }

  pub fn run(&mut self) {
    while self.current_frame().ip < self.current_frame().instructions().len() {
      let mut ip = self.current_frame().ip;
      let instructions = self.current_frame().instructions();
      let op = Opcode::lookup(instructions[ip]);
      ip += 1;
      self.set_ip(ip);
      match op {
        Opcode::OpSetGlobal => {
          let global_index = u16::from_be_bytes(instructions[ip..ip + 2].try_into().expect("invalid slice")) as usize;
          ip += 2;
          self.set_ip(ip);
          let len = self.globals.len();
          let global = self.pop();
          if len <= global_index {
            self.globals.push(global);
          } else {
            self.globals[global_index] = global;
          }
        }
        Opcode::OpGetGlobal => {
          let global_index = u16::from_be_bytes(instructions[ip..ip + 2].try_into().expect("invalid slice")) as usize;
          ip += 2;
          self.set_ip(ip);
          self.push((*self.globals[global_index]).clone())
        }
        Opcode::OpSetLocal => {
          let local_index = u8::from_be_bytes(instructions[ip..ip + 1].try_into().expect("invalid slice")) as usize;
          ip += 1;
          self.set_ip(ip);
          let bp = self.current_frame().base_pointer;
          self.stack[bp + local_index] = self.pop();
        }
        Opcode::OpGetLocal => {
          let local_index = u8::from_be_bytes(instructions[ip..ip + 1].try_into().expect("invalid slice")) as usize;
          ip += 1;
          self.set_ip(ip);
          let bp = self.current_frame().base_pointer;
          self.push_rc(self.stack[bp + local_index].clone());
        }
        Opcode::OpNull => {
          self.push(NULL);
        }
        Opcode::OpConstant => {
          let const_index = u16::from_be_bytes(instructions[ip..ip + 2].try_into().expect("invalid slice")) as usize;
          self.push(self.constants[const_index].clone());
          ip += 2;
          self.set_ip(ip);
        }
        Opcode::OpJumpNotTruthy  => {
          let pos = u16::from_be_bytes(instructions[ip..ip + 2].try_into().expect("invalid slice")) as usize;
          ip += 2;
          self.set_ip(ip);
          let condition = self.pop();
          if !self.is_truthy(condition) {
            ip = pos;
            self.set_ip(ip);
          }
        }
        Opcode::OpJump => {
          let pos = u16::from_be_bytes(instructions[ip..ip + 2].try_into().expect("invalid slice")) as usize;
          ip = pos;
          self.set_ip(ip);
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
            Object::Null => {
              self.push(self.boolean_to_const(true))
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
              self.push(Object::Integer(-val))
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
        Opcode::OpArray => {
          let num_elements = u16::from_be_bytes(instructions[ip..ip + 2].try_into().expect("invalid slice")) as usize;
          ip += 2;
          self.set_ip(ip);
          let mut elements = Vec::<Rc<Object>>::with_capacity(num_elements);
          for _ in 0..num_elements {
            elements.push(self.pop());
          }
          elements.reverse();
          self.push(Object::Array(elements));
        },
        Opcode::OpHash => {
          let num_elements = u16::from_be_bytes(instructions[ip..ip + 2].try_into().expect("invalid slice")) as usize;
          ip += 2;
          self.set_ip(ip);
          let mut hash = HashMap::new();
          for _ in 0..num_elements / 2 {
            let value = self.pop();
            let key = self.pop();
            match &*key {
              Object::String(val) => {
                hash.insert(HashKey::String(val.to_string()), value);
              },
              Object::Integer(val) => {
                hash.insert(HashKey::Integer(*val), value);

              },
              Object::Boolean(val) => {
                hash.insert(HashKey::Boolean(*val), value);
              },
              _ => {
                panic!("unsupported hash key")
              }
            }
          }
          self.push(Object::Hash(hash))
        },
        Opcode::OpIndex => {
          let index = self.pop();
          let left = self.pop();
          match (&*left, &*index) {
            (Object::Array(arr), Object::Integer(idx)) => {
              let null = &Rc::new(NULL);
              let val = arr.get(*idx as usize).unwrap_or(null);
              self.push_rc(val.clone());
            },
            (Object::Hash(hash), _) => {
              if let Some(hash_key) = HashKey::from_object(&index) {
                let null = &Rc::new(NULL);
                let val = hash.get(&hash_key).unwrap_or(null);
                self.push_rc(val.clone());
              } else {
                panic!("unsupported hash key {}", index.object_type());
              };
            },
            _ => panic!("index operator not supported on {} with {}", left.object_type(), index.object_type())
          }
        },
        Opcode::OpCall => {
          let num_args = u8::from_be_bytes(instructions[ip..ip + 1].try_into().unwrap()) as usize;
          ip += 1;
          self.set_ip(ip);
          let func = self.stack[self.sp - 1 - num_args].clone();
          if let Object::CompiledFunction(_, num_locals, num_params) = *func {
            if num_params != num_args {
              panic!("wrong number of arguments wanted {} got {}", num_params, num_args);
            }
            let frame = Frame::new(func, self.sp - num_args);
            let bp = frame.base_pointer;
            self.push_frame(frame);
            self.sp = bp + num_locals;
          } else {
            panic!("caling non-function")
          }

        },
        Opcode::OpReturnValue => {
          let return_value = self.pop();
          let bp = self.current_frame().base_pointer;
          self.pop_frame();
          self.sp = bp - 1;
          self.push_rc(return_value);
        },
        Opcode::OpReturn => {
          let bp = self.current_frame().base_pointer;
          self.pop_frame();
          self.sp = bp - 1;
          self.push(NULL);
        }
      }
    }
  }
  fn is_truthy(&self, obj: Rc<Object>) -> bool {
    match *obj {
      Object::Boolean(val) => val,
      Object::Null => false,
      _ => true
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
      (Object::String(val1), Object::String(val2)) => {
        self.push(Object::String(val1.to_string() + val2));
        return
      },
      _ => panic!("unsupported types for binary operation {} {}", left.object_type(), right.object_type())
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
    self.push_rc(Rc::new(object));
  }
  fn push_rc(&mut self, object: Rc<Object>) {
    self.stack[self.sp] = object;
    self.sp += 1;
  }
  fn pop(&mut self) -> Rc<Object>{
    let obj = self.stack.get(self.sp - 1);
    self.sp -= 1;
    obj.unwrap().clone()
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
      },
      VMTestCase {
        input: "if (true) { 10 }".to_string(),
        expected: Object::Integer(10)
      },
      VMTestCase {
        input: "if (true) { 10 } else { 20 }".to_string(),
        expected: Object::Integer(10)
      },
      VMTestCase {
        input: "if (false) { 10 } else { 20 }".to_string(),
        expected: Object::Integer(20)
      },
      VMTestCase {
        input: "if (1) { 10 }".to_string(),
        expected: Object::Integer(10)
      },
      VMTestCase {
        input: "if (1 < 2) { 10 }".to_string(),
        expected: Object::Integer(10)
      },
      VMTestCase {
        input: "if (1 < 2) { 10 } else { 20 }".to_string(),
        expected: Object::Integer(10)
      },
      VMTestCase {
        input: "if (1 > 2) { 10 } else { 20 }".to_string(),
        expected: Object::Integer(20)
      },
      VMTestCase {
        input: "if (1 > 2) { 10 }".to_string(),
        expected: NULL
      },
      VMTestCase {
        input: "if (false) { 10 }".to_string(),
        expected: NULL
      },
      VMTestCase {
        input: "!(if (false) { 5; })".to_string(),
        expected: Object::Boolean(true)
      },
      VMTestCase {
        input: "if ((if (false) { 10 })) { 10 } else { 20 }".to_string(),
        expected: Object::Integer(20)
      },
      VMTestCase {
        input: "let one = 1; one".to_string(),
        expected: Object::Integer(1)
      },
      VMTestCase {
        input: "let one = 1; let two = 2; one + two".to_string(),
        expected: Object::Integer(3)
      },
      VMTestCase {
        input: "let one = 1; let two = one + one; one + two".to_string(),
        expected: Object::Integer(3)
      },
      VMTestCase {
        input: "\"monkey\"".to_string(),
        expected: Object::String("monkey".to_string())
      },
      VMTestCase {
        input: "\"mon\" + \"key\"".to_string(),
        expected: Object::String("monkey".to_string())
      },
      VMTestCase {
        input: "\"mon\" + \"key\" + \"banana\"".to_string(),
        expected: Object::String("monkeybanana".to_string())
      },
      VMTestCase {
        input: "[]".to_string(),
        expected: Object::Array(vec![])
      },
      VMTestCase {
        input: "[1, 2, 3]".to_string(),
        expected: Object::Array(vec![
          Rc::new(Object::Integer(1)),
          Rc::new(Object::Integer(2)),
          Rc::new(Object::Integer(3))
        ])
      },
      VMTestCase {
        input: "[1 + 2, 3 * 4, 5 + 6]".to_string(),
        expected: Object::Array(vec![
          Rc::new(Object::Integer(3)),
          Rc::new(Object::Integer(12)),
          Rc::new(Object::Integer(11))
        ])
      },
      VMTestCase {
        input: "{}".to_string(),
        expected: Object::Hash([].iter().cloned().collect())
      },
      VMTestCase {
        input: "{1: 2, 2: 3}".to_string(),
        expected: Object::Hash([
          (HashKey::Integer(1), Rc::new(Object::Integer(2))),
          (HashKey::Integer(2), Rc::new(Object::Integer(3)))
        ].iter().cloned().collect())
      },
      VMTestCase {
        input: "{1 + 1: 2 * 2, 3 + 3: 4 * 4}".to_string(),
        expected: Object::Hash([
          (HashKey::Integer(2), Rc::new(Object::Integer(4))),
          (HashKey::Integer(6), Rc::new(Object::Integer(16)))
        ].iter().cloned().collect())
      },
      VMTestCase {
        input: "[1, 2, 3][1]".to_string(),
        expected: Object::Integer(2)
      },
      VMTestCase {
        input: "[1, 2, 3][0 + 2]".to_string(),
        expected: Object::Integer(3)
      },
      VMTestCase {
        input: "[[1, 1, 1]][0][0]".to_string(),
        expected: Object::Integer(1)
      },
      VMTestCase {
        input: "[][0]".to_string(),
        expected: NULL
      },
      VMTestCase {
        input: "[1, 2, 3][99]".to_string(),
        expected: NULL
      },
      VMTestCase {
        input: "[1][-1]".to_string(),
        expected: NULL
      },
      VMTestCase {
        input: "{1: 1, 2: 2}[1]".to_string(),
        expected: Object::Integer(1)
      },
      VMTestCase {
        input: "{1: 1, 2: 2}[2]".to_string(),
        expected: Object::Integer(2)
      },
      VMTestCase {
        input: "{1: 1}[0]".to_string(),
        expected: NULL
      },
      VMTestCase {
        input: "{}[0]".to_string(),
        expected: NULL
      },
      VMTestCase {
        input: "let fivePlusTen = fn() { 5 + 10; }; fivePlusTen();".to_string(),
        expected: Object::Integer(15)
      },
      VMTestCase {
        input: "let one = fn() { 1; }; let two = fn() { 2; }; one() + two()".to_string(),
        expected: Object::Integer(3)
      },
      VMTestCase {
        input: "let a = fn() { 1 }; let b = fn() { a() + 1 }; let c = fn() { b() + 1 }; c();".to_string(),
        expected: Object::Integer(3)
      },
      VMTestCase {
        input: "let earlyExit = fn() { return 99; 100; }; earlyExit();".to_string(),
        expected: Object::Integer(99)
      },
      VMTestCase {
        input: "let earlyExit = fn() { return 99; return 100; }; earlyExit();".to_string(),
        expected: Object::Integer(99)
      },
      VMTestCase {
        input: "let noReturn = fn() { }; noReturn();".to_string(),
        expected: NULL
      },
      VMTestCase {
        input: "let noReturn = fn() { };
        let noReturnTwo = fn() { noReturn(); }; noReturn();
        noReturnTwo();".to_string(),
        expected: NULL
      },
      VMTestCase {
        input: "
        let returnsOne = fn() { 1; };
        let returnsOneReturner = fn() { returnsOne; }; returnsOneReturner()();".to_string(),
        expected: Object::Integer(1)
      },
      VMTestCase {
        input: "
        let one = fn() { let one = 1; one }; one();".to_string(),
        expected: Object::Integer(1)
      },
      VMTestCase {
        input: "
          let oneAndTwo = fn() { let one = 1; let two = 2; one + two; }; oneAndTwo();
        ".to_string(),
        expected: Object::Integer(3)
      },
      VMTestCase {
        input: "
          let oneAndTwo = fn() { let one = 1; let two = 2; one + two; };
          let threeAndFour = fn() { let three = 3; let four = 4; three + four; };
          oneAndTwo() + threeAndFour();
        ".to_string(),
        expected: Object::Integer(10)
      },
      VMTestCase {
        input: "
          let firstFoobar = fn() { let foobar = 50; foobar; }; let secondFoobar = fn() { let foobar = 100; foobar; }; firstFoobar() + secondFoobar();
        ".to_string(),
        expected: Object::Integer(150)
      },
      VMTestCase {
        input: "
          let globalSeed = 50;
          let minusOne = fn() {
          let num = 1;
          globalSeed - num; }
                  let minusTwo = fn() {
                      let num = 2;
          globalSeed - num; }
          minusOne() + minusTwo();
        ".to_string(),
        expected: Object::Integer(97)
      },
      VMTestCase {
        input: "
          let returnsOneReturner = fn() {
            let returnsOne = fn() { 1; };
            returnsOne;
          }; returnsOneReturner()();
        ".to_string(),
        expected: Object::Integer(1)
      },
      VMTestCase {
        input: "
          let sum = fn(a, b) {
            let c = a + b;
            c;
          };
          sum(1, 2);
        ".to_string(),
        expected: Object::Integer(3)
      },
      VMTestCase {
        input: "
          let sum = fn(a, b) {
            let c = a + b;
            c;
          };
          sum(1, 2) + sum(3, 4);
        ".to_string(),
        expected: Object::Integer(10)
      },
      VMTestCase {
        input: "
        let sum = fn(a, b) {
          let c = a + b;
          c;
        };
        let outer = fn() {
            sum(1, 2) + sum(3, 4);
        };
        outer();".to_string(),
        expected: Object::Integer(10)
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
        (Object::Null, Object::Null) => {
          assert_eq!(true, true)
        },
        (Object::String(val1), Object::String(val2)) => {
          assert_eq!(val1, *val2)
        },
        (Object::Array(val1), Object::Array(val2)) => {
          for (i, _) in val1.iter().enumerate() {
            match (&*val1[i], &*val2[i]) {
              (Object::Integer(int_1), Object::Integer(int_2)) => {
                assert_eq!(int_1, int_2);
              },
              _ => {
                panic!("unhandled array comparison")
              }
            }
          }
        },
        (Object::Hash(hash1), Object::Hash(hash2)) => {
          for key in hash1.keys() {
            let val1 = &**hash1.get(key).unwrap();
            let val2 = &**hash2.get(key).unwrap();
            match (val1, val2) {
              (Object::Integer(int1), Object::Integer(int2)) => {
                assert_eq!(int1, int2)
              }
              _ => {
                panic!("unhandled hash comparison")
              }
            }
          }
        }
        _ => panic!("unhandled object type")
      }
    }
  }
  #[test]
  #[should_panic(expected = "wrong number of arguments wanted 0 got 1")]
  fn test_wrong_args() {
    let program = parse("fn() { 1; }(1);");
    let mut compiler = Compiler::new();
    compiler.compile(&program);
    let mut vm = VM::new(compiler.bytecode());
    vm.run()
  }
}