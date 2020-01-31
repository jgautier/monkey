use crate::ast;
use crate::ast::Node;
use std::rc::Rc;
use std::cell::RefCell;
use std::collections::HashMap;

pub trait Object {
  fn object_type(&self) -> String;
  fn inspect(&self) -> String;
}

#[derive(Debug, Clone)]
pub enum ObjectType {
  Integer(Integer),
  StringObj(StringObj),
  Boolean(Boolean),
  Null(Null),
  Return(Box<ObjectType>),
  Function(Function),
  BuiltIn(BuiltIn),
  Error(String)
}

impl Object for ObjectType {
  fn object_type(&self) -> String {
    match self {
      ObjectType::Integer(_) => {
        "INTEGER".to_string()
      },
      ObjectType::Boolean(_) => {
        "BOOLEAN".to_string()
      },
      ObjectType::Null(_) => {
        "NULL".to_string()
      },
      ObjectType::Return(_) => {
        "RETURN".to_string()
      },
      ObjectType::Error(_) => {
        "ERROR".to_string()
      },
      ObjectType::Function(_) => {
        "FUNCTION".to_string()
      },
      ObjectType::StringObj(_) => {
        "STRING".to_string()
      },
      ObjectType::BuiltIn(_) => {
        "BUILTIN".to_string()
      },
    }
  }
  fn inspect(&self) -> String {
    match self {
      ObjectType::Integer(i) => {
        format!("{}", i.value)
      },
      ObjectType::Boolean(b) => {
        format!("{}", b.value)
      },
      ObjectType::Null(_) => {
        "null".to_string()
      },
      ObjectType::Return(obj) => {
        obj.inspect()
      },
      ObjectType::Error(err_str) => {
        err_str.to_string()
      },
      ObjectType::StringObj(string) => {
        string.value.to_string()
      }
      ObjectType::Function(func) => {
        let params = func.params.clone().into_iter().map(|p| p.token.to_string()).collect::<Vec<String>>().join(",");
        let strings = vec![
          format!("fn({}) {{", params),
          func.body.to_string(),
          "}}".to_string()
        ];
        strings.join("\n")
      },
      ObjectType::BuiltIn(_) => {
        "builtin function".to_string()
      }
    }
  }
}
#[derive(Debug, Clone, Copy)]
pub struct Integer {
  value: i64
}

#[derive(Debug, Clone)]
pub struct StringObj {
  value: String
}


#[derive(Debug, Clone, Copy)]
pub struct Boolean {
  value: bool
}
#[derive(Debug, Clone)]
pub struct Null {}

#[derive(Debug, Clone)]
pub struct Function {
  params: Vec<ast::Identifier>,
  body: ast::BlockStatement,
  env: Rc<RefCell<Environment>>
}

#[derive(Debug)]
pub struct Environment {
  store: HashMap<String, ObjectType>,
  outer: Option<Rc<RefCell<Environment>>>
}

impl Environment {
  pub fn new(outer: Option<Rc<RefCell<Environment>>>) -> Self {
    Self {
      store: HashMap::new(),
      outer
    }
  }
  fn get(&self, var_name: String) -> Option<ObjectType> {
    let val = self.store.get(&var_name);
    if let Some(value) = val {
      return Some(value.clone());
    }
    if let Some(env) = &self.outer {
       return env.borrow().get(var_name);
    }
    None
  }
  fn set(&mut self, var_name: String, value: &ObjectType) {
    self.store.insert(var_name, value.clone());
  }
}

#[derive(Debug, Clone)]
pub struct BuiltIn {
  function: fn(Vec<ObjectType>) -> ObjectType
}

pub struct Evaluator {
  env: Rc<RefCell<Environment>>,
  built_ins: HashMap<String, ObjectType>
}

impl Evaluator {
  pub fn new() -> Evaluator {
    let mut built_ins = HashMap::new();
    built_ins.insert("len".to_string(), ObjectType::BuiltIn(BuiltIn {
      function: |args: Vec<ObjectType>| -> ObjectType {
        if args.len() > 1 {
          return ObjectType::Error(format!("Expected 1 argument got {}", args.len()));
        }
        if let ObjectType::StringObj(string) = &args[0] {
          ObjectType::Integer(Integer { value: string.value.len() as i64 })
        } else {
          ObjectType::Error(format!("Expected a String got a {}", args[0].object_type()))
        }
      }
    }));
    Self {
      env: Rc::new(RefCell::new(Environment::new(None))),
      built_ins
    }
  }
  pub fn eval_program(&self, prog: ast::Program) -> ObjectType {
    let mut result = ObjectType::Null(Null {});
    for stmt in prog.statements {
      result = self.eval_statement(stmt, &self.env);
      match result {
        ObjectType::Return(obj) => {
          return *obj
        },
        ObjectType::Error(_) => {
          return result
        }
        _=> {}
      }
    }
    result
  }
  fn eval_expression(&self, expr: ast::Expression, env: &Rc<RefCell<Environment>>) -> ObjectType {
    match expr {
      ast::Expression::Infix(expr) => {
        let left = self.eval_expression(*expr.left, env);
        if let ObjectType::Error(_) = left {
          return left
        }
        let right = self.eval_expression(*expr.right, env);
        if let ObjectType::Error(_) = right {
          return right
        }
        eval_infix_expression(&expr.operator, left, right)
      },
      ast::Expression::Prefix(expr) => {
        let right = self.eval_expression(*expr.right, env);
        if let ObjectType::Error(_) = right {
          return right
        }
        eval_prefix_expression(&expr.operator, right)
      },
      ast::Expression::IntegerLiteral(int) => {
        ObjectType::Integer(Integer { value: int.value })
      },
      ast::Expression::Boolean(boolean) => {
        ObjectType::Boolean(Boolean { value: boolean.value })
      },
      ast::Expression::If(i) => {
        self.eval_if_expression(i, env)
      },
      ast::Expression::Identifier(id) => {
        match env.borrow().get(id.to_string()) {
          Some(obj) => {
            match obj {
              ObjectType::Integer(i) => {
                ObjectType::Integer(Integer { value: i.value })
              },
              ObjectType::Boolean(b) => {
                ObjectType::Boolean(Boolean { value: b.value })
              },
              ObjectType::Function(f) => {
                ObjectType::Function(Function { params: f.params, body: f.body, env: f.env })
              }
              ObjectType::StringObj(s) => {
                ObjectType::StringObj(StringObj { value: s.value })
              }
              _ => {
                ObjectType::Null(Null {})
              }
            }
          },
          None => {
            match self.built_ins.get(&id.to_string()) {
              Some(obj) => {
                match obj {
                  ObjectType::BuiltIn(b) => {
                    ObjectType::BuiltIn(BuiltIn { function: b.function })
                  }
                  _ => {
                    ObjectType::Null(Null {})
                  }
                }
              }
              None => {
                ObjectType::Error(format!("identifier not found: {}", id.to_string()))
              }
            }
          }
        }
      },
      ast::Expression::Call(call) => {
        let res = self.eval_expression(*call.function, env);
        match res {
          ObjectType::Error(_) => {
            res
          },
          ObjectType::Function(func) => {
            let mut args = Vec::new();
            for arg in call.args {
              let res = self.eval_expression(arg, env);
              if let ObjectType::Error(_) = res {
                return res
              }
              args.push(res);
            }
            let mut func_env = Environment::new(Some(Rc::clone(&func.env)));
            for (idx, param) in func.params.iter().enumerate() {
              func_env.set(param.to_string(), &args[idx]);
            }
            let func_res = self.eval_block_statements(func.body.statements, &Rc::new(RefCell::new(func_env)));
            if let ObjectType::Return(val) = func_res {
              return *val;
            }
            func_res
          },
          ObjectType::BuiltIn(built_in) => {
            let mut args = Vec::new();
            for arg in call.args {
              let res = self.eval_expression(arg, env);
              if let ObjectType::Error(_) = res {
                return res
              }
              args.push(res);
            }
            (built_in.function)(args)
          },
          _ => {
            ObjectType::Error("Not a Function".to_string())
          }
        }
      }
      ast::Expression::Fn(fn_expr) => {
        ObjectType::Function(Function { params: fn_expr.params, body: *fn_expr.body, env: Rc::clone(env) })
      },
      ast::Expression::StringLiteral(string) => {
        ObjectType::StringObj(StringObj{ value: string.value })
      }
    }
  }
  fn eval_if_expression(&self, if_expr: ast::If, env: &Rc<RefCell<Environment>>) -> ObjectType {
    let condition = self.eval_expression(*if_expr.condition, env);
    if let ObjectType::Error(_) = condition {
      return condition
    }
    if is_truthy(condition) {
      self.eval_block_statements(if_expr.consequence.statements, env)
    } else {
      match if_expr.alternative {
        Some(block) => {
          self.eval_block_statements(block.statements, env)
        },
        _ => {
          ObjectType::Null(Null {})
        }
      }
    }
  }

  fn eval_statement(&self, node: ast::StatementType, env: &Rc<RefCell<Environment>>) -> ObjectType {
    match node {
      ast::StatementType::Expression(expr) => {
        self.eval_expression(expr.expr, env)
      },
      ast::StatementType::Return(ret_stmt) => {
        let ret = self.eval_expression(*ret_stmt.value, env);
        if let ObjectType::Error(_) = ret {
          return ret
        }
        ObjectType::Return(Box::new(ret))
      },
      ast::StatementType::Let(stmt) => {
        let ret = self.eval_expression(*stmt.value, env);
        env.borrow_mut().set(stmt.name.to_string(), &ret);
        ObjectType::Null(Null {})
      }
    }
  }
  pub fn eval_block_statements(&self, stmts: Vec<ast::StatementType>, env: &Rc<RefCell<Environment>>) -> ObjectType {
    let mut result = ObjectType::Null(Null {});
    for stmt in stmts {
      result = self.eval_statement(stmt, env);
      match result {
        ObjectType::Return(_) | ObjectType::Error(_) => {
          return result
        },
        _=> {}
      }
    }
    result
  }
}


fn eval_bang_operator(right: ObjectType) -> ObjectType {
  let val = match right {
    ObjectType::Boolean(b) => {
      !b.value
    },
    ObjectType::Null(_) => true,
    _ => false
  };
  ObjectType::Boolean(Boolean { value: val })
}

fn eval_minus_operator(right: ObjectType) -> ObjectType {
  match right {
    ObjectType::Integer(i) => ObjectType::Integer(Integer { value : -i.value }),
    _ => {
      ObjectType::Error(format!("unknown operator: -{}", right.object_type())) 
    }
  }
}

fn eval_prefix_expression(operator: &str, right: ObjectType) -> ObjectType {
  match operator {
    "!" => {
      eval_bang_operator(right)  
    }
    "-" => {
      eval_minus_operator(right)
    }
    _ => {
      ObjectType::Error(format!("unknown operator: {}{}", operator, right.object_type())) 
    }
  }
}

fn eval_string_infix_expression(operator: &str, left: StringObj, right: StringObj) -> ObjectType {
  match operator {
    "+" => {
      ObjectType::StringObj(StringObj { value: format!("{}{}", left.value, right.value) })
    },
    _ => {
       ObjectType::Error(format!("unknown operator: {} for strings.", operator)) 
    }
  }
}

fn eval_int_infix_expression(operator: &str, left: Integer, right: Integer) -> ObjectType {
  match operator {
    "+" => {
      ObjectType::Integer(Integer { value: left.value + right.value })
    },
    "-" => {
      ObjectType::Integer(Integer { value: left.value - right.value })
    },
    "*" => {
      ObjectType::Integer(Integer { value: left.value * right.value })
    },
    "/" => {
      ObjectType::Integer(Integer { value: left.value / right.value })
    },
    "<" => {
      ObjectType::Boolean(Boolean { value: left.value < right.value })
    },
    ">" => {
      ObjectType::Boolean(Boolean { value: left.value > right.value })
    },
    "==" => {
      ObjectType::Boolean(Boolean { value: left.value == right.value })
    },
    "!=" => {
      ObjectType::Boolean(Boolean { value: left.value != right.value })
    },
    _ => {
       ObjectType::Error(format!("unknown operator: {} for integers.", operator)) 
    }
  }
}

fn eval_bool_infix_expression(operator: &str, left: Boolean, right: Boolean) -> ObjectType {
  match operator {
    "==" => {
      ObjectType::Boolean(Boolean { value: left.value == right.value })
    },
    "!=" => {
      ObjectType::Boolean(Boolean { value: left.value != right.value })
    },
    _ => {
      ObjectType::Error(format!("unknown operator: {} for booleans", operator)) 
    }
  }
}

fn eval_infix_expression(operator: &str, left: ObjectType, right: ObjectType) -> ObjectType {
  let left_type = left.object_type();
  let right_type = right.object_type();
  match (left, right) {
    (ObjectType::Integer(i1), ObjectType::Integer(i2)) => {
      eval_int_infix_expression(operator, i1, i2)
    },
    (ObjectType::Boolean(b1), ObjectType::Boolean(b2)) => {
      eval_bool_infix_expression(operator, b1, b2)
    },
    (ObjectType::StringObj(s1), ObjectType::StringObj(s2)) => {
      eval_string_infix_expression(operator, s1, s2)
    }
    _ => {
      ObjectType::Error(format!("type mismatch: {} {} {}", left_type, operator, right_type)) 
    }
  }
}

fn is_truthy(val: ObjectType) -> bool {
  match val {
    ObjectType::Null(_) => {
      false
    },
    ObjectType::Boolean(b) => {
      b.value
    }
    _=> {
      true
    }
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::lexer::Lexer;
  #[test]
  fn test_integer_literal() {
    let lexer = Lexer::new(&"5");
    let prog = ast::Parser::new(lexer).parse();
    let obj = Evaluator::new().eval_program(prog);
    if let ObjectType::Integer(i) = obj {
      assert_eq!(5, i.value);
    } else {
      assert_eq!(false, true)
    }
  }
  #[test]
  fn test_boolean_literal() {
    let lexer = Lexer::new(&"true");
    let prog = ast::Parser::new(lexer).parse();
    let obj = Evaluator::new().eval_program(prog);
    if let ObjectType::Boolean(i) = obj {
      assert_eq!(true, i.value);
    } else {
      assert_eq!(false, true)
    }
  }
  #[test]
  fn test_bang_operator () {
    let tests = vec![
      ("!true", false),
      ("!false", true),
      ("!5", false),
      ("!!true", true),
      ("!!false", false),
      ("!!5", true)
    ];
    for test in tests {
      let lexer = Lexer::new(&test.0);
      let prog = ast::Parser::new(lexer).parse();

      let obj = Evaluator::new().eval_program(prog);
      if let ObjectType::Boolean(i) = obj {
        assert_eq!(test.1, i.value);
      } else {
        assert_eq!(false, true)
      }
    }
  }
  #[test]
  fn test_minus_operator () {
    let tests = vec![
      ("5", 5),
      ("10", 10),
      ("-5", -5),
      ("-10", -10),
    ];
    for test in tests {
      let lexer = Lexer::new(&test.0);
      let prog = ast::Parser::new(lexer).parse();

      let obj = Evaluator::new().eval_program(prog);
      if let ObjectType::Integer(i) = obj {
        assert_eq!(test.1, i.value);
      } else {
        assert_eq!(false, true)
      }
    }
  }
  #[test]
  fn test_integer_infix () {
    let tests = vec![
      ("5 + 5 + 5 + 5 - 10", 10),
      ("2 * 2 * 2 * 2 * 2", 32),
      ("-50 + 100 + -50", 0),
      ("5 * 2 + 10", 20),
      ("5 + 2 * 10", 25),
      ("20 + 2 * -10", 0),
      ("50 / 2 * 2 + 10", 60),
      ("2 * (5 + 10)", 30),
      ("3 * 3 * 3 + 10", 37),
      ("3 * (3 * 3) + 10", 37),
      ("(5 + 10 * 2 + 15 / 3) * 2 + -10", 50)
    ];
    for test in tests {
      let lexer = Lexer::new(&test.0);
      let prog = ast::Parser::new(lexer).parse();

      let obj = Evaluator::new().eval_program(prog);
      if let ObjectType::Integer(i) = obj {
        assert_eq!(test.1, i.value);
      } else {
        assert_eq!(false, true)
      }
    }
  }
   #[test]
  fn test_integer_boolean_infix () {
    let tests = vec![
      ("1 < 2", true),
      ("1 > 2", false),
      ("1 < 1", false),
      ("1 > 1", false),
      ("1 == 1", true),
      ("1 != 1", false),
      ("1 == 2", false),
      ("1 != 2", true)
    ];
    for test in tests {
      let lexer = Lexer::new(&test.0);
      let prog = ast::Parser::new(lexer).parse();

      let obj = Evaluator::new().eval_program(prog);
      if let ObjectType::Boolean(b) = obj {
        assert_eq!(test.1, b.value);
      } else {
        assert_eq!(false, true)
      }
    }
  }
  #[test]
  fn test_boolean_infix () {
    let tests = vec![
      ("true == true", true),
      ("false == false", true),
      ("true == false", false),
      ("true != false", true),
      ("false != true", true),
      ("(1 < 2) == true", true),
      ("(1 < 2) == false", false),
      ("(1 > 2) == true", false),
      ("(1 > 2) == false", true)
    ];
    for test in tests {
      let lexer = Lexer::new(&test.0);
      let prog = ast::Parser::new(lexer).parse();

      let obj = Evaluator::new().eval_program(prog);
      if let ObjectType::Boolean(b) = obj {
        assert_eq!(test.1, b.value);
      } else {
        assert_eq!(false, true)
      }
    }
  }
   #[test]
  fn test_if_expressions () {
    let tests = vec![      
      ("if (true) { 10 }", 10),
      ("if (1) { 10 }", 10),
      ("if (1 < 2) { 10 }", 10),
      ("if (1 > 2) { 10 } else { 20 }", 20),
      ("if (1 < 2) { 10 } else { 20 }", 10)
    ];
    for test in tests {
      let lexer = Lexer::new(&test.0);
      let prog = ast::Parser::new(lexer).parse();

      let obj = Evaluator::new().eval_program(prog);
      if let ObjectType::Integer(i) = obj {
        assert_eq!(test.1, i.value);
      } else {
        assert_eq!(false, true)
      }
    }
  }
   #[test]
  fn test_if_no_else_expressions () {
    let tests = vec![      
      "if (false) { 10 }",
      "if (1 > 2) { 10 }"
    ];
    for test in tests {
      let lexer = Lexer::new(&test);
      let prog = ast::Parser::new(lexer).parse();

      let obj = Evaluator::new().eval_program(prog);
      if let ObjectType::Null(_) = obj {
        assert_eq!(true, true);
      } else {
        assert_eq!(false, true)
      }
    }
  }
  #[test]
  fn test_return_statements() {
    let tests = vec![      
      ("return 10;", 10),
      ("return 10; 9;", 10),
      ("return 2 * 5; 9;", 10),
      ("9; return 2 * 5; 9;", 10),
      ("if (10 > 1) {
          if (10 > 1) {
            return 10;
          }
          return 1; 
        }
      ", 10)
    ];
    for test in tests {
      let lexer = Lexer::new(&test.0);
      let prog = ast::Parser::new(lexer).parse();

      let obj = Evaluator::new().eval_program(prog);
      if let ObjectType::Integer(i) = obj {
        assert_eq!(i.value, test.1);
      } else {
        assert_eq!(false, true)
      }
    }
  }
  #[test]
  fn test_error_statements() {
    let tests = vec![      
      ("5 + true;", "type mismatch: INTEGER + BOOLEAN"),
      ("5 + true; 5;", "type mismatch: INTEGER + BOOLEAN"),
      ("-true", "unknown operator: -BOOLEAN"),
      ("true + false;", "unknown operator: + for booleans"),
      ("5; true + false; false", "unknown operator: + for booleans"),
      ("if (10 > 1 ) { true + false }", "unknown operator: + for booleans"),
      ("if (10 > 1) {
          if (10 > 1) {
            return true + false;
          }
          return 1; 
        }
      ", "unknown operator: + for booleans"),
      ("foobar", "identifier not found: foobar")
    ];
    for test in tests {
      let lexer = Lexer::new(&test.0);
      let prog = ast::Parser::new(lexer).parse();

      let obj = Evaluator::new().eval_program(prog);
      if let ObjectType::Error(err_str) = obj {
        assert_eq!(err_str, test.1);
      } else {
        println!("{}", obj.inspect());
        assert_eq!(false, true)
      }
    }
  }
  #[test]
  fn test_let_statements() {
    let tests = vec![      
      ("let a = 5; a;", 5),
      ("let a = 5 * 5; a;", 25),
      ("let a = 5; let b = a; b;", 5),
      ("let a = 5; let b = a; let c = a + b + 5; c;", 15)
    ];
    for test in tests {
      let lexer = Lexer::new(&test.0);
      let prog = ast::Parser::new(lexer).parse();

      let obj = Evaluator::new().eval_program(prog);
      if let ObjectType::Integer(i) = obj {
        assert_eq!(i.value, test.1);
      } else {
        println!("{}", obj.inspect());
        assert_eq!(false, true)
      }
    }
  }
  #[test]
  fn test_funcs() {
    let tests = vec![      
      ("let identity = fn(x) { x; }; identity(5);", 5),
      ("let identity = fn(x) { return x; }; identity(5);", 5),
      ("let double = fn(x) { x * 2; }; double(5);", 10),
      ("let add = fn(x, y) { x + y; }; add(5, 5);", 10),
      ("let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));", 20),
      ("fn(x) { x; }(5)",5)
    ];
    for test in tests {
      let lexer = Lexer::new(&test.0);
      let prog = ast::Parser::new(lexer).parse();

      let obj = Evaluator::new().eval_program(prog);
      if let ObjectType::Integer(i) = obj {
        assert_eq!(i.value, test.1);
      } else {
        println!("{}", obj.inspect());
        assert_eq!(false, true)
      }
    }
  }
  #[test]
  fn test_closures() {
    let tests = vec![      
      ("let newAdder = fn(x) {
          fn(y) { x + y };
        };
        let addTwo = newAdder(2);
        addTwo(2);", 4),
    ];
    for test in tests {
      let lexer = Lexer::new(&test.0);
      let prog = ast::Parser::new(lexer).parse();

      let obj = Evaluator::new().eval_program(prog);
      if let ObjectType::Integer(i) = obj {
        assert_eq!(i.value, test.1);
      } else {
        println!("{}", obj.inspect());
        assert_eq!(false, true)
      }
    }
  }
  #[test]
  fn test_string() {
    let tests = vec![      
      ("\"Hello World!\"", "Hello World!"),
      ("\"Hello\" + \" \" + \"World!\"", "Hello World!")
    ];
    for test in tests {
      let lexer = Lexer::new(&test.0);
      let prog = ast::Parser::new(lexer).parse();

      let obj = Evaluator::new().eval_program(prog);
      if let ObjectType::StringObj(i) = obj {
        assert_eq!(i.value, test.1);
      } else {
        println!("{}", obj.inspect());
        assert_eq!(false, true)
      }
    }
  }
  #[test]
  fn test_len() {
    let tests = vec![      
      ("len(\"\")", 0),
      ("len(\"four\")", 4),
      ("len(\"fourfourfourfour\")", 16)
    ];
    for test in tests {
      let lexer = Lexer::new(&test.0);
      let prog = ast::Parser::new(lexer).parse();

      let obj = Evaluator::new().eval_program(prog);
      if let ObjectType::Integer(i) = obj {
        assert_eq!(i.value, test.1);
      } else {
        println!("{}", obj.inspect());
        assert_eq!(false, true)
      }
    }
  }
}
