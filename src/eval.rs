use crate::ast;
use crate::ast::Node;
use std::rc::Rc;
use std::cell::RefCell;
use std::collections::HashMap;

pub trait Object {
  fn object_type(&self) -> String;
  fn inspect(&self) -> String;
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum HashKey {
  Integer(i64),
  String(String),
  Boolean(bool)
}

impl Object for HashKey {
  fn object_type(&self) -> String {
    match self {
      HashKey::Integer(_) => {
        "INTEGER".to_string()
      },
      HashKey::Boolean(_) => {
        "BOOLEAN".to_string()
      },
      HashKey::String(_) => {
        "STRING".to_string()
      }
    }
  }
  fn inspect(&self) -> String {
    match self {
      HashKey::Integer(i) => {
        format!("{}", i)
      }
      HashKey::Boolean(b) => {
        format!("{}", b)
      }
      HashKey::String(string) => {
        string.to_string()
      }
    }
  }
}

#[derive(Debug, Clone)]
pub enum ObjectType {
  Integer(i64),
  String(String),
  Boolean(bool),
  Null,
  Return(Box<ObjectType>),
  Function{ params: Vec<String>, body: ast::BlockStatement, env: Rc<RefCell<Environment>> },
  BuiltIn(fn(Vec<ObjectType>) -> ObjectType),
  Array(Vec<ObjectType>),
  Hash(HashMap<HashKey, ObjectType>),
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
      ObjectType::Null => {
        "NULL".to_string()
      },
      ObjectType::Return(_) => {
        "RETURN".to_string()
      },
      ObjectType::Error(_) => {
        "ERROR".to_string()
      },
      ObjectType::Function{..} => {
        "FUNCTION".to_string()
      },
      ObjectType::String(_) => {
        "STRING".to_string()
      },
      ObjectType::BuiltIn(_) => {
        "BUILTIN".to_string()
      },
      ObjectType::Array(_) => {
        "ARRAY".to_string()
      },
      ObjectType::Hash(_) => {
        "HASH".to_string()
      }
    }
  }
  fn inspect(&self) -> String {
    match self {
      ObjectType::Integer(i) => {
        format!("{}", i)
      },
      ObjectType::Boolean(b) => {
        format!("{}", b)
      },
      ObjectType::Null => {
        "null".to_string()
      },
      ObjectType::Return(obj) => {
        obj.inspect()
      },
      ObjectType::Error(err_str) => {
        err_str.to_string()
      },
      ObjectType::String(string) => {
        string.to_string()
      }
      ObjectType::Function{ params, body, ..} => {
        let params = params.join(",");
        let strings = vec![
          format!("fn({}) {{", params), body.to_string(), "}}".to_string()
        ];
        strings.join("\n")
      },
      ObjectType::BuiltIn(_) => {
        "builtin function".to_string()
      }
      ObjectType::Array(arr) => {
        let els = arr.clone().into_iter().map(|el| el.inspect()).collect::<Vec<String>>().join(",");
        format!("[{}]", els)
      }
      ObjectType::Hash(hash) => {
        let pairs = hash.clone().into_iter().map(|pair| format!("{}: {}", pair.0.inspect(), pair.1.inspect())).collect::<Vec<String>>().join(", ");
        format!("{{{}}}", pairs)
      }
    }
  }
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

pub struct Evaluator {
  env: Rc<RefCell<Environment>>,
  built_ins: HashMap<String, ObjectType>
}

impl Evaluator {
  pub fn new() -> Evaluator {
    let mut built_ins = HashMap::new();
    built_ins.insert("len".to_string(), ObjectType::BuiltIn(|args: Vec<ObjectType>| -> ObjectType {
      if args.len() > 1 {
        return ObjectType::Error(format!("Expected 1 argument got {}", args.len()));
      }
      match &args[0] {
        ObjectType::String(string) => {
          ObjectType::Integer(string.len() as i64)
        },
        ObjectType::Array(arr) => {
          ObjectType::Integer(arr.len() as i64)
        },
        _=> {
          ObjectType::Error(format!("Expected a String or Array got a {}", args[0].object_type()))
        }
      }
    }));
    built_ins.insert("first".to_string(), ObjectType::BuiltIn(|args: Vec<ObjectType>| -> ObjectType {
      if args.len() > 1 {
        return ObjectType::Error(format!("Expected 1 argument got {}", args.len()));
      }
      match &args[0] {
        ObjectType::Array(arr) => {
          arr[0].clone()
        },
        _=> {
          ObjectType::Error(format!("Expected a Array got a {}", args[0].object_type()))
        }
      }
    }));
    built_ins.insert("last".to_string(), ObjectType::BuiltIn(|args: Vec<ObjectType>| -> ObjectType {
      if args.len() > 1 {
        return ObjectType::Error(format!("Expected 1 argument got {}", args.len()));
      }
      match &args[0] {
        ObjectType::Array(arr) => {
          if let Some(obj) = arr.last() {
            return obj.clone()
          }
          ObjectType::Null
        },
        _=> {
          ObjectType::Error(format!("Expected a Array got a {}", args[0].object_type()))
        }
      }
    }));
    built_ins.insert("rest".to_string(), ObjectType::BuiltIn(|args: Vec<ObjectType>| -> ObjectType {
      if args.len() > 1 {
        return ObjectType::Error(format!("Expected 1 argument got {}", args.len()));
      }
      match &args[0] {
        ObjectType::Array(arr) => {
          if let Some((_, elements)) = arr.split_first() {
            ObjectType::Array(elements.to_vec())
          } else {
            ObjectType::Null
          }
        },
        _=> {
          ObjectType::Error(format!("Expected a Array got a {}", args[0].object_type()))
        }
      }
    }));
    built_ins.insert("push".to_string(), ObjectType::BuiltIn(|args: Vec<ObjectType>| -> ObjectType {
      if args.len() > 2 {
        return ObjectType::Error(format!("Expected 2 argument got {}", args.len()));
      }
      match &args[0] {
        ObjectType::Array(arr) => {
          let mut new_arr = arr.to_vec();
          new_arr.push(args[1].clone());
          ObjectType::Array(new_arr)
        },
        _=> {
          ObjectType::Error(format!("Expected a Array got a {}", args[0].object_type()))
        }
      }
    }));
    built_ins.insert("puts".to_string(), ObjectType::BuiltIn(|args: Vec<ObjectType>| -> ObjectType {
      for arg in args {
        println!("{}", arg.inspect())
      }
      ObjectType::Null
    }));
    Self {
      env: Rc::new(RefCell::new(Environment::new(None))),
      built_ins
    }
  }
  pub fn eval_program(&self, prog: ast::Program) -> ObjectType {
    let mut result = ObjectType::Null;
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
  fn eval_expression(&self, expr: ast::Expression, environment: &Rc<RefCell<Environment>>) -> ObjectType {
    match expr {
      ast::Expression::Index{ left, index } => {
        let left = self.eval_expression(left.as_ref().clone(), environment);
        if let ObjectType::Error(_) = left {
          return left
        }
        let index = self.eval_expression(index.as_ref().clone(), environment);
        if let ObjectType::Error(_) = index {
          return index
        }
        match (&left, &index) {
          (ObjectType::Array(left), ObjectType::Integer(index)) => {
            if let Some(obj) = left.get(*index as usize) {
              return obj.clone()
            }
          },
          (ObjectType::Hash(left), ObjectType::Integer(i)) => {
            if let Some(obj) = left.get(&HashKey::Integer(*i)) {
              return obj.clone()
            }
          },
          (ObjectType::Hash(left), ObjectType::String(string)) => {
            if let Some(obj) = left.get(&HashKey::String(string.to_string())) {
              return obj.clone()
            }
          },
          (ObjectType::Hash(left), ObjectType::Boolean(boolean)) => {
            if let Some(obj) = left.get(&HashKey::Boolean(*boolean)) {
              return obj.clone()
            }
          },
          _ => {
            return ObjectType::Error(format!("Expecting ARRAY and INTEGER found {} and {}", left.object_type(), index.object_type()))
          }
        };
        ObjectType::Null
      }
      ast::Expression::Infix{ operator, left, right } => {
        let left = self.eval_expression(left.as_ref().clone(), environment);
        if let ObjectType::Error(_) = left {
          return left
        }
        let right = self.eval_expression(right.as_ref().clone(), environment);
        if let ObjectType::Error(_) = right {
          return right
        }
        eval_infix_expression(&operator, left, right)
      },
      ast::Expression::Prefix{ operator, right} => {
        let right = self.eval_expression(right.as_ref().clone(), environment);
        if let ObjectType::Error(_) = right {
          return right
        }
        eval_prefix_expression(&operator, right)
      },
      ast::Expression::IntegerLiteral(int) => {
        ObjectType::Integer(int)
      },
      ast::Expression::Boolean(value) => {
        ObjectType::Boolean(value)
      },
      ast::Expression::If{ condition, consequence, alternative } => {
        self.eval_if_expression(condition, consequence, alternative, environment)
      },
      ast::Expression::Identifier(id) => {
        if let Some(obj) = environment.borrow().get(id.to_string()) {
          return obj
        }
        if let Some(obj) = self.built_ins.get(&id.to_string()) {
          return obj.clone()
        }
        ObjectType::Error(format!("identifier not found: {}", id))
      },
      ast::Expression::Call{ function, args } => {
        let res = self.eval_expression(function.as_ref().clone(), environment);
        match res {
          ObjectType::Error(_) => {
            res
          },
          ObjectType::Function{ params, body, env } => {
            let mut call_args = Vec::new();
            for arg in args {
              let res = self.eval_expression(arg, environment);
              if let ObjectType::Error(_) = res {
                return res
              }
              call_args.push(res);
            }
            let mut func_env = Environment::new(Some(Rc::clone(&env)));
            for (idx, param) in params.iter().enumerate() {
              func_env.set(param.to_string(), &call_args[idx]);
            }
            let func_res = self.eval_block_statements(body.statements, &Rc::new(RefCell::new(func_env)));
            if let ObjectType::Return(val) = func_res {
              return *val;
            }
            func_res
          },
          ObjectType::BuiltIn(built_in) => {
            let mut call_args = Vec::new();
            for arg in args {
              let res = self.eval_expression(arg, environment);
              if let ObjectType::Error(_) = res {
                return res
              }
              call_args.push(res);
            }
            (built_in)(call_args)
          },
          _ => {
            ObjectType::Error("Not a Function".to_string())
          }
        }
      }
      ast::Expression::Fn{ params, body } => {
        ObjectType::Function{ params, body, env: Rc::clone(environment) }
      },
      ast::Expression::StringLiteral(value) => {
        ObjectType::String(value)
      },
      ast::Expression::ArrayLiteral(els) => {
        let mut elements = Vec::new();
        for el in els {
          let res = self.eval_expression(el, environment);
          if let ObjectType::Error(_) = res {
            return res
          }
          elements.push(res);
        }
        ObjectType::Array(elements)
      },
      ast::Expression::HashLiteral(hash) => {
        let mut pairs = HashMap::new();
        for pair in hash.pairs.into_iter() {
          let key = self.eval_expression(pair.0, environment);
          let hash_key = match key {
            ObjectType::String(string) => {
              HashKey::String(string)
            }
            ObjectType::Integer(int) => {
              HashKey::Integer(int)
            }
            ObjectType::Boolean(boolean) => {
              HashKey::Boolean(boolean)
            }
            _ => {
              return ObjectType::Error("Wrong type for hash key, must be string, integer or boolean".to_string())
            }
          };
          let value = self.eval_expression(pair.1, environment);
          if let ObjectType::Error(_) = value {
            return value
          }
          pairs.insert(hash_key, value);
        }
        ObjectType::Hash(pairs)
      }
    }
  }
  fn eval_if_expression(&self, condition: Box<ast::Expression>, consequence: ast::BlockStatement, alternative: Option<ast::BlockStatement>, env: &Rc<RefCell<Environment>>) -> ObjectType {
    let condition = self.eval_expression(condition.as_ref().clone(), env);
    if let ObjectType::Error(_) = condition {
      return condition
    }
    if is_truthy(condition) {
      self.eval_block_statements(consequence.statements, env)
    } else {
      if let Some(block) = alternative {
        return self.eval_block_statements(block.statements, env)
      }
      ObjectType::Null
    }
  }

  fn eval_statement(&self, node: ast::Statement, env: &Rc<RefCell<Environment>>) -> ObjectType {
    match node {
      ast::Statement::Expression(expr) => {
        self.eval_expression(expr, env)
      },
      ast::Statement::Return(value) => {
        let ret = self.eval_expression(value, env);
        if let ObjectType::Error(_) = ret {
          return ret
        }
        ObjectType::Return(Box::new(ret))
      },
      ast::Statement::Let{ identifier, value} => {
        let ret = self.eval_expression(value, env);
        env.borrow_mut().set(identifier, &ret);
        ObjectType::Null
      }
    }
  }
  pub fn eval_block_statements(&self, stmts: Vec<ast::Statement>, env: &Rc<RefCell<Environment>>) -> ObjectType {
    let mut result = ObjectType::Null;
    for stmt in stmts {
      result = self.eval_statement(stmt, env);
      if let ObjectType::Return(_) | ObjectType::Error(_) = result {
          return result
      }
    }
    result
  }
}

fn eval_bang_operator(right: ObjectType) -> ObjectType {
  let val = match right {
    ObjectType::Boolean(b) => {
      !b
    },
    ObjectType::Null => true,
    _ => false
  };
  ObjectType::Boolean(val)
}

fn eval_minus_operator(right: ObjectType) -> ObjectType {
  if let ObjectType::Integer(i) = right {
    return ObjectType::Integer(-i)
  }
  ObjectType::Error(format!("unknown operator: -{}", right.object_type())) 
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

fn eval_string_infix_expression(operator: &str, left: String, right: String) -> ObjectType {
  match operator {
    "+" => {
      ObjectType::String(format!("{}{}", left, right))
    },
    _ => {
       ObjectType::Error(format!("unknown operator: {} for strings.", operator)) 
    }
  }
}

fn eval_int_infix_expression(operator: &str, left: i64, right: i64) -> ObjectType {
  match operator {
    "+" => {
      ObjectType::Integer(left + right)
    },
    "-" => {
      ObjectType::Integer(left - right)
    },
    "*" => {
      ObjectType::Integer(left * right)
    },
    "/" => {
      ObjectType::Integer(left / right)
    },
    "<" => {
      ObjectType::Boolean(left < right)
    },
    ">" => {
      ObjectType::Boolean(left > right)
    },
    "==" => {
      ObjectType::Boolean(left == right)
    },
    "!=" => {
      ObjectType::Boolean(left != right)
    },
    _ => {
       ObjectType::Error(format!("unknown operator: {} for integers.", operator)) 
    }
  }
}

fn eval_bool_infix_expression(operator: &str, left: bool, right: bool) -> ObjectType {
  match operator {
    "==" => {
      ObjectType::Boolean(left == right)
    },
    "!=" => {
      ObjectType::Boolean(left != right)
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
    (ObjectType::String(s1), ObjectType::String(s2)) => {
      eval_string_infix_expression(operator, s1, s2)
    }
    _ => {
      ObjectType::Error(format!("type mismatch: {} {} {}", left_type, operator, right_type)) 
    }
  }
}

fn is_truthy(val: ObjectType) -> bool {
  match val {
    ObjectType::Null => {
      false
    },
    ObjectType::Boolean(b) => {
      b
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
      assert_eq!(5, i);
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
      assert_eq!(true, i);
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
        assert_eq!(test.1, i);
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
        assert_eq!(test.1, i);
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
        assert_eq!(test.1, i);
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
        assert_eq!(test.1, b);
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
        assert_eq!(test.1, b);
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
        assert_eq!(test.1, i);
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
      if let ObjectType::Null = obj {
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
        assert_eq!(i, test.1);
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
        assert_eq!(i, test.1);
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
        assert_eq!(i, test.1);
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
        assert_eq!(i, test.1);
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
      if let ObjectType::String(i) = obj {
        assert_eq!(i, test.1);
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
      ("len(\"fourfourfourfour\")", 16),
      ("len([1,2,3,4])", 4)
    ];
    for test in tests {
      let lexer = Lexer::new(&test.0);
      let prog = ast::Parser::new(lexer).parse();

      let obj = Evaluator::new().eval_program(prog);
      if let ObjectType::Integer(i) = obj {
        assert_eq!(i, test.1);
      } else {
        println!("{}", obj.inspect());
        assert_eq!(false, true)
      }
    }
  }
  #[test]
  fn test_array_literal() {
    let tests = vec![      
      ("[1, 2 * 2,3 + 3]", vec!["1", "4", "6"]),
    ];
    for test in tests {
      let lexer = Lexer::new(&test.0);
      let prog = ast::Parser::new(lexer).parse();

      let obj = Evaluator::new().eval_program(prog);
      if let ObjectType::Array(arr) = obj {
        for (i, num) in arr.iter().enumerate() {
          assert_eq!(num.inspect(), test.1[i]);
        }
      } else {
        println!("{}", obj.inspect());
        assert_eq!(false, true)
      }
    }
  }
  #[test]
  fn test_array_index() {
    let tests = vec![      
      ("[1, 2, 3][0]", 1),
      ("[1, 2, 3][1]", 2),
      ("[1, 2, 3][2]", 3),
      ("let i = 0; [1][i];", 1),
      ("[1, 2, 3][1 + 1]", 3),
      ("let myArray = [1, 2, 3]; myArray[2];", 3),
      ("let myArray = [1, 2, 3]; myArray[0] + myArray[1] + myArray[2];", 6),
      ("let myArray = [1, 2, 3]; let i = myArray[0]; i", 1)
    ];
    for test in tests {
      let lexer = Lexer::new(&test.0);
      let prog = ast::Parser::new(lexer).parse();
      let obj = Evaluator::new().eval_program(prog);
      if let ObjectType::Integer(i) = obj {
        assert_eq!(i, test.1);
      } else {
        println!("{}", obj.inspect());
        assert_eq!(false, true)
      }
    }
  }
  #[test]
  fn test_first() {
    let tests = vec![      
      ("first([1])", 1)
    ];
    for test in tests {
      let lexer = Lexer::new(&test.0);
      let prog = ast::Parser::new(lexer).parse();

      let obj = Evaluator::new().eval_program(prog);
      if let ObjectType::Integer(i) = obj {
        assert_eq!(i, test.1);
      } else {
        println!("{}", obj.inspect());
        assert_eq!(false, true)
      }
    }
  }
  #[test]
  fn test_last() {
    let tests = vec![      
      ("last([1, 2])", 2)
    ];
    for test in tests {
      let lexer = Lexer::new(&test.0);
      let prog = ast::Parser::new(lexer).parse();

      let obj = Evaluator::new().eval_program(prog);
      if let ObjectType::Integer(i) = obj {
        assert_eq!(i, test.1);
      } else {
        println!("{}", obj.inspect());
        assert_eq!(false, true)
      }
    }
  }
  #[test]
  fn test_rest() {
    let tests = vec![      
      ("rest([1, 2, 3])", vec![2, 3])
    ];
    for test in tests {
      let lexer = Lexer::new(&test.0);
      let prog = ast::Parser::new(lexer).parse();

      let obj = Evaluator::new().eval_program(prog);
      if let ObjectType::Array(arr) = obj {
        match (arr[0].clone(), arr[1].clone()) { 
          (ObjectType::Integer(i1), ObjectType::Integer(i2)) => {
            assert_eq!(i1, test.1[0]);
            assert_eq!(i2, test.1[1]);
          },
          _=> {
            assert_eq!(false, true)
          }
        }
      } else {
        println!("{}", obj.inspect());
        assert_eq!(false, true)
      }
    }
  }
  #[test]
  fn test_push() {
    let tests = vec![      
      ("push([1], 2)", vec![1, 2])
    ];
    for test in tests {
      let lexer = Lexer::new(&test.0);
      let prog = ast::Parser::new(lexer).parse();
      let obj = Evaluator::new().eval_program(prog);
      if let ObjectType::Array(arr) = obj {
        match (arr[0].clone(), arr[1].clone()) { 
          (ObjectType::Integer(i1), ObjectType::Integer(i2)) => {
            assert_eq!(i1, test.1[0]);
            assert_eq!(i2, test.1[1]);
          },
          _=> {
            assert_eq!(false, true)
          }
        }
      } else {
        println!("{}", obj.inspect());
        assert_eq!(false, true)
      }
    }
  }
  #[test]
  fn test_string_hash_literal() {
    let input = "
      let two = \"two\";
      {
           \"one\": 10 - 9,
           two: 1 + 1,
           \"thr\" + \"ee\": 6 / 2,
      }";
    let test_values = vec![
      ("one", 1),
      ("two", 2),
      ("three", 3)
    ];
    let lexer = Lexer::new(input);
    let prog = ast::Parser::new(lexer).parse();
    let obj = Evaluator::new().eval_program(prog);
    if let ObjectType::Hash(hash) = obj {
      for test in test_values {
        let value = hash.get(&HashKey::String(test.0.to_string())).unwrap();
        if let ObjectType::Integer(i) = value {
          assert_eq!(*i, test.1)
        }
      }
    } else {
      println!("{}", obj.inspect());
      assert_eq!(false, true)
    }
  }
  #[test]
  fn test_int_hash_literal() {
    let input = "
      let two = 2;
      {
           1: 10 - 9,
           two: 1 + 1,
           3: 6 / 2,
      }";
    let test_values = vec![
      (1, 1),
      (2, 2),
      (3, 3)
    ];
    let lexer = Lexer::new(input);
    let prog = ast::Parser::new(lexer).parse();
    let obj = Evaluator::new().eval_program(prog);
    if let ObjectType::Hash(hash) = obj {
      for test in test_values {
        let value = hash.get(&HashKey::Integer(test.0)).unwrap();
        if let ObjectType::Integer(i) = value {
          assert_eq!(*i, test.1)
        }
      }
    } else {
      println!("{}", obj.inspect());
      assert_eq!(false, true)
    }
  }
  #[test]
  fn test_bool_hash_literal() {
    let input = "
      {
           true: 1,
           false: 2,
      }";
    let test_values = vec![
      (true, 1),
      (false, 2)
    ];
    let lexer = Lexer::new(input);
    let prog = ast::Parser::new(lexer).parse();
    let obj = Evaluator::new().eval_program(prog);
    if let ObjectType::Hash(hash) = obj {
      for test in test_values {
        let value = hash.get(&HashKey::Boolean(test.0)).unwrap();
        if let ObjectType::Integer(i) = value {
          assert_eq!(*i, test.1)
        }
      }
    } else {
      println!("{}", obj.inspect());
      assert_eq!(false, true)
    }
  }
  #[test]
  fn test_hash_index() {
    let tests = vec![      
      ("{\"foo\": 5}[\"foo\"]", 5),
      ("let five = { \"five\": 5 }; five[\"five\"]", 5)
    ];
    for test in tests {
      let lexer = Lexer::new(&test.0);
      let prog = ast::Parser::new(lexer).parse();
      let obj = Evaluator::new().eval_program(prog);
      if let ObjectType::Integer(i) = obj {
        assert_eq!(i, test.1);     
      } else {
        println!("{}", obj.inspect());
        assert_eq!(false, true)
      }
    }
  }
}
