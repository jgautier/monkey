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
  Integer(Integer),
  StringObj(StringObj),
  Boolean(Boolean)
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
      HashKey::StringObj(_) => {
        "STRING".to_string()
      }
    }
  }
  fn inspect(&self) -> String {
    match self {
      HashKey::Integer(i) => {
        format!("{}", i.value)
      }
      HashKey::Boolean(b) => {
        format!("{}", b.value)
      }
      HashKey::StringObj(string) => {
        string.value.to_string()
      }
    }
  }
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
  Array(Array),
  Hash(Hash),
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
        let params = func.params.clone().into_iter().map(|p| p.identifier).collect::<Vec<String>>().join(",");
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
      ObjectType::Array(arr) => {
        let els = arr.elements.clone().into_iter().map(|el| el.inspect()).collect::<Vec<String>>().join(",");
        format!("[{}]", els)
      }
      ObjectType::Hash(hash) => {
        let pairs = hash.pairs.clone().into_iter().map(|pair| format!("{}: {}", pair.0.inspect(), pair.1.inspect())).collect::<Vec<String>>().join(", ");
        format!("{{{}}}", pairs)
      }
    }
  }
}
#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub struct Integer {
  value: i64
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct StringObj {
  value: String
}


#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub struct Boolean {
  value: bool
}
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Null {}

#[derive(Debug, Clone)]
pub struct Function {
  params: Vec<ast::Identifier>,
  body: ast::BlockStatement,
  env: Rc<RefCell<Environment>>
}

#[derive(Debug, Clone)]
pub struct Array {
  elements: Vec<ObjectType>
}

#[derive(Debug, Clone)]
pub struct Hash {
  pairs: HashMap<HashKey, ObjectType>
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

#[derive(Debug, Clone, PartialEq, Eq)]
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
        match &args[0] {
          ObjectType::StringObj(string) => {
            ObjectType::Integer(Integer { value: string.value.len() as i64 })
          },
          ObjectType::Array(arr) => {
            ObjectType::Integer(Integer { value: arr.elements.len() as i64 })
          },
          _=> {
            ObjectType::Error(format!("Expected a String or Array got a {}", args[0].object_type()))
          }
        }
      }
    }));
    built_ins.insert("first".to_string(), ObjectType::BuiltIn(BuiltIn {
      function: |args: Vec<ObjectType>| -> ObjectType {
        if args.len() > 1 {
          return ObjectType::Error(format!("Expected 1 argument got {}", args.len()));
        }
        match &args[0] {
          ObjectType::Array(arr) => {
            arr.elements[0].clone()
          },
          _=> {
            ObjectType::Error(format!("Expected a Array got a {}", args[0].object_type()))
          }
        }
      }
    }));
    built_ins.insert("last".to_string(), ObjectType::BuiltIn(BuiltIn {
      function: |args: Vec<ObjectType>| -> ObjectType {
        if args.len() > 1 {
          return ObjectType::Error(format!("Expected 1 argument got {}", args.len()));
        }
        match &args[0] {
          ObjectType::Array(arr) => {
            arr.elements.last().unwrap_or_else(|| &ObjectType::Null(Null{})).clone()
          },
          _=> {
            ObjectType::Error(format!("Expected a Array got a {}", args[0].object_type()))
          }
        }
      }
    }));
    built_ins.insert("rest".to_string(), ObjectType::BuiltIn(BuiltIn {
      function: |args: Vec<ObjectType>| -> ObjectType {
        if args.len() > 1 {
          return ObjectType::Error(format!("Expected 1 argument got {}", args.len()));
        }
        match &args[0] {
          ObjectType::Array(arr) => {
            if let Some((_, elements)) = arr.elements.split_first() {
              ObjectType::Array(Array {elements: elements.to_vec()})
            } else {
              ObjectType::Null(Null{})
            }
          },
          _=> {
            ObjectType::Error(format!("Expected a Array got a {}", args[0].object_type()))
          }
        }
      }
    }));
    built_ins.insert("push".to_string(), ObjectType::BuiltIn(BuiltIn {
      function: |args: Vec<ObjectType>| -> ObjectType {
        if args.len() > 2 {
          return ObjectType::Error(format!("Expected 2 argument got {}", args.len()));
        }
        match &args[0] {
          ObjectType::Array(arr) => {
            let mut new_arr = arr.elements.to_vec();
            new_arr.push(args[1].clone());
            ObjectType::Array(Array {elements: new_arr})
          },
          _=> {
            ObjectType::Error(format!("Expected a Array got a {}", args[0].object_type()))
          }
        }
      }
    }));
    built_ins.insert("puts".to_string(), ObjectType::BuiltIn(BuiltIn {
      function: |args: Vec<ObjectType>| -> ObjectType {
        for arg in args {
          println!("{}", arg.inspect())
        }
        ObjectType::Null(Null {})
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
      ast::Expression::Index(expr) => {
        let left = self.eval_expression(*expr.left, env);
        if let ObjectType::Error(_) = left {
          return left
        }
        let index = self.eval_expression(*expr.index, env);
        if let ObjectType::Error(_) = index {
          return index
        }
        match (&left, &index) {
          (ObjectType::Array(left), ObjectType::Integer(index)) => {
            match left.elements.get(index.value as usize) {
              Some(obj) => {
                obj.clone()
              },
              None => {
                ObjectType::Null(Null {})
              }
            }
          },
          (ObjectType::Hash(left), ObjectType::Integer(i)) => {
            match left.pairs.get(&HashKey::Integer(Integer { value: i.value })) {
              Some(obj) => {
                obj.clone()
              },
              None => {
                ObjectType::Null(Null {})
              }
            }
          },
          (ObjectType::Hash(left), ObjectType::StringObj(str_obj)) => {
            match left.pairs.get(&HashKey::StringObj(StringObj { value: str_obj.value.to_string() })) {
              Some(obj) => {
                obj.clone()
              },
              None => {
                ObjectType::Null(Null {})
              }
            }
          },
          (ObjectType::Hash(left), ObjectType::Boolean(boolean)) => {
            match left.pairs.get(&HashKey::Boolean(Boolean { value: boolean.value })) {
              Some(obj) => {
                obj.clone()
              },
              None => {
                ObjectType::Null(Null {})
              }
            }
          },
          _ => {
            ObjectType::Error(format!("Expecting ARRAY and INTEGER found {} and {}", left.object_type(), index.object_type()))
          }
        }
      }
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
              ObjectType::Array(arr) => {
                ObjectType::Array(Array { elements: arr.elements })
              }
              ObjectType::Hash(hash) => {
                ObjectType::Hash(Hash { pairs: hash.pairs })
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
      },
      ast::Expression::ArrayLiteral(arr) => {
        let mut elements = Vec::new();
        for el in arr.elements {
          let res = self.eval_expression(el, env);
          if let ObjectType::Error(_) = res {
            return res
          }
          elements.push(res);
        }
        ObjectType::Array(Array { elements })
      },
      ast::Expression::HashLiteral(hash) => {
        let mut pairs = HashMap::new();
        for pair in hash.pairs.into_iter() {
          let key = self.eval_expression(pair.0, env);
          let hash_key = match key {
            ObjectType::StringObj(str_obj) => {
              HashKey::StringObj(str_obj)
            }
            ObjectType::Integer(int) => {
              HashKey::Integer(int)
            }
            ObjectType::Boolean(boolean) => {
              HashKey::Boolean(boolean)
            }
            ObjectType::Error(_) => {
              return key.clone()
            }
            _ => {
              return ObjectType::Error("Wrong type for hash key, must be string, integer or boolean".to_string())
            }
          };
          let value = self.eval_expression(pair.1, env);
          if let ObjectType::Error(_) = value {
            return value
          }
          pairs.insert(hash_key, value);
        }
        ObjectType::Hash(Hash{ pairs })
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
        self.eval_expression(expr, env)
      },
      ast::StatementType::Return(value) => {
        let ret = self.eval_expression(value, env);
        if let ObjectType::Error(_) = ret {
          return ret
        }
        ObjectType::Return(Box::new(ret))
      },
      ast::StatementType::Let{ identifier, value} => {
        let ret = self.eval_expression(value, env);
        env.borrow_mut().set(identifier, &ret);
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
      ("len(\"fourfourfourfour\")", 16),
      ("len([1,2,3,4])", 4)
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
  fn test_array_literal() {
    let tests = vec![      
      ("[1, 2 * 2,3 + 3]", vec!["1", "4", "6"]),
    ];
    for test in tests {
      let lexer = Lexer::new(&test.0);
      let prog = ast::Parser::new(lexer).parse();

      let obj = Evaluator::new().eval_program(prog);
      if let ObjectType::Array(arr) = obj {
        for (i, num) in arr.elements.iter().enumerate() {
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
        assert_eq!(i.value, test.1);
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
        assert_eq!(i.value, test.1);
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
        assert_eq!(i.value, test.1);
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
        match (arr.elements[0].clone(), arr.elements[1].clone()) { 
          (ObjectType::Integer(i1), ObjectType::Integer(i2)) => {
            assert_eq!(i1.value, test.1[0]);
            assert_eq!(i2.value, test.1[1]);
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
        match (arr.elements[0].clone(), arr.elements[1].clone()) { 
          (ObjectType::Integer(i1), ObjectType::Integer(i2)) => {
            assert_eq!(i1.value, test.1[0]);
            assert_eq!(i2.value, test.1[1]);
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
        let value = hash.pairs.get(&HashKey::StringObj(StringObj{ value: test.0.to_string()})).unwrap();
        if let ObjectType::Integer(i) = value {
          assert_eq!(i.value, test.1)
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
        let value = hash.pairs.get(&HashKey::Integer(Integer{ value: test.0})).unwrap();
        if let ObjectType::Integer(i) = value {
          assert_eq!(i.value, test.1)
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
        let value = hash.pairs.get(&HashKey::Boolean(Boolean{ value: test.0})).unwrap();
        if let ObjectType::Integer(i) = value {
          assert_eq!(i.value, test.1)
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
        assert_eq!(i.value, test.1);     
      } else {
        println!("{}", obj.inspect());
        assert_eq!(false, true)
      }
    }
  }
}
