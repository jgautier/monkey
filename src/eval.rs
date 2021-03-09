use crate::ast;
use crate::object::Object;
use crate::object::HashKey;
use crate::object::Env;
use crate::object::Environment;
use crate::object::get_built_in_map;
use std::rc::Rc;
use std::cell::RefCell;
use std::collections::HashMap;

pub struct Evaluator {
  env: Env,
  built_ins: HashMap<String, Object>,
  null: Rc<Object>
}

impl Evaluator {
  #[allow(clippy::new_without_default)]
  pub fn new() -> Evaluator {
    Self {
      env: Rc::new(RefCell::new(Environment::new(None))),
      built_ins: get_built_in_map(),
      null: Rc::new(Object::Null)
    }
  }
  pub fn eval_program(&self, prog: ast::Program) -> Rc<Object> {
    let mut result = self.null.clone();
    for stmt in prog.statements {
      result = self.eval_statement(&stmt, &self.env);
      match &*result {
        Object::Return(obj) => {
          return obj.clone()
        },
        Object::Error(_) => {
          return result
        }
        _=> {}
      }
    }
    result
  }
  pub fn error(&self, msg: &str) -> Rc<Object> {
    Rc::new(Object::Error(msg.to_string()))
  }
  fn eval_expression(&self, expr: &ast::Expression, environment: &Env) -> Rc<Object> {
    match expr {
      ast::Expression::Index{ left, index } => {
        self.eval_index_expression(left, index, environment)
      }
      ast::Expression::Infix{ operator, left, right } => {
        self.eval_infix_expression(&operator, left, right, environment)
      },
      ast::Expression::Prefix{ operator, right} => {
        self.eval_prefix_expression(&operator, right, environment)
      },
      ast::Expression::IntegerLiteral(int) => {
        Rc::new(Object::Integer(*int))
      },
      ast::Expression::Boolean(value) => {
        Rc::new(Object::Boolean(*value))
      },
      ast::Expression::If{ condition, consequence, alternative } => {
        self.eval_if_expression(condition, consequence, alternative, environment)
      },
      ast::Expression::Identifier(id) => {
        self.eval_identifier(id, environment)
      },
      ast::Expression::Call{ function, args } => {
        self.eval_call_expression(function, args, environment)
      }
      ast::Expression::Fn{ params, body , ..} => {
        Rc::new(Object::Function{ params: params.to_vec(), body: body.clone(), env: Rc::clone(environment) })
      },
      ast::Expression::StringLiteral(value) => {
        Rc::new(Object::String(value.to_string()))
      },
      ast::Expression::ArrayLiteral(els) => {
        self.eval_array_literal(els, environment)
      },
      ast::Expression::HashLiteral(hash) => {
        self.eval_hash_literal(hash, environment)
      }
    }
  }
  fn eval_index_expression (&self, left: &ast::Expression, index: &ast::Expression, environment: &Env) -> Rc<Object> {
    let left = self.eval_expression(&left, environment);
    if let Object::Error(_) = *left {
      return left
    }
    let index = self.eval_expression(&index, environment);
    if let Object::Error(_) = *index {
      return index
    }
    let ret = match (&*left, &*index) {
      (Object::Array(left), Object::Integer(index)) => {
        left.get(*index as usize)
      },
      (Object::Hash(left), Object::Integer(i)) => {
        left.get(&HashKey::Integer(*i))
      },
      (Object::Hash(left), Object::String(string)) => {
        left.get(&HashKey::String(string.to_string()))
      },
      (Object::Hash(left), Object::Boolean(boolean)) => {
        left.get(&HashKey::Boolean(*boolean))
      },
      _ => {
        return self.error(&format!("Expecting ARRAY and INTEGER found {} and {}", left.object_type(), index.object_type()));
      }
    };
    ret.unwrap_or(&self.null).clone()
  }
  fn eval_infix_expression(&self, operator: &str, left: &ast::Expression, right: &ast::Expression, environment: &Env) -> Rc<Object> {
    let left = self.eval_expression(&left, environment);
    if let Object::Error(_) = *left {
      return left
    }
    let right = self.eval_expression(&right, environment);
    if let Object::Error(_) = *right {
      return right
    }
    let ret = match (&*left, &*right) {
      (Object::Integer(i1), Object::Integer(i2)) => {
        eval_int_infix_expression(operator, *i1, *i2)
      },
      (Object::Boolean(b1), Object::Boolean(b2)) => {
        eval_bool_infix_expression(operator, *b1, *b2)
      },
      (Object::String(s1), Object::String(s2)) => {
        eval_string_infix_expression(operator, s1.to_string(), s2.to_string())
      }
      _ => {
        Object::Error(format!("type mismatch: {} {} {}", left.object_type(), operator, right.object_type()))
      }
    };
    Rc::new(ret)
  }
  fn eval_prefix_expression(&self, operator: &str, right: &ast::Expression, environment: &Env) -> Rc<Object> {
    let right = self.eval_expression(&right, environment);
    if let Object::Error(_) = *right {
      return right
    }
    Rc::new(match operator {
      "!" => {
        eval_bang_operator(right)
      }
      "-" => {
        eval_minus_operator(right)
      }
      _ => {
        Object::Error(format!("unknown operator: {}{}", operator, right.object_type()))
      }
    })
  }
  fn eval_identifier(&self, id: &str, environment: &Env) -> Rc<Object> {
    if let Some(obj) = environment.borrow().get(id.to_string()) {
      return obj
    }
    if let Some(obj) = self.built_ins.get(&id.to_string()) {
      return Rc::new(obj.clone())
    }
    Rc::new(Object::Error(format!("identifier not found: {}", id)))
  }
  fn eval_call_expression(&self, function: &ast::Expression, args: &[ast::Expression], environment: &Env) -> Rc<Object> {
    let res = self.eval_expression(&function, environment);
    if let Object::Error(_) = &*res {
      return res.clone()
    }
    let mut call_args = Vec::new();
    for arg in args {
      let res = self.eval_expression(arg, environment);
      if let Object::Error(_) = *res {
        return res
      }
      call_args.push(res);
    }
    match &*res {
      Object::Function{ params, body, env } => {
        let mut func_env = Environment::new(Some(Rc::clone(&env)));
        for (idx, param) in params.iter().enumerate() {
          func_env.set(param.to_string(), call_args[idx].clone());
        }
        let func_res = self.eval_block_statements(&body.statements, &Rc::new(RefCell::new(func_env)));
        if let Object::Return(val) = &*func_res {
          return val.clone();
        }
        func_res
      },
      Object::BuiltIn(built_in) => {
        (built_in)(call_args)
      },
      _ => {
        Rc::new(Object::Error("Not a Function".to_string()))
      }
    }
  }
  fn eval_array_literal(&self, els: &[ast::Expression], environment: &Env) -> Rc<Object> {
    let mut elements = Vec::new();
    for el in els {
      let res = self.eval_expression(el, environment);
      if let Object::Error(_) = *res {
        return res
      }
      elements.push(res);
    }
    Rc::new(Object::Array(elements))
  }
  fn eval_hash_literal(&self, hash: &ast::HashLiteral, environment: &Env) -> Rc<Object> {
    let mut pairs = HashMap::new();
    for pair in hash.pairs.iter() {
      let key = self.eval_expression(&pair.0, environment);
      let hash_key = match &*key {
        Object::String(string) => {
          HashKey::String(string.to_string())
        }
        Object::Integer(int) => {
          HashKey::Integer(*int)
        }
        Object::Boolean(boolean) => {
          HashKey::Boolean(*boolean)
        }
        _ => {
          return Rc::new(Object::Error("Wrong type for hash key, must be string, integer or boolean".to_string()))
        }
      };
      let value = self.eval_expression(&pair.1, environment);
      if let Object::Error(_) = *value {
        return value
      }
      pairs.insert(hash_key, value);
    }
    Rc::new(Object::Hash(pairs))
  }
  fn eval_if_expression(&self, condition: &ast::Expression, consequence: &ast::BlockStatement, alternative: &Option<ast::BlockStatement>, env: &Env) -> Rc<Object> {
    let condition = self.eval_expression(&condition, env);
    if let Object::Error(_) = *condition {
      return condition
    }
    if is_truthy(condition) {
      self.eval_block_statements(&consequence.statements, env)
    } else {
      if let Some(block) = alternative {
        return self.eval_block_statements(&block.statements, env)
      }
      self.null.clone()
    }
  }

  fn eval_statement(&self, node: &ast::Statement, env: &Env) -> Rc<Object> {
    match node {
      ast::Statement::Expression(expr) => {
        self.eval_expression(expr, env)
      },
      ast::Statement::Return(value) => {
        let ret = self.eval_expression(value, env);
        if let Object::Error(_) = *ret {
          return ret
        }
        Rc::new(Object::Return(ret))
      },
      ast::Statement::Let{ identifier, value} => {
        let ret = self.eval_expression(value, env);
        env.borrow_mut().set(identifier.to_string(), ret);
        self.null.clone()
      }
    }
  }
  pub fn eval_block_statements(&self, stmts: &[ast::Statement], env: &Env) -> Rc<Object> {
    let mut result = self.null.clone();
    for stmt in stmts {
      result = self.eval_statement(stmt, env);
      if let Object::Return(_) | Object::Error(_) = *result {
          return result
      }
    }
    result
  }
}

fn eval_bang_operator(right: Rc<Object>) -> Object {
  let val = match *right {
    Object::Boolean(b) => {
      !b
    },
    Object::Null => true,
    _ => false
  };
  Object::Boolean(val)
}

fn eval_minus_operator(right: Rc<Object>) -> Object {
  if let Object::Integer(i) = *right {
    return Object::Integer(-i)
  }
  Object::Error(format!("unknown operator: -{}", right.object_type()))
}

fn eval_string_infix_expression(operator: &str, left: String, right: String) -> Object {
  match operator {
    "+" => {
      Object::String(format!("{}{}", left, right))
    },
    _ => {
       Object::Error(format!("unknown operator: {} for strings.", operator))
    }
  }
}

fn eval_int_infix_expression(operator: &str, left: i64, right: i64) -> Object {
  match operator {
    "+" => {
      Object::Integer(left + right)
    },
    "-" => {
      Object::Integer(left - right)
    },
    "*" => {
      Object::Integer(left * right)
    },
    "/" => {
      Object::Integer(left / right)
    },
    "<" => {
      Object::Boolean(left < right)
    },
    ">" => {
      Object::Boolean(left > right)
    },
    "==" => {
      Object::Boolean(left == right)
    },
    "!=" => {
      Object::Boolean(left != right)
    },
    _ => {
       Object::Error(format!("unknown operator: {} for integers.", operator))
    }
  }
}

fn eval_bool_infix_expression(operator: &str, left: bool, right: bool) -> Object {
  match operator {
    "==" => {
      Object::Boolean(left == right)
    },
    "!=" => {
      Object::Boolean(left != right)
    },
    _ => {
      Object::Error(format!("unknown operator: {} for booleans", operator))
    }
  }
}

fn is_truthy(val: Rc<Object>) -> bool {
  match &*val {
    Object::Null => {
      false
    },
    Object::Boolean(b) => {
      *b
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
    if let Object::Integer(i) = *obj {
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
    if let Object::Boolean(i) = *obj {
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
      if let Object::Boolean(i) = *obj {
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
      if let Object::Integer(i) = *obj {
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
      if let Object::Integer(i) = *obj {
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
      if let Object::Boolean(b) = *obj {
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
      if let Object::Boolean(b) = *obj {
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
      if let Object::Integer(i) = *obj {
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
      if let Object::Null = *obj {
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
      if let Object::Integer(i) = *obj {
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
      if let Object::Error(err_str) = &*obj {
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
      if let Object::Integer(i) = *obj {
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
      if let Object::Integer(i) = *obj {
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
      if let Object::Integer(i) = *obj {
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
      if let Object::String(i) = &*obj {
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
      if let Object::Integer(i) = *obj {
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
      if let Object::Array(arr) = &*obj {
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
      if let Object::Integer(i) = *obj {
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
      if let Object::Integer(i) = *obj {
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
      if let Object::Integer(i) = *obj {
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
      if let Object::Array(arr) = &*obj {
        match (&*arr[0].clone(), &*arr[1].clone()) {
          (Object::Integer(i1), Object::Integer(i2)) => {
            assert_eq!(*i1, test.1[0]);
            assert_eq!(*i2, test.1[1]);
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
      if let Object::Array(arr) = &*obj {
        match (&*arr[0].clone(), &*arr[1].clone()) {
          (Object::Integer(i1), Object::Integer(i2)) => {
            assert_eq!(*i1, test.1[0]);
            assert_eq!(*i2, test.1[1]);
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
    if let Object::Hash(hash) = &*obj {
      for test in test_values {
        let value = hash.get(&HashKey::String(test.0.to_string())).unwrap();
        if let Object::Integer(i) = &*value.clone() {
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
    if let Object::Hash(hash) = &*obj {
      for test in test_values {
        let value = hash.get(&HashKey::Integer(test.0)).unwrap();
        if let Object::Integer(i) = &*value.clone() {
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
    if let Object::Hash(hash) = &*obj {
      for test in test_values {
        let value = hash.get(&HashKey::Boolean(test.0)).unwrap();
        if let Object::Integer(i) = &*value.clone() {
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
      if let Object::Integer(i) = *obj {
        assert_eq!(i, test.1);
      } else {
        println!("{}", obj.inspect());
        assert_eq!(false, true)
      }
    }
  }
}
