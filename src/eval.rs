use crate::ast;
use crate::lexer;

pub trait Object {
  fn object_type(&self) -> String;
  fn inspect(&self) -> String;
}

#[derive(Debug)]
pub enum ObjectType {
  Integer(Integer),
  Boolean(Boolean),
  Null(Null)
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
      }
    }
  }
}
#[derive(Debug)]
struct Integer {
  value: i64
}

impl Object for Integer {
  fn object_type(&self) -> String {
    "INTEGER".to_string()
  }
  fn inspect(&self) -> String {
    format!("{}", self.value)
  }
}
#[derive(Debug)]
struct Boolean {
  value: bool
}
#[derive(Debug)]
struct Null {}

pub fn eval_program(prog: ast::Program) -> ObjectType {
  let mut result = ObjectType::Null(Null {});
  for stmt in prog.statements {
    result = eval_statement(stmt)
  }
  result
}
fn eval_bang_operator(right: ObjectType) -> ObjectType {
  let val = match right {
    ObjectType::Boolean(b) => {
      if b.value {
        false
      } else {
        true
      }
    },
    ObjectType::Null(_) => true,
    _ => false
  };
  ObjectType::Boolean(Boolean { value: val })
}
fn eval_minus_oeprator(right: ObjectType) -> ObjectType {
  match right {
    ObjectType::Integer(i) => ObjectType::Integer(Integer { value : -i.value }),
    _ => ObjectType::Null(Null {})
  }
}
fn eval_prefix_expression(operator: &str, right: ObjectType) -> ObjectType {
  match operator {
    "!" => {
      eval_bang_operator(right)  
    }
    "-" => {
      eval_minus_oeprator(right)
    }
    _ => {
      ObjectType::Null(Null {})
    }
  }
}
fn eval_int_infix_expression (operator: &str, left: Integer, right: Integer) -> ObjectType {
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
      ObjectType::Null(Null {})
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
      ObjectType::Null(Null {})
    }
  }
}
fn eval_infix_expression(operator: &str, left: ObjectType, right: ObjectType) -> ObjectType {
  match (left, right) {
    (ObjectType::Integer(i1), ObjectType::Integer(i2)) => {
      eval_int_infix_expression(operator, i1, i2)
    },
    (ObjectType::Boolean(b1), ObjectType::Boolean(b2)) => {
      eval_bool_infix_expression(operator, b1, b2)
    }
    _ => {
      ObjectType::Null(Null {})
    }
  }
}
fn eval_expression(expr: ast::Expression) -> ObjectType {
  match expr {
    ast::Expression::Infix(expr) => {
      let left = eval_expression(*expr.left);
      let right = eval_expression(*expr.right);
      eval_infix_expression(expr.operator, left, right)
    },
    ast::Expression::Prefix(expr) => {
      let right = eval_expression(*expr.right);
      eval_prefix_expression(expr.operator, right)
    },
    ast::Expression::IntegerLiteral(int) => {
      ObjectType::Integer(Integer { value: int.value })
    },
    ast::Expression::Boolean(boolean) => {
      ObjectType::Boolean(Boolean { value: boolean.value })
    },
    _ => {
      ObjectType::Null(Null {})
    }
  }
}

fn eval_statement(node: ast::StatementType) -> ObjectType {
  match node {
    ast::StatementType::Expression(expr) => {
      eval_expression(expr.expr)
    },
    _ => {
      ObjectType::Null(Null {})
    }
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  #[test]
  fn test_integer_literal() {
    let lexer = lexer::Lexer::new(&"5");
    let prog = ast::Parser::new(lexer).parse();
    let obj = eval_program(prog);
    if let ObjectType::Integer(i) = obj {
      assert_eq!(5, i.value);
    } else {
      assert_eq!(false, true)
    }
  }
  #[test]
  fn test_boolean_literal() {
    let lexer = lexer::Lexer::new(&"true");
    let prog = ast::Parser::new(lexer).parse();
    let obj = eval_program(prog);
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
      let lexer = lexer::Lexer::new(&test.0);
      let prog = ast::Parser::new(lexer).parse();
      let obj = eval_program(prog);
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
      let lexer = lexer::Lexer::new(&test.0);
      let prog = ast::Parser::new(lexer).parse();
      let obj = eval_program(prog);
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
      let lexer = lexer::Lexer::new(&test.0);
      let prog = ast::Parser::new(lexer).parse();
      let obj = eval_program(prog);
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
      let lexer = lexer::Lexer::new(&test.0);
      let prog = ast::Parser::new(lexer).parse();
      let obj = eval_program(prog);
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
      let lexer = lexer::Lexer::new(&test.0);
      let prog = ast::Parser::new(lexer).parse();
      let obj = eval_program(prog);
      if let ObjectType::Boolean(b) = obj {
        assert_eq!(test.1, b.value);
      } else {
        assert_eq!(false, true)
      }
    }
  }
}
