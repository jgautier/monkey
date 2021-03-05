use crate::code::Instructions;
use crate::ast::BlockStatement;
use crate::ast::Node;
use std::rc::Rc;
use std::collections::HashMap;
use std::cell::RefCell;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum HashKey {
  Integer(i64),
  String(String),
  Boolean(bool)
}

impl HashKey {
  pub fn inspect(&self) -> String {
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
  pub fn from_object(obj: &Object) -> Option<HashKey> {
    match obj {
      Object::String(val) => {
        Some(HashKey::String(val.to_string()))
      },
      Object::Integer(val) => {
        Some(HashKey::Integer(*val))
      },
      Object::Boolean(val) => {
        Some(HashKey::Boolean(*val))
      },
      _ => None
    }
  }
}

#[derive(Debug, Clone)]
pub enum Object {
  Integer(i64),
  String(String),
  Boolean(bool),
  Null,
  Return(Rc<Object>),
  Function{ params: Vec<String>, body: BlockStatement, env: Env },
  BuiltIn(fn(Vec<Rc<Object>>) -> Rc<Object>),
  Array(Vec<Rc<Object>>),
  Hash(HashMap<HashKey, Rc<Object>>),
  Error(String),
  CompiledFunction(Instructions, usize, usize)
}

impl Object {
  pub fn object_type(&self) -> String {
    match self {
      Object::Integer(_) => {
        "INTEGER".to_string()
      },
      Object::Boolean(_) => {
        "BOOLEAN".to_string()
      },
      Object::Null => {
        "NULL".to_string()
      },
      Object::Return(_) => {
        "RETURN".to_string()
      },
      Object::Error(_) => {
        "ERROR".to_string()
      },
      Object::Function{..} => {
        "FUNCTION".to_string()
      },
      Object::String(_) => {
        "STRING".to_string()
      },
      Object::BuiltIn(_) => {
        "BUILTIN".to_string()
      },
      Object::Array(_) => {
        "ARRAY".to_string()
      },
      Object::Hash(_) => {
        "HASH".to_string()
      },
      Object::CompiledFunction(_,_,_) => {
        "COMPILED_FUNCTION".to_string()
      }
    }
  }
  pub fn inspect(&self) -> String {
    match self {
      Object::Integer(i) => {
        format!("{}", i)
      },
      Object::Boolean(b) => {
        format!("{}", b)
      },
      Object::Null => {
        "null".to_string()
      },
      Object::Return(obj) => {
        obj.inspect()
      },
      Object::Error(err_str) => {
        err_str.to_string()
      },
      Object::String(string) => {
        string.to_string()
      }
      Object::Function{ params, body, ..} => {
        let params = params.join(",");
        let strings = vec![
          format!("fn({}) {{", params), body.to_string(), "}}".to_string()
        ];
        strings.join("\n")
      },
      Object::BuiltIn(_) => {
        "builtin function".to_string()
      }
      Object::Array(arr) => {
        let els = arr.iter().map(|el| el.inspect()).collect::<Vec<String>>().join(",");
        format!("[{}]", els)
      }
      Object::Hash(hash) => {
        let pairs = hash.iter().map(|pair| format!("{}: {}", pair.0.inspect(), pair.1.inspect())).collect::<Vec<String>>().join(", ");
        format!("{{{}}}", pairs)
      },
      Object::CompiledFunction(cf, _, _) => {
        format!("{:?}", cf)
      }
    }
  }
}

#[derive(Debug)]
pub struct Environment {
  store: HashMap<String, Rc<Object>>,
  outer: Option<Env>
}

impl Environment {
  pub fn new(outer: Option<Env>) -> Self {
    Self {
      store: HashMap::new(),
      outer
    }
  }
  pub fn get(&self, var_name: String) -> Option<Rc<Object>> {
    let val = self.store.get(&var_name);
    if let Some(value) = val {
      return Some(value.clone());
    }
    if let Some(env) = &self.outer {
       return env.borrow().get(var_name);
    }
    None
  }
  pub fn set(&mut self, var_name: String, value: Rc<Object>) {
    self.store.insert(var_name, value);
  }
}

pub type Env = Rc<RefCell<Environment>>;

pub fn get_built_in_vec() -> Vec<(String, Object)> {
  vec![
    ("len".to_string(), Object::BuiltIn(|args: Vec<Rc<Object>>| -> Rc<Object> {
      if args.len() > 1 {
        return Rc::new(Object::Error(format!("Expected 1 argument got {}", args.len())));
      }
      match &*args[0] {
        Object::String(string) => {
          Rc::new(Object::Integer(string.len() as i64))
        },
        Object::Array(arr) => {
          Rc::new(Object::Integer(arr.len() as i64))
        },
        _=> {
          Rc::new(Object::Error(format!("Expected a String or Array got a {}", args[0].object_type())))
        }
      }
    })),
    ("first".to_string(), Object::BuiltIn(|args: Vec<Rc<Object>>| -> Rc<Object> {
      if args.len() > 1 {
        return Rc::new(Object::Error(format!("Expected 1 argument got {}", args.len())))
      }
      match &*args[0] {
        Object::Array(arr) => {
          arr[0].clone()
        },
        _=> {
          Rc::new(Object::Error(format!("Expected a Array got a {}", args[0].object_type())))
        }
      }
    })),
    ("last".to_string(), Object::BuiltIn(|args: Vec<Rc<Object>>| -> Rc<Object> {
      if args.len() > 1 {
        return Rc::new(Object::Error(format!("Expected 1 argument got {}", args.len())));
      }
      match &*args[0] {
        Object::Array(arr) => {
          if let Some(obj) = arr.last() {
            return obj.clone()
          }
          Rc::new(Object::Null)
        },
        _=> {
          Rc::new(Object::Error(format!("Expected a Array got a {}", args[0].object_type())))
        }
      }
    })),
    ("rest".to_string(), Object::BuiltIn(|args: Vec<Rc<Object>>| -> Rc<Object> {
      if args.len() > 1 {
        return Rc::new(Object::Error(format!("Expected 1 argument got {}", args.len())));
      }
      match &*args[0] {
        Object::Array(arr) => {
          if let Some((_, elements)) = arr.split_first() {
            Rc::new(Object::Array(elements.to_vec()))
          } else {
            Rc::new(Object::Null)
          }
        },
        _=> {
          Rc::new(Object::Error(format!("Expected a Array got a {}", args[0].object_type())))
        }
      }
    })),
    ("push".to_string(), Object::BuiltIn(|args: Vec<Rc<Object>>| -> Rc<Object> {
      if args.len() > 2 {
        return Rc::new(Object::Error(format!("Expected 2 argument got {}", args.len())))
      }
      match &*args[0] {
        Object::Array(arr) => {
          let mut new_arr = arr.to_vec();
          new_arr.push(args[1].clone());
          Rc::new(Object::Array(new_arr))
        },
        _=> {
          Rc::new(Object::Error(format!("Expected a Array got a {}", args[0].object_type())))
        }
      }
    })),
    ("puts".to_string(), Object::BuiltIn(|args: Vec<Rc<Object>>| -> Rc<Object> {
      for arg in args {
        println!("{}", arg.inspect())
      }
      Rc::new(Object::Null)
    }))
  ]
}
pub fn get_built_in_map() -> HashMap<String, Object> {
  let mut built_in_map = HashMap::new();
  let built_ins = get_built_in_vec();
  for built_in in built_ins {
    built_in_map.insert(built_in.0, built_in.1);
  }
  built_in_map
}