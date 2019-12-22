use std::convert::TryInto;
use chrono::NaiveDateTime;
use serde_json::Value;
use serde_json::Map;

use crate::gugugu::lang::rust::runtime::codec::{
  EncoderImpl, Encoding, DecoderImpl, Decoding,
};
use crate::gugugu::lang::rust::runtime::foreign::{
  ForeignEncodersImpl, ForeignDecodersImpl,
};
use crate::utils::err;

#[derive(Debug, Clone)]
pub struct JsonCodecImpl {
  pub date_time_format: String,
}

pub struct JsonCursor {
  top: Value,
  stack: Vec<Value>,
}

impl JsonCodecImpl {
  pub fn new_default() -> Self
  {
    JsonCodecImpl {
      date_time_format: String::from("%Y-%m-%dT%H:%M:%S"),
    }
  }
}

impl EncoderImpl for JsonCodecImpl {
  type Repr = Value;

  fn encode_value<A>(&self, a: &A) -> Result<Value, String> where A: Encoding
  {
    let mut s = JsonCursor { top: Value::Null, stack: Vec::new() };
    s = A::encode(s, a, self)?;
    Ok(s.top)
  }

  fn encode_record<A>
    ( &self
    , mut s: JsonCursor
    , n_fields: usize
    , a: &A
    , k: fn(&Self, JsonCursor, &A) -> Result<JsonCursor, String>
    ) -> Result<JsonCursor, String>
  {
    s.top = Value::Null;
    let obj = Value::Object(Map::with_capacity(n_fields));
    s.stack.push(obj);
    let mut s1 = k(self, s, a)?;
    match s1.stack.pop() {
      None          => err("Error"),
      Some(new_top) => {
        s1.top = new_top;
        Ok(s1)
      },
    }
  }

  fn encode_record_field<A>
    ( &self
    , s: JsonCursor
    , _: usize
    , name: &str
    , a: &A
    ) -> Result<JsonCursor, String>
  where A: Encoding
  {
    let mut s1 = A::encode(s, a, self)?;
    match s1.stack.last_mut() {
      Some(Value::Object(m)) => {
        m.insert(String::from(name), s1.top.clone());
        Ok(s1)
      },
      _                      => err("Error"),
    }
  }

  fn encode_enum<A>
    ( &self
    , s: JsonCursor
    , a: A
    , _: fn(A) -> i32
    , as_name: fn(A) -> &'static str
    ) -> Result<JsonCursor, String>
  {
    self.encode_string(s, &String::from(as_name(a)))
  }


  fn encode_maybe<A>
    ( &self
    , mut s: JsonCursor
    , v: &Option<A>
    ) -> Result<JsonCursor, String>
  where A: Encoding
  {
    match v {
      Some(v_) => A::encode(s, v_, self),
      None     => {
        s.top = Value::Null;
        Ok(s)
      }
    }
  }

  fn encode_list<A>
    ( &self
    , mut s: JsonCursor
    , v: &Vec<A>
    ) -> Result<JsonCursor, String>
  where A: Encoding
  {
    let mut top = Vec::with_capacity(v.len());
    for x in v.iter() {
      s = A::encode(s, x, self)?;
      top.push(s.top.clone());
    }
    s.top = Value::Array(top);
    Ok(s)
  }


  fn encode_unit
    ( &self
    , mut s: JsonCursor
    , _: &()
    ) -> Result<JsonCursor, String>
  {
    s.top = Value::Null;
    Ok(s)
  }

  fn encode_bool
    ( &self
    , mut s: JsonCursor
    , v: &bool
    ) -> Result<JsonCursor, String>
  {
    s.top = Value::Bool(*v);
    Ok(s)
  }

  fn encode_int32
    ( &self
    , mut s: JsonCursor
    , v: &i32
    ) -> Result<JsonCursor, String>
  {
    s.top = Value::from(*v);
    Ok(s)
  }

  fn encode_double
    ( &self
    , mut s: JsonCursor
    , v: &f64
    ) -> Result<JsonCursor, String>
  {
    s.top = Value::from(*v);
    Ok(s)
  }

  fn encode_string
    ( &self
    , mut s: JsonCursor
    , v: &String
    ) -> Result<JsonCursor, String>
  {
    s.top = Value::from(v.clone());
    Ok(s)
  }
}

impl DecoderImpl for JsonCodecImpl {
  type Repr = Value;

  fn decode_value<A>(&self, r: &Value) -> Result<A, String> where A: Decoding {
    let s = JsonCursor { top: r.clone(), stack: Vec::new() };
    let (_, a) = A::decode(s, self)?;
    Ok(a)
  }

  fn decode_record<A>
    ( &self
    , s: JsonCursor
    , _: usize
    , k: fn(&Self, JsonCursor) -> Result<(JsonCursor, A), String>
    ) -> Result<(JsonCursor, A), String>
  {
    let mut s1 = JsonCursor { ..s };
    s1.stack.push(s1.top);
    s1.top = Value::Null;
    let (mut s2, a) = k(self, s1)?;
    match s2.stack.pop() {
      None          => err("Error"),
      Some(old_top) => {
        s2.top = old_top;
        Ok((s2, a))
      },
    }
  }

  fn decode_record_field<A>
    ( &self
    , s: JsonCursor
    , _: usize
    , name: &str
    ) -> Result<(JsonCursor, A), String>
  where A: Decoding
  {
    let mut s1 = s;
    match s1.stack.last_mut() {
      Some(Value::Object(m)) => match m.get(name) {
        Some(v) => {
          s1.top = v.clone();
          A::decode(s1, self)
        },
        None    => err("cannot decode Record"),
      },
      _                      => err("Error")
    }
  }


  fn decode_enum<A>
    ( &self
    , s: JsonCursor
    , _: fn(i32) -> Option<A>
    , by_name: fn(&str) -> Option<A>
    ) -> Result<(JsonCursor, A), String>
  {
    let (s1, n) = self.decode_string(s)?;
    match by_name(&n) {
      Some(v) => Ok((s1, v)),
      None    => err("cannot decode Enum"),
    }
  }


  fn decode_maybe<A>
    ( &self
    , s: JsonCursor
    ) -> Result<(JsonCursor, Option<A>), String>
  where A: Decoding
  {
    match s.top {
      Value::Null => Ok((s, None)),
      _           => {
        let (s1, a) = A::decode(s, self)?;
        Ok((s1, Some(a)))
      },
    }
  }

  fn decode_list<A>
    ( &self
    , s: JsonCursor
    ) -> Result<(JsonCursor, Vec<A>), String>
  where A: Decoding
  {
    let mut s1 = s;
    match &s1.top {
      Value::Array(vs) => {
        let vs_ = vs.clone();
        let mut rv = Vec::with_capacity(vs_.len());
        for v in vs_ {
          s1.top = v;
          let (s2, x) = A::decode(s1, self)?;
          s1 = s2;
          rv.push(x);
        }
        Ok((s1, rv))
      },
      _           => err("cannot decode List"),
    }
  }


  fn decode_unit(&self, s: JsonCursor) -> Result<(JsonCursor, ()), String>
  {
    match s.top {
      Value::Null => Ok((s, ())),
      _           => err("cannot decode Unit"),
    }
  }

  fn decode_bool(&self, s: JsonCursor) -> Result<(JsonCursor, bool), String>
  {
    match s.top {
      Value::Bool(v) => Ok((s, v)),
      _              => err("cannot decode Bool"),
    }
  }

  fn decode_int32(&self, s: JsonCursor) -> Result<(JsonCursor, i32), String>
  {
    let e = err("cannot decode Int32");
    match &s.top {
      Value::Number(n) => {
        match n.as_i64() {
          Some(v) => match v.try_into() {
            Ok(v_) => Ok((s, v_)),
            Err(_) => e,
          },
          None    => e,
        }
      },
      _                => e,
    }
  }

  fn decode_double(&self, s: JsonCursor) -> Result<(JsonCursor, f64), String>
  {
    let e = err("cannot decode Double");
    match &s.top {
      Value::Number(n) => {
        match n.as_f64() {
          Some(v) => Ok((s, v)),
          None    => e,
        }
      },
      _                => e,
    }
  }

  fn decode_string(&self, s: JsonCursor) -> Result<(JsonCursor, String), String>
  {
    match &s.top {
      Value::String(v) => {
        let v_ = v.clone();
        Ok((s, v_))
      },
      _                => err("cannot decode String"),
    }
  }
}

impl ForeignEncodersImpl for JsonCodecImpl {
  type Error = String;
  type State = JsonCursor;

  fn encode_date_time
    ( &self
    , s: JsonCursor
    , v: &NaiveDateTime
    ) -> Result<JsonCursor, String>
  {
    let date_s = v.format(&self.date_time_format).to_string();
    self.encode_string(s, &date_s)
  }
}

impl ForeignDecodersImpl for JsonCodecImpl {
  type Error = String;
  type State = JsonCursor;

  fn decode_date_time
    ( &self
    , s: JsonCursor
    ) -> Result<(JsonCursor, NaiveDateTime), String>
  {
    let (s1, date_s) = self.decode_string(s)?;
    match NaiveDateTime::parse_from_str(&date_s, &self.date_time_format) {
      Ok(v)  => Ok((s1, v)),
      Err(_) => err("cannot decode DateTime"),
    }
  }
}
