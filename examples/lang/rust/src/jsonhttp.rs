use std::collections::HashMap;
use std::str::{FromStr, from_utf8};

use serde_json::Value;
use http::header::{HeaderMap, HeaderName, HeaderValue, CONTENT_TYPE};
use hyper::body::{HttpBody, Body, to_bytes};

use crate::utils::err;

pub type Meta = HashMap<String, String>;

#[derive(Debug, Clone)]
pub struct MetaHandler {
  application_json: HeaderValue,
}

impl MetaHandler
{
  pub fn new() -> Result<Self, String>
  {
    let application_json = match HeaderValue::from_str("application/json") {
      Ok(v) => v,
      Err(_) => return err("impossible"),
    };
    Ok(MetaHandler {
      application_json: application_json
    })
  }

  pub fn write_meta_to_headers
    ( &self
    , meta: &Meta
    , headers: &mut HeaderMap<HeaderValue>
    ) -> Result<(), String>
  {
    headers.reserve(meta.len() + 1);
    headers.insert(CONTENT_TYPE, self.application_json.clone());
    for (k, v) in meta {
      let header_name = match HeaderName::from_str(&k) {
        Ok(x) => x,
        Err(_) => return err("cannot decode header name"),
      };
      let header_value = match HeaderValue::from_str(&v) {
        Ok(x) => x,
        Err(_) => return err("cannot decode header value"),
      };
      headers.insert(header_name, header_value);
    }
    Ok(())
  }

}

pub fn meta_from_headers(headers: &HeaderMap) -> Result<Meta, String>
{
  let mut rv = HashMap::with_capacity(headers.len());
  for (header_name, header_value) in headers {
    let k = header_name.to_string();
    let v = match header_value.to_str() {
      Ok(s) => String::from(s),
      Err(_) => return err("cannot encode header value"),
    };
    rv.insert(k, v);
  }
  Ok(rv)
}

pub async fn decode_json_from_body<A>
  ( body: A
  ) -> Result<Value, String>
where A: HttpBody
{
  let bs = match to_bytes(body).await {
    Ok(v) => v,
    Err(_) => return err("cannot read body"),
  };
  let s = match from_utf8(&bs) {
    Ok(v) => v,
    Err(_) => return err("cannot decode body"),
  };
  let j = match serde_json::from_str(s) {
    Ok(v) => v,
    Err(_) => return err("cannot decode body"),
  };
  Ok(j)
}

pub fn encode_json_to_body(json: &Value) -> Result<Body, String>
{
  let s = match serde_json::to_string(json) {
    Ok(v) => v,
    Err(_) => return err("cannot encode body"),
  };
  let body = Body::from(s);
  Ok(body)
}
