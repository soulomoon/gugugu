use std::env::var;

pub fn get_host_and_port() -> (String, u16)
{
  let host = match var("GUGUGU_EXAMPLE_HOST") {
    Ok(v)  => v,
    Err(_) => String::from("127.0.0.1"),
  };
  let port = match var("GUGUGU_EXAMPLE_PORT") {
    Ok(v)  => match v.parse() {
      Ok(v_) => v_,
      Err(_) => 8080,
    },
    Err(_) => 8080,
  };
  (host, port)
}

pub fn err<A>(s: &str) -> Result<A, String>
{
  Err(String::from(s))
}
