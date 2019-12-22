pub fn err<A>(s: &str) -> Result<A, String>
{
  Err(String::from(s))
}
