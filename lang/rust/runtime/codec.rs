use super::foreign::{ForeignEncodersImpl, ForeignDecodersImpl};

pub trait Encoding {
  fn encode<C>
    ( s: C::State
    , a: &Self
    , c: &C
    ) -> Result<C::State, C::Error>
  where C: EncoderImpl
  ;
}

pub trait Decoding: Sized {
  fn decode<C>
    ( s: C::State
    , c: &C
    ) -> Result<(C::State, Self), C::Error>
  where C: DecoderImpl
  ;
}

pub trait EncoderImpl: ForeignEncodersImpl {
  type Repr;

  fn encode_value<A>
    ( &self
    , a: &A
    ) -> Result<Self::Repr, Self::Error>
  where A: Encoding
  ;

  fn encode_record<A>
    ( &self
    , s: Self::State
    , n_fields: usize
    , a: &A
    , k: fn(&Self, Self::State, &A) -> Result<Self::State, Self::Error>
    ) -> Result<Self::State, Self::Error>
  ;
  fn encode_record_field<A>
    ( &self
    , s: Self::State
    , i: usize
    , name: &str
    , a: &A
    ) -> Result<Self::State, Self::Error>
  where A: Encoding
  ;

  fn encode_enum<A>
    ( &self
    , s: Self::State
    , a: A
    , as_index: fn(A) -> i32
    , as_name: fn(A) -> &'static str
    ) -> Result<Self::State, Self::Error>
  ;

  fn encode_maybe<A>
    ( &self
    , s: Self::State
    , v: &Option<A>
    ) -> Result<Self::State, Self::Error>
  where A: Encoding
  ;
  fn encode_list<A>
    ( &self
    , s: Self::State
    , v: &Vec<A>
    ) -> Result<Self::State, Self::Error>
  where A: Encoding
  ;
  fn encode_unit
    ( &self
    , s: Self::State
    , v: &()
    ) -> Result<Self::State, Self::Error>
  ;
  fn encode_bool
    ( &self
    , s: Self::State
    , v: &bool
    ) -> Result<Self::State, Self::Error>
  ;
  fn encode_int32
    ( &self
    , s: Self::State
    , v: &i32
    ) -> Result<Self::State, Self::Error>
  ;
  fn encode_double
    ( &self
    , s: Self::State
    , v: &f64
    ) -> Result<Self::State, Self::Error>
  ;
  fn encode_string
    ( &self
    , s: Self::State
    , v: &String
    ) -> Result<Self::State, Self::Error>
  ;
}

pub trait DecoderImpl: ForeignDecodersImpl {
  type Repr;

  fn decode_value<A>
    ( &self
    , r: &Self::Repr
    ) -> Result<A, Self::Error>
  where A: Decoding
  ;

  fn decode_record<A>
    ( &self
    , s: Self::State
    , n_fields: usize
    , k: fn(&Self, Self::State) -> Result<(Self::State, A), Self::Error>
    ) -> Result<(Self::State, A), Self::Error>
  ;
  fn decode_record_field<A>
    ( &self
    , s: Self::State
    , i: usize
    , name: &str
    ) -> Result<(Self::State, A), Self::Error>
  where A: Decoding
  ;

  fn decode_enum<A>
    ( &self
    , s: Self::State
    , by_index: fn(i32) -> Option<A>
    , by_name: fn(&str) -> Option<A>
    ) -> Result<(Self::State, A), Self::Error>
  ;

  fn decode_maybe<A>
    ( &self
    , s: Self::State
    ) -> Result<(Self::State, Option<A>), Self::Error>
  where A: Decoding
  ;
  fn decode_list<A>
    ( &self
    , s: Self::State
    ) -> Result<(Self::State, Vec<A>), Self::Error>
  where A: Decoding
  ;
  fn decode_unit
    ( &self
    , s: Self::State
    ) -> Result<(Self::State, ()), Self::Error>
  ;
  fn decode_bool
    ( &self
    , s: Self::State
    ) -> Result<(Self::State, bool), Self::Error>
  ;
  fn decode_int32
    ( &self
    , s: Self::State
    ) -> Result<(Self::State, i32), Self::Error>
  ;
  fn decode_double
    ( &self
    , s: Self::State
    ) -> Result<(Self::State, f64), Self::Error>
  ;
  fn decode_string
    ( &self
    , s: Self::State
    ) -> Result<(Self::State, String), Self::Error>
  ;
}

impl<A> Encoding for Option<A> where A: Encoding {
  fn encode<C>
    ( s: C::State
    , a: &Self
    , c: &C
    ) -> Result<C::State, C::Error>
  where C: EncoderImpl
  {
    c.encode_maybe(s, a)
  }
}

impl<A> Encoding for Vec<A> where A: Encoding {
  fn encode<C>
    ( s: C::State
    , a: &Self
    , c: &C
    ) -> Result<C::State, C::Error>
  where C: EncoderImpl
  {
    c.encode_list(s, a)
  }
}

impl Encoding for () {
  fn encode<C>
    ( s: C::State
    , a: &Self
    , c: &C
    ) -> Result<C::State, C::Error>
  where C: EncoderImpl
  {
    c.encode_unit(s, a)
  }
}

impl Encoding for bool {
  fn encode<C>
    ( s: C::State
    , a: &Self
    , c: &C
    ) -> Result<C::State, C::Error>
  where C: EncoderImpl
  {
    c.encode_bool(s, a)
  }
}

impl Encoding for i32 {
  fn encode<C>
    ( s: C::State
    , a: &Self
    , c: &C
    ) -> Result<C::State, C::Error>
  where C: EncoderImpl
  {
    c.encode_int32(s, a)
  }
}

impl Encoding for f64 {
  fn encode<C>
    ( s: C::State
    , a: &Self
    , c: &C
    ) -> Result<C::State, C::Error>
  where C: EncoderImpl
  {
    c.encode_double(s, a)
  }
}

impl Encoding for String {
  fn encode<C>
    ( s: C::State
    , a: &Self
    , c: &C
    ) -> Result<C::State, C::Error>
  where C: EncoderImpl
  {
    c.encode_string(s, a)
  }
}


impl<A> Decoding for Option<A> where A: Decoding {
  fn decode<C>
    ( s: C::State
    , c: &C
    ) -> Result<(C::State, Self), C::Error>
  where C: DecoderImpl
  {
    c.decode_maybe(s)
  }
}

impl<A> Decoding for Vec<A> where A: Decoding {
  fn decode<C>
    ( s: C::State
    , c: &C
    ) -> Result<(C::State, Self), C::Error>
  where C: DecoderImpl
  {
    c.decode_list(s)
  }
}

impl Decoding for () {
  fn decode<C>
    ( s: C::State
    , c: &C
    ) -> Result<(C::State, Self), C::Error>
  where C: DecoderImpl
  {
    c.decode_unit(s)
  }
}

impl Decoding for bool {
  fn decode<C>
    ( s: C::State
    , c: &C
    ) -> Result<(C::State, Self), C::Error>
  where C: DecoderImpl
  {
    c.decode_bool(s)
  }
}

impl Decoding for i32 {
  fn decode<C>
    ( s: C::State
    , c: &C
    ) -> Result<(C::State, Self), C::Error>
  where C: DecoderImpl
  {
    c.decode_int32(s)
  }
}

impl Decoding for f64 {
  fn decode<C>
    ( s: C::State
    , c: &C
    ) -> Result<(C::State, Self), C::Error>
  where C: DecoderImpl
  {
    c.decode_double(s)
  }
}

impl Decoding for String {
  fn decode<C>
    ( s: C::State
    , c: &C
    ) -> Result<(C::State, Self), C::Error>
  where C: DecoderImpl
  {
    c.decode_string(s)
  }
}
