use std::pin::Pin;

pub trait ClientTransport<E, I, O, RA, RB, EA, EB> {
  fn send<A, B, CA, CB>
    ( &self
    , namespace: &[&str]
    , name: &str
    , a: A
    , i: I
    , ca: Arc<CA>
    , cb: Arc<CB>
    ) -> Pin<Box<dyn Future<Output = Result<(O, B), E>> + Send>>
  where A: Encoding + Send + 'static
      , B: Decoding + Send + 'static
      , CA: EncoderImpl<Error = EA, Repr = RA> + Send + Sync + 'static
      , CB: DecoderImpl<Error = EB, Repr = RB> + Send + Sync + 'static
  ;
}

#[derive(Clone)]
pub struct GuguguClient<T, CA, CB> {
  pub transport: T,
  pub encoder_impl: Arc<CA>,
  pub decoder_impl: Arc<CB>,
}
