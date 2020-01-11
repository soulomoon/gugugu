pub trait ServerCodecHandler<E, I, O, RA, RB, EA, EB> {
  type OutputFuture: Future<Output = Result<(O, RB), E>>;

  fn run<A, CA, CB, B, R>
    ( &self
    , ca: Arc<CA>
    , cb: Arc<CB>
    , k: impl FnOnce(A, I) -> R + Send + 'static
    , ra: RA
    , i: I
    ) -> Self::OutputFuture
  where R: Future<Output = Result<(O, B), E>> + Send
      , A: Decoding + Send + 'static
      , B: Encoding + Send + 'static
      , CA: DecoderImpl<Error = EA, Repr = RA> + Send + Sync + 'static
      , CB: EncoderImpl<Error = EB, Repr = RB> + Send + Sync + 'static
  ;

}
