type ClientTransport f g m ra rb ha hb =
  forall a b. (a -> ha ra)
           -> (rb -> hb b)
           -> QualName Data.Text.Text
           -> f a
           -> m (g b)
