type ClientTransport f g m ra rb ha hb =
  forall a b. (a -> ha ra)
           -> (rb -> hb b)
           -> QualName Data.Text.Text
           -> f a
           -> m (g b)

type ClientTransport' f m r h = ClientTransport f f m r r h h

type GuguguClient' f m r h = GuguguClient f f m r r h h
