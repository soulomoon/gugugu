type ServerCodecHandler f g m ra rb ha hb =
  forall a b. (ra   ->   ha  a )
           -> ( b   ->   hb rb )
           -> (f a  -> m (g  b))
           ->  f ra -> m (g rb)

type ServerCodecHandler' f m r h = ServerCodecHandler f f m r r h h

type ServerTransport f g m ra rb ha hb =
     QualName Data.Text.Text
  -> ServerCodecHandler f g m ra rb ha hb
  -> Maybe (f ra -> m (g rb))

type ServerTransport' f m r h = ServerTransport f f m r r h h
