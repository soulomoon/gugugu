class ServerTransport(ABC):
    """ The type of this handler cannot be expressed in Python.
        Haskell notation:

        type ServerCodecHandler f g m ra rb =
            forall a b. (ra -> a)           -- ^ request decoder
                     -> (b -> rb)           -- ^ response encoder
                     -> (f a -> m (g b))    -- ^ request handler
                     -> f ra                -- ^ request encoded
                     -> m (g rb)            -- ^ response encoded
        type ServerTransport f g m ra rb = QualName String
                                        -> ServerCodecHandler f g m ra rb
                                        -> Maybe (f ra -> m (g rb))
    """
    @abstractmethod
    def ask(self, name: QualName[str], k):
        """ The self is expected to be type: ServerTransport f g m ra rb
            The k is expected to be type: ServerCodecHandler f g m ra rb
        """
        raise NotImplementedError
