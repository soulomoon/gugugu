class ClientTransport(ABC):
    """ The type of this handler cannot be expressed in Python.
        Haskell notation:

        type ClientTransport f g m ra rb =
            forall a b. QualName String
                     -> (a -> ra)
                     -> (rb -> b)
                     -> f a
                     -> m (g b)
    """
    @abstractmethod
    def send(self, name: QualName[str], encoder, decoder, fa):
        raise NotImplementedError
