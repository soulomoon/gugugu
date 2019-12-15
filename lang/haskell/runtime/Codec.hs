class Encoding a where
  encode :: (EncoderImpl c r g f, Applicative f) => c -> a -> f ()

class Decoding a where
  decode :: (DecoderImpl c r g f, Applicative f) => c ->      f a


class ForeignEncodersImpl c f => EncoderImpl c r g f | c -> r g f where
  encodeValue :: Encoding a => c -> a -> g r

  encodeRecord      ::               Int -> (c -> a -> f ()) -> c -> a -> f ()
  encodeRecordField :: Encoding a => Int -> Data.Text.Text   -> c -> a -> f ()

  encodeEnum :: (a -> Int) -> (a -> Data.Text.Text) -> c -> a -> f ()

  encodeMaybe :: Encoding a => c -> Maybe              a -> f ()
  encodeList  :: Encoding a => c -> Data.Vector.Vector a -> f ()

  encodeUnit   :: c -> ()             -> f ()
  encodeBool   :: c -> Bool           -> f ()
  encodeInt32  :: c -> Data.Int.Int32 -> f ()
  encodeDouble :: c -> Double         -> f ()
  encodeString :: c -> Data.Text.Text -> f ()

class ForeignDecodersImpl c f => DecoderImpl c r g f | c -> r g f where
  decodeValue :: Decoding a => c -> r -> g a

  decodeRecord      ::               Int -> (c -> f a)     -> c -> f a
  decodeRecordField :: Decoding a => Int -> Data.Text.Text -> c -> f a

  decodeEnum :: (Int -> Maybe a) -> (Data.Text.Text -> Maybe a) -> c -> f a

  decodeMaybe :: Decoding a => c -> f (Maybe              a)
  decodeList  :: Decoding a => c -> f (Data.Vector.Vector a)

  decodeUnit   :: c -> f ()
  decodeBool   :: c -> f Bool
  decodeInt32  :: c -> f Data.Int.Int32
  decodeDouble :: c -> f Double
  decodeString :: c -> f Data.Text.Text


instance Encoding ()             where encode = encodeUnit
instance Encoding Bool           where encode = encodeBool
instance Encoding Data.Int.Int32 where encode = encodeInt32
instance Encoding Double         where encode = encodeDouble
instance Encoding Data.Text.Text where encode = encodeString
instance Encoding a => Encoding (Maybe              a) where
  encode = encodeMaybe
instance Encoding a => Encoding (Data.Vector.Vector a) where
  encode = encodeList

instance Decoding ()             where decode = decodeUnit
instance Decoding Bool           where decode = decodeBool
instance Decoding Data.Int.Int32 where decode = decodeInt32
instance Decoding Double         where decode = decodeDouble
instance Decoding Data.Text.Text where decode = decodeString
instance Decoding a => Decoding (Maybe              a) where
  decode = decodeMaybe
instance Decoding a => Decoding (Data.Vector.Vector a) where
  decode = decodeList
