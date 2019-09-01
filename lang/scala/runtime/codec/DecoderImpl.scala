trait DecoderImpl[S, R] {
  def decodeWithState[A](r: R, k: S => (S, A)): A

  def decodeUnit(s: S): (S, Unit)
  def decodeBool(s: S): (S, Boolean)
  def decodeInt32(s: S): (S, Int)
  def decodeDouble(s: S): (S, Double)
  def decodeString(s: S): (S, String)

  def decodeMaybe[A](s: S, k: (S, Boolean) => (S, A)): (S, A)

  def decodeList[A](s: S, k: (S, Int) => (S, A)): (S, A)
  def decodeListNth[A](s: S, i: Int, k: S => (S, A)): (S, A)

  def decodeRecord[A](s: S, nFields: Int, k: S => (S, A)): (S, A)
  def decodeRecordField[A]( s: S
                          , i: Int, name: String
                          , k: S => (S, A)
                          ): (S, A)
}
