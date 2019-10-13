trait EncoderImpl[S, R] {
  def encodeWithState(k: S => S): R

  def encodeUnit(s: S, v: Unit): S
  def encodeBool(s: S, v: Boolean): S
  def encodeInt32(s: S, v: Int): S
  def encodeDouble(s: S, v: Double): S
  def encodeString(s: S, v: String): S

  def encodeMaybe(s: S, isNothing: Boolean, k: S => S): S

  def encodeList(s: S, len: Int, k: S => S): S
  def encodeListNth(s: S, i: Int, k: S => S): S

  def encodeRecord(s: S, nFields: Int, k: S => S): S
  def encodeRecordField( s: S
                       , i: Int, name: String
                       , k: S => S
                       ): S

  def encodeEnum[A]( s: S, a: A
                   , asIndex: A => Int, asName: A => String
                   ): S
}
