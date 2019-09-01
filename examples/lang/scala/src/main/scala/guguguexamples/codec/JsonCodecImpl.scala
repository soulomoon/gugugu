package guguguexamples.codec

import gugugu.lang.scala.runtime.codec._
import io.circe.Json

object JsonCodecImpl
  extends EncoderImpl[JsonCursor, Json]
     with DecoderImpl[JsonCursor, Json] {

  override def encodeWithState(k: JsonCursor => JsonCursor): Json = {
    val s1 = JsonCursor(Json.Null, Nil)
    val s2 = k(s1)
    if (s2.stack.nonEmpty) {
      throw new CodecException("unexpected finalize")
    }
    s2.top
  }

  override def decodeWithState[A]( r: Json
                                 , k: JsonCursor => (JsonCursor, A)
                                 ): A = {
    val s1 = JsonCursor(r, Nil)
    val (s2, a) = k(s1)
    if (s2.stack.nonEmpty) {
      throw new CodecException("unconsumed input")
    }
    a
  }

  override def encodeUnit(s: JsonCursor, v: Unit): JsonCursor = {
    JsonCursor(Json.Null, s.stack)
  }
  override def decodeUnit(s: JsonCursor): (JsonCursor, Unit) = {
    s.top.asNull match {
      case None => throw new CodecException("cannot read Unit")
      case Some(v) => (s, v)
    }
  }

  override def encodeBool(s: JsonCursor, v: Boolean): JsonCursor = {
    JsonCursor(Json.fromBoolean(v), s.stack)
  }
  override def decodeBool(s: JsonCursor): (JsonCursor, Boolean) = {
    s.top.asBoolean match {
      case None => throw new CodecException("cannot read Bool")
      case Some(v) => (s, v)
    }
  }

  override def encodeInt32(s: JsonCursor, v: Int): JsonCursor = {
    JsonCursor(Json.fromInt(v), s.stack)
  }
  override def decodeInt32(s: JsonCursor): (JsonCursor, Int) = {
    s.top.asNumber.flatMap(_.toInt) match {
      case None => throw new CodecException("cannot read Int32")
      case Some(v) => (s, v)
    }
  }

  override def encodeDouble(s: JsonCursor, v: Double): JsonCursor = {
    Json.fromDouble(v) match {
      case Some(s1) => JsonCursor(s1, s.stack)
      case None => throw new CodecException(s"cannot write Double: $v")
    }
  }
  override def decodeDouble(s: JsonCursor): (JsonCursor, Double) = {
    s.top.asNumber.map(_.toDouble) match {
      case None => throw new CodecException("cannot read Double")
      case Some(v) => (s, v)
    }
  }

  override def encodeString(s: JsonCursor, v: String): JsonCursor = {
    JsonCursor(Json.fromString(v), s.stack)
  }
  override def decodeString(s: JsonCursor): (JsonCursor, String) = {
    s.top.asString match {
      case None => throw new CodecException("cannot read String")
      case Some(v) => (s, v)
    }
  }


  override def encodeMaybe( s: JsonCursor, isNothing: Boolean
                          , k: JsonCursor => JsonCursor
                          ): JsonCursor = {
    if (isNothing) {
      k(JsonCursor(Json.Null, s.stack))
    } else {
      k(s)
    }
  }
  override def decodeMaybe[A]( s: JsonCursor
                             , k: (JsonCursor, Boolean) => (JsonCursor, A)
                             ): (JsonCursor, A) = {
    k(s, s.top.isNull)
  }

  override def encodeList( s: JsonCursor, len: Int
                         , k: JsonCursor => JsonCursor
                         ): JsonCursor = {
    k(JsonCursor(Json.fromValues(Nil), s.stack))
  }
  override def encodeListNth( s: JsonCursor, i: Int
                            , k: JsonCursor => JsonCursor
                            ): JsonCursor = {
    val s1 = JsonCursor(Json.Null, s.top :: s.stack)
    val s2 = k(s1)
    s2.stack match {
      case j1 :: js1 =>
        val xs = j1.asArray match {
          case Some(v) => v
          case None => throw new CodecException(s"cannot write List nth: $i")
        }
        JsonCursor(Json.fromValues(xs :+ s2.top), js1)
      case _ => throw new CodecException("bad state: encodeListNth")

    }
  }
  override def decodeList[A]( s: JsonCursor
                            , k: (JsonCursor, Int) => (JsonCursor, A)
                            ): (JsonCursor, A) = {
    s.top.asArray match {
      case None => throw new CodecException("cannot read List")
      case Some(js) => k(s, js.length)
    }
  }
  override def decodeListNth[A]( s: JsonCursor, i: Int
                               , k: JsonCursor => (JsonCursor, A)
                               ): (JsonCursor, A) = {
    val js = s.top.asArray match {
      case None => throw new CodecException("cannot read List")
      case Some(v) => v
    }
    if (i >= js.length) {
      throw new CodecException(s"cannot read List nth: $i")
    }
    val s1 = JsonCursor(js(i), s.top :: s.stack)
    val (s2, a) = k(s1)
    s2.stack match {
      case j1 :: js1 => (JsonCursor(j1, js1), a)
      case _ => throw new CodecException("bad state: decodeListNth")
    }
  }

  override def encodeRecord( s: JsonCursor, nFields: Int
                           , k: JsonCursor => JsonCursor
                           ): JsonCursor = {
    k(JsonCursor(Json.obj(), s.stack))
  }
  override def encodeRecordField( s: JsonCursor, i: Int, name: String
                                , k: JsonCursor => JsonCursor
                                ): JsonCursor = {
    val s1 = JsonCursor(Json.Null, s.top :: s.stack)
    val s2 = k(s1)
    s2.stack match {
      case j1 :: js1 =>
        val obj = j1.asObject match {
          case Some(v) => v
          case None => throw new CodecException(s"cannot write Record field: $name")
        }
        JsonCursor(Json.fromJsonObject(obj.add(name, s2.top)), js1)
      case _ => throw new CodecException("bad state: encodeRecordField")
    }
  }
  override def decodeRecord[A]( s: JsonCursor, nFields: Int
                              , k: JsonCursor => (JsonCursor, A)
                              ): (JsonCursor, A) = {
    s.top.asObject match {
      case Some(jo) if jo.size >= nFields =>
      case _ => throw new CodecException("cannot read Record")
    }
    k(s)
  }
  override def decodeRecordField[A]( s: JsonCursor, i: Int, name: String
                                   , k: JsonCursor => (JsonCursor, A)
                                   ): (JsonCursor, A) = {
    val js = s.top.asObject match {
      case Some(v) => v
      case _ => throw new CodecException("cannot read Record")
    }
    val fieldObj = js(name) match {
      case Some(v) => v
      case _ => throw new CodecException(s"cannot read Record field: $name")
    }
    val s1 = JsonCursor(fieldObj, s.top :: s.stack)
    val (s2, a) = k(s1)
    s2.stack match {
      case j1 :: js1 => (JsonCursor(j1, js1), a)
      case _ => throw new CodecException("bad state: decodeRecordField")
    }
  }

}
