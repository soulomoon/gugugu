trait Decoder[A] {
  def decode[S, R](s: S, impl: DecoderImpl[S, R]): (S, A)
}

object Decoder extends ForeignDecoders {
  def apply[A](implicit decoder: Decoder[A]): Decoder[A] = decoder
  def decode[S, R, A](r: R, impl: DecoderImpl[S, R])
                     (implicit decoder: Decoder[A]): A = {
    impl.decodeWithState(r, s => decoder.decode(s, impl))
  }

  implicit val decodeUnit: Decoder[Unit] = new Decoder[Unit] {
    override def decode[S, R](s: S, impl: DecoderImpl[S, R]): (S, Unit) = {
      impl.decodeUnit(s)
    }
  }
  implicit val decodeBool: Decoder[Boolean] = new Decoder[Boolean] {
    override def decode[S, R](s: S, impl: DecoderImpl[S, R]): (S, Boolean) = {
      impl.decodeBool(s)
    }
  }
  implicit val decodeInt32: Decoder[Int] = new Decoder[Int] {
    override def decode[S, R](s: S, impl: DecoderImpl[S, R]): (S, Int) = {
      impl.decodeInt32(s)
    }
  }
  implicit val decodeDouble: Decoder[Double] = new Decoder[Double] {
    override def decode[S, R](s: S, impl: DecoderImpl[S, R]): (S, Double) = {
      impl.decodeDouble(s)
    }
  }
  implicit val decodeString: Decoder[String] = new Decoder[String] {
    override def decode[S, R](s: S, impl: DecoderImpl[S, R]): (S, String) = {
      impl.decodeString(s)
    }
  }

  implicit def decodeMaybe[A]( implicit decoder: Decoder[A]
                             ): Decoder[Option[A]] = {
    new Decoder[Option[A]] {
      override def decode[S, R]( s: S
                               , impl: DecoderImpl[S, R]): (S, Option[A]) = {
        impl.decodeMaybe(s, (s1, isNothing) => {
          if (isNothing) {
            (s1, None)
          } else {
            val (s2, a) = decoder.decode(s1, impl)
            (s2, Some(a))
          }
        })
      }
    }
  }

  implicit def decodeList[A]( implicit decoder: Decoder[A]
                            ): Decoder[Vector[A]] = {
    new Decoder[Vector[A]] {
      override def decode[S, R]( s: S
                               , impl: DecoderImpl[S, R]
                               ): (S, Vector[A]) = {
        impl.decodeList(s, (s1, len) => {
          val rv = Vector.newBuilder[A]
          val s2 = 0.until(len).foldLeft(s1) { (s3, i) =>
            val (s4, a) = impl.decodeListNth(s3, i, decoder.decode(_, impl))
            rv += a
            s4
          }
          (s2, rv.result())
        })
      }
    }
  }
}
