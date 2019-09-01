trait Encoder[A] {
  def encode[S, R](s: S, a: A, impl: EncoderImpl[S, R]): S
}

object Encoder {
  def apply[A](implicit encoder: Encoder[A]): Encoder[A] = encoder
  def encode[S, R, A](a: A, impl: EncoderImpl[S, R])
                     (implicit encoder: Encoder[A]): R = {
    impl.encodeWithState(s => encoder.encode(s, a, impl))
  }

  implicit val encodeUnit: Encoder[Unit] = new Encoder[Unit] {
    override def encode[S, R](s: S, a: Unit, impl: EncoderImpl[S, R]): S = {
      impl.encodeUnit(s, a)
    }
  }
  implicit val encodeBool: Encoder[Boolean] = new Encoder[Boolean] {
    override def encode[S, R](s: S, a: Boolean, impl: EncoderImpl[S, R]): S = {
      impl.encodeBool(s, a)
    }
  }
  implicit val encodeInt32: Encoder[Int] = new Encoder[Int] {
    override def encode[S, R](s: S, a: Int, impl: EncoderImpl[S, R]): S = {
      impl.encodeInt32(s, a)
    }
  }
  implicit val encodeDouble: Encoder[Double] = new Encoder[Double] {
    override def encode[S, R](s: S, a: Double, impl: EncoderImpl[S, R]): S = {
      impl.encodeDouble(s, a)
    }
  }
  implicit val encodeString: Encoder[String] = new Encoder[String] {
    override def encode[S, R](s: S, a: String, impl: EncoderImpl[S, R]): S = {
      impl.encodeString(s, a)
    }
  }

  implicit def encodeMaybe[A]( implicit encoder: Encoder[A]
                             ): Encoder[Option[A]] = {
    new Encoder[Option[A]] {
      override def encode[S, R]( s: S, a: Option[A]
                               , impl: EncoderImpl[S, R]
                               ): S = {
        impl.encodeMaybe(s, a.isEmpty, s1 => {
          a match {
            case Some(v) => encoder.encode(s1, v, impl)
            case None => s1
          }
        })
      }
    }
  }

  implicit def encodeList[A]( implicit encoder: Encoder[A]
                            ): Encoder[Vector[A]] = {
    new Encoder[Vector[A]] {
      override def encode[S, R]( s: S, a: Vector[A]
                               , impl: EncoderImpl[S, R]
                               ): S = {
        val len = a.length
        impl.encodeList(s, len, s1 => {
          a.zipWithIndex.foldLeft(s1) { (s2, vi) =>
            val (v, i) = vi
            impl.encodeListNth(s2, i, encoder.encode(_, v, impl))
          }
        })
      }
    }
  }
}
