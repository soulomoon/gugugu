package guguguexamples.utils

object EnvConfig {

  lazy val host: String = {
    sys.env.getOrElse("GUGUGU_EXAMPLE_HOST", "127.0.0.1")
  }
  lazy val port: Int = {
    sys.env.get("GUGUGU_EXAMPLE_PORT") match {
      case Some(v) => v.toInt
      case None => 8080
    }
  }

}
