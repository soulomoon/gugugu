package guguguexamples.codec

import io.circe.Json

case class JsonCursor(top: Json, stack: List[Json])
