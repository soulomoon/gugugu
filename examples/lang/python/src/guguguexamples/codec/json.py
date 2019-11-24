from __future__ import annotations
from dataclasses import dataclass
from typing import List, Callable, Tuple, Optional, TypeVar, Union
from datetime import datetime

from gugugu.lang.python.runtime.codec import (
    EncoderImpl,
    DecoderImpl,
)

JsonRepr = Union[None, int, float, str, list, dict]
A = TypeVar("A")


@dataclass()
class JsonCursor:
    top: JsonRepr
    stack: List[JsonRepr]

    @classmethod
    def from_json(cls, v: JsonRepr) -> JsonCursor:
        return cls(top=v, stack=list())

    def assert_empty_stack(self) -> None:
        if len(self.stack) != 0:
            raise CodecException("unexpected finalize")

    def push(self, new_top: JsonRepr) -> None:
        self.stack.append(self.top)
        self.top = new_top

    def pop(self) -> JsonRepr:
        if len(self.stack) == 0:
            raise CodecException("stack empty")
        old_top = self.top
        self.top = self.stack.pop()
        return old_top


class CodecException(Exception):
    pass


class JsonCodecImpl(EncoderImpl[JsonCursor, JsonRepr],
                    DecoderImpl[JsonCursor, JsonRepr]):

    UNIT = object()

    def encode_with_state(self,
                          k: Callable[[JsonCursor], JsonCursor]) -> JsonRepr:
        s = JsonCursor.from_json(None)
        s = k(s)
        s.assert_empty_stack()
        return s.top

    def decode_with_state(self, r: JsonRepr,
                          k: Callable[[JsonCursor], Tuple[JsonCursor, A]],
                          ) -> A:
        s = JsonCursor.from_json(r)
        s, a = k(s)
        s.assert_empty_stack()
        return a

    def encode_record(self, s: JsonCursor, n_fields: int,
                      k: Callable[[JsonCursor], JsonCursor]) -> JsonCursor:
        s.top = dict()
        return k(s)

    def encode_record_field(self, s: JsonCursor, i: int, name: str,
                            k: Callable[[JsonCursor], JsonCursor],
                            ) -> JsonCursor:
        s.push(None)
        s = k(s)
        value = s.pop()
        if not isinstance(s.top, dict):
            raise CodecException(f"cannot write Record field: {name}")
        s.top[name] = value
        return s

    def decode_record(self, s: JsonCursor, n_fields: int,
                      k: Callable[[JsonCursor], Tuple[JsonCursor, A]],
                      ) -> Tuple[JsonCursor, A]:
        if not isinstance(s.top, dict) or len(s.top) < n_fields:
            raise CodecException("cannot read Record")
        return k(s)

    def decode_record_field(self, s: JsonCursor, i: int, name: str,
                            k: Callable[[JsonCursor], Tuple[JsonCursor, A]],
                            ) -> Tuple[JsonCursor, A]:
        top = s.top
        if not isinstance(top, dict):
            raise CodecException("cannot read Record")
        if name not in top:
            raise CodecException(f"cannot read Record field: {name}")
        s.push(top[name])
        s, a = k(s)
        s.pop()
        return s, a

    def encode_enum(self, s: JsonCursor, a: A, as_index: Callable[[A], int],
                    as_name: Callable[[A], str]) -> JsonCursor:
        return self.encode_string(s, as_name(a))

    def decode_enum(self, s: JsonCursor,
                    by_index: Callable[[int], Optional[A]],
                    by_name: Callable[[str], Optional[A]],
                    ) -> Tuple[JsonCursor, A]:
        s, name = self.decode_string(s)
        a = by_name(name)
        if a is None:
            raise CodecException("cannot read Enum")
        return s, a

    def encode_maybe(self, s: JsonCursor, v: Optional[A],
                     k: Callable[[JsonCursor, A], JsonCursor],
                     ) -> JsonCursor:
        if v is None:
            s.top = None
        else:
            s = k(s, v)
        return s

    def decode_maybe(self, s: JsonCursor,
                     k: Callable[[JsonCursor], Tuple[JsonCursor, A]],
                     ) -> Tuple[JsonCursor, Optional[A]]:
        if s.top is None:
            return s, None
        return k(s)

    def encode_list(self, s: JsonCursor, v: List[A],
                    k: Callable[[JsonCursor, A], JsonCursor],
                    ) -> JsonCursor:
        s.top = list()
        for a in v:
            s.push(None)
            s = k(s, a)
            value = s.pop()
            if not isinstance(s.top, list):
                raise CodecException("bad state: encode_list")
            s.top.append(value)
        return s

    def decode_list(self, s: JsonCursor,
                    k: Callable[[JsonCursor], Tuple[JsonCursor, A]],
                    ) -> Tuple[JsonCursor, List[A]]:
        if not isinstance(s.top, list):
            raise CodecException("cannot decode List")
        rv = list()
        for value in s.top:
            s.push(value)
            s, a = k(s)
            s.pop()
            rv.append(a)
        return s, rv

    def encode_unit(self, s: JsonCursor, v: object) -> JsonCursor:
        s.top = None
        return s

    def decode_unit(self, s: JsonCursor) -> Tuple[JsonCursor, object]:
        if s.top is not None:
            raise CodecException("cannot read Unit")
        return s, self.UNIT

    def encode_bool(self, s: JsonCursor, v: bool) -> JsonCursor:
        s.top = v
        return s

    def decode_bool(self, s: JsonCursor) -> Tuple[JsonCursor, bool]:
        if not isinstance(s.top, bool):
            raise CodecException("cannot read Bool")
        return s, s.top

    def encode_int32(self, s: JsonCursor, v: int) -> JsonCursor:
        s.top = v
        return s

    def decode_int32(self, s: JsonCursor) -> Tuple[JsonCursor, int]:
        if not isinstance(s.top, int):
            raise CodecException("cannot read Int32")
        return s, s.top

    def encode_double(self, s: JsonCursor, v: float) -> JsonCursor:
        s.top = v
        return s

    def decode_double(self, s: JsonCursor) -> Tuple[JsonCursor, float]:
        if not isinstance(s.top, float):
            raise CodecException("cannot read Double")
        return s, s.top

    def encode_string(self, s: JsonCursor, v: str) -> JsonCursor:
        s.top = v
        return s

    def decode_string(self, s: JsonCursor) -> Tuple[JsonCursor, str]:
        if not isinstance(s.top, str):
            raise CodecException("cannot read String")
        return s, s.top

    FORMATTER = "%Y-%m-%dT%H:%M:%S"

    def encode_date_time(self, s: JsonCursor, v: datetime) -> JsonCursor:
        str_ = v.strftime(self.FORMATTER)
        return self.encode_string(s, str_)

    def decode_date_time(self, s: JsonCursor) -> Tuple[JsonCursor, datetime]:
        s, str_ = self.decode_string(s)
        return s, datetime.strptime(str_, self.FORMATTER)
