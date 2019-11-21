from __future__ import annotations
from abc import abstractmethod, ABC
from typing import Generic, TypeVar, Callable, Tuple, Optional, List

from .foreign import ForeignEncodersImpl, ForeignDecodersImpl

S = TypeVar("S")
R = TypeVar("R")
A = TypeVar("A")


class EncoderImpl(ForeignEncodersImpl[S], Generic[S, R]):

    @abstractmethod
    def encode_with_state(self, k: Callable[[S], S]) -> R:
        raise NotImplementedError

    @abstractmethod
    def encode_record(self, s: S, n_fields: int, k: Callable[[S], S]) -> S:
        raise NotImplementedError

    @abstractmethod
    def encode_record_field(self, s: S,
                            i: int, name: str,
                            k: Callable[[S], S]) -> S:
        raise NotImplementedError

    @abstractmethod
    def encode_enum(self, s: S, a: A,
                    as_index: Callable[[A], int],
                    as_name: Callable[[A], str]) -> S:
        raise NotImplementedError

    @abstractmethod
    def encode_unit(self, s: S, v: object) -> S:
        raise NotImplementedError

    @abstractmethod
    def encode_bool(self, s: S, v: bool) -> S:
        raise NotImplementedError

    @abstractmethod
    def encode_int32(self, s: S, v: int) -> S:
        raise NotImplementedError

    @abstractmethod
    def encode_double(self, s: S, v: float) -> S:
        raise NotImplementedError

    @abstractmethod
    def encode_string(self, s: S, v: str) -> S:
        raise NotImplementedError

    @abstractmethod
    def encode_maybe(self, s: S, v: Optional[A], k: Callable[[S, A], S]) -> S:
        raise NotImplementedError

    @abstractmethod
    def encode_list(self, s: S, v: List[A], k: Callable[[S, A], S]) -> S:
        raise NotImplementedError

class DecoderImpl(ForeignDecodersImpl[S], Generic[S, R]):

    @abstractmethod
    def decode_with_state(self, r: R, k: Callable[[S], Tuple[S, A]]) -> A:
        raise NotImplementedError

    @abstractmethod
    def decode_record(self, s: S, n_fields: int,
                      k: Callable[[S], Tuple[S, A]]) -> Tuple[S, A]:
        raise NotImplementedError

    @abstractmethod
    def decode_record_field(self, s: S,
                            i: int, name: str,
                            k: Callable[[S], Tuple[S, A]]) -> Tuple[S, A]:
        raise NotImplementedError

    @abstractmethod
    def decode_enum(self, s: S,
                    by_index: Callable[[int], Optional[A]],
                    by_name: Callable[[str], Optional[A]]) -> Tuple[S, A]:
        raise NotImplementedError

    @abstractmethod
    def decode_unit(self, s: S) -> Tuple[S, object]:
        raise NotImplementedError

    @abstractmethod
    def decode_bool(self, s: S) -> Tuple[S, bool]:
        raise NotImplementedError

    @abstractmethod
    def decode_int32(self, s: S) -> Tuple[S, int]:
        raise NotImplementedError

    @abstractmethod
    def decode_double(self, s: S) -> Tuple[S, float]:
        raise NotImplementedError

    @abstractmethod
    def decode_string(self, s: S) -> Tuple[S, str]:
        raise NotImplementedError

    @abstractmethod
    def decode_unit(self, s: S) -> Tuple[S, object]:
        raise NotImplementedError

    @abstractmethod
    def decode_maybe(self, s: S,
                     k: Callable[[S], Tuple[S, A]]) -> Tuple[S, Optional[A]]:
        raise NotImplementedError

    @abstractmethod
    def decode_list(self, s: S,
                    k: Callable[[S], Tuple[S, A]]) -> Tuple[S, List[A]]:
        raise NotImplementedError


Encoder = Callable[[S, A, EncoderImpl[S, R]], S]
Decoder = Callable[[S, EncoderImpl[S, R]], Tuple[S, A]]


class Encoders:

    @classmethod
    def encode_unit(cls, s: S, a: object, impl: EncoderImpl[S, R]) -> S:
        return impl.encode_unit(s, a)

    @classmethod
    def encode_bool(cls, s: S, a: bool, impl: EncoderImpl[S, R]) -> S:
        return impl.encode_bool(s, a)

    @classmethod
    def encode_int32(cls, s: S, a: int, impl: EncoderImpl[S, R]) -> S:
        return impl.encode_int32(s, a)

    @classmethod
    def encode_double(cls, s: S, a: float, impl: EncoderImpl[S, R]) -> S:
        return impl.encode_double(s, a)

    @classmethod
    def encode_string(cls, s: S, a: str, impl: EncoderImpl[S, R]) -> S:
        return impl.encode_string(s, a)

    @classmethod
    def encode_maybe(cls, encoder: Encoder):
        def encode_maybe_a(s: S, a: Optional[A], impl: EncoderImpl[S, R]) -> S:
            return impl.encode_maybe(s, a,
                                     lambda s1, a1: encoder(s1, a1, impl))
        return encode_maybe_a

    @classmethod
    def encode_list(cls, encoder: Encoder):
        def encode_list_a(s: S, a: List[A], impl: EncoderImpl[S, R]) -> S:
            return impl.encode_list(s, a,
                                    lambda s1, a1: encoder(s1, a1, impl))
        return encode_list_a


class Decoders:

    @classmethod
    def decode_unit(cls, s: S, impl: DecoderImpl[S, R]) -> Tuple[S, object]:
        return impl.decode_unit(s)

    @classmethod
    def decode_bool(cls, s: S, impl: DecoderImpl[S, R]) -> Tuple[S, bool]:
        return impl.decode_bool(s)

    @classmethod
    def decode_int32(cls, s: S, impl: DecoderImpl[S, R]) -> Tuple[S, int]:
        return impl.decode_int32(s)

    @classmethod
    def decode_double(cls, s: S, impl: DecoderImpl[S, R]) -> Tuple[S, float]:
        return impl.decode_double(s)

    @classmethod
    def decode_string(cls, s: S, impl: DecoderImpl[S, R]) -> Tuple[S, str]:
        return impl.decode_string(s)

    @classmethod
    def decode_maybe(cls, decoder: Decoder):
        def decode_maybe_a(s: S, impl: DecoderImpl[S, R],
                           ) -> Tuple[S, Optional[A]]:
            return impl.decode_maybe(s, lambda s1: decoder(s1, impl))
        return decode_maybe_a

    @classmethod
    def decode_list(cls, decoder: Decoder):
        def decode_list_a(s: S, impl: DecoderImpl[S, R]) -> Tuple[S, List[A]]:
            return impl.decode_list(s, lambda s1: decoder(s1, impl))
        return decode_list_a


def encode(a: A, impl: EncoderImpl[S, R], encoder: Encoder) -> R:
    return impl.encode_with_state(lambda s: encoder(s, a, impl))


def decode(r: R, impl: DecoderImpl[S, R], decoder: Decoder) -> A:
    return impl.decode_with_state(r, lambda s: decoder(s, impl))
