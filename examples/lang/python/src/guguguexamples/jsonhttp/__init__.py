from dataclasses import dataclass
from typing import Generic, TypeVar, Dict

A = TypeVar("A")


@dataclass()
class WithMeta(Generic[A]):
    meta: Dict[str, str]
    data: A
