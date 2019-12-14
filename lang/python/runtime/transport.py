from abc import ABC, abstractmethod
from typing import Generic, TypeVar, List
from dataclasses import dataclass

A = TypeVar("A")


@dataclass()
class QualName(Generic[A]):
    namespace: List[A]
    name: A
