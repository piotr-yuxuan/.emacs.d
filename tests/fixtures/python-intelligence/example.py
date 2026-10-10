import sys
import os
from abc import ABC, abstractmethod

GLOBAL_VALUE = 1


class Base(ABC):
    @abstractmethod
    def run(self) -> int: ...


class Left(Base):
    def __init__(self) -> None:
        self.value = 1

    def run(self) -> int:
        local_value = GLOBAL_VALUE
        return local_value


class Right(Base):
    def run(self) -> int:
        return 2


class Diamond(Left, Right):
    pass


def result() -> int:
    return 1


result()
