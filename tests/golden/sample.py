"""A golden fixture of Python: a small inventory module."""

from __future__ import annotations

import dataclasses
import sys
from pathlib import Path

LIMIT = 0x10  # at most sixteen items per shelf


@dataclasses.dataclass(frozen=True)
class Item:
    """One item on a shelf."""

    name: str
    price: float = 1.5
    tags: tuple[str, ...] = ()

    def label(self, currency: str = "EUR") -> str:
        """Return the label printed on the shelf."""
        return f"{self.name}: {self.price:.2f} {currency}"


def load(path: Path) -> list[Item]:
    """Read one item per line of PATH, as NAME,PRICE."""
    items = []
    for line in path.read_text(encoding="utf-8").splitlines():
        if not line or line.startswith("#"):
            continue
        name, price = line.split(",", maxsplit=1)
        items.append(Item(name.strip(), float(price)))
    if len(items) > LIMIT:
        message = f"too many items: {len(items)}"
        raise ValueError(message)
    return sorted(items, key=lambda item: (item.price, item.name))


if __name__ == "__main__":
    sys.stdout.writelines(f"{item.label()}\n" for item in load(Path("items.csv")))
