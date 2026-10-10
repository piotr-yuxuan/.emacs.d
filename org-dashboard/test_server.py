"""Tests for the local Org dashboard server helpers."""

from __future__ import annotations

import json
import sys
import tempfile
import unittest
from pathlib import Path


sys.path.insert(0, str(Path(__file__).resolve().parent))

from server import DashboardRequestError, command_for_task, load_manifest, refresh_command  # noqa: E402


class DashboardServerTest(unittest.TestCase):
    def setUp(self) -> None:
        self.temporary_directory = tempfile.TemporaryDirectory()
        self.addCleanup(self.temporary_directory.cleanup)
        self.root = Path(self.temporary_directory.name)
        self.source = self.root / "todo.org"
        self.source.write_text("* NEXT Do the work\n", encoding="utf-8")
        self.data = self.root / "tasks.json"
        self.data.write_text(
            json.dumps(
                {
                    "schemaVersion": 1,
                    "tasks": [
                        {
                            "id": "0123456789abcdef-1",
                            "file": str(self.source),
                            "line": 1,
                        }
                    ],
                }
            ),
            encoding="utf-8",
        )

    def test_loads_versioned_manifest(self) -> None:
        self.assertEqual(load_manifest(self.data)["schemaVersion"], 1)

    def test_resolves_open_command_without_a_shell(self) -> None:
        self.assertEqual(
            command_for_task("0123456789abcdef-1", self.data, "/bin/emacsclient"),
            ["/bin/emacsclient", "--no-wait", "+1", str(self.source)],
        )

    def test_rejects_unknown_task(self) -> None:
        with self.assertRaisesRegex(DashboardRequestError, "Unknown"):
            command_for_task("0123456789abcdef-9", self.data, "/bin/emacsclient")

    def test_rejects_relative_source(self) -> None:
        payload = load_manifest(self.data)
        payload["tasks"][0]["file"] = "todo.org"
        self.data.write_text(json.dumps(payload), encoding="utf-8")
        with self.assertRaisesRegex(DashboardRequestError, "absolute"):
            command_for_task("0123456789abcdef-1", self.data, "/bin/emacsclient")

    def test_refresh_uses_a_fixed_expression(self) -> None:
        self.assertEqual(
            refresh_command("/bin/emacsclient"),
            ["/bin/emacsclient", "--eval", "(my/org-dashboard-export)"],
        )


if __name__ == "__main__":
    unittest.main()
