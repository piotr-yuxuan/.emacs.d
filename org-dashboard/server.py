#!/usr/bin/env python3
"""Serve the read-only Org task dashboard on the loopback interface."""

from __future__ import annotations

import argparse
import json
import re
import shutil
import subprocess
from http import HTTPStatus
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
from pathlib import Path
from typing import Any


STATIC_FILES = {
    "/": ("index.html", "text/html; charset=utf-8"),
    "/app.js": ("app.js", "text/javascript; charset=utf-8"),
    "/styles.css": ("styles.css", "text/css; charset=utf-8"),
}
TASK_ID_PATTERN = re.compile(r"[0-9a-f]{16}-[1-9][0-9]*\Z")
MAX_BODY_BYTES = 4096


class DashboardRequestError(ValueError):
    """An invalid or unsafe dashboard request."""


def load_manifest(data_file: Path) -> dict[str, Any]:
    """Load and minimally validate the generated dashboard payload."""
    try:
        payload = json.loads(data_file.read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError) as error:
        raise DashboardRequestError(f"Could not read task data: {error}") from error
    if payload.get("schemaVersion") != 1 or not isinstance(payload.get("tasks"), list):
        raise DashboardRequestError("Unsupported task data format")
    return payload


def command_for_task(task_id: str, data_file: Path, emacsclient: str) -> list[str]:
    """Resolve TASK_ID from DATA_FILE and return a shell-free open command."""
    if not isinstance(task_id, str) or TASK_ID_PATTERN.fullmatch(task_id) is None:
        raise DashboardRequestError("Invalid task identifier")

    task = next(
        (candidate for candidate in load_manifest(data_file)["tasks"] if candidate.get("id") == task_id),
        None,
    )
    if task is None:
        raise DashboardRequestError("Unknown task identifier")

    file_value = task.get("file")
    line_value = task.get("line")
    if not isinstance(file_value, str) or not Path(file_value).is_absolute():
        raise DashboardRequestError("Task source is not an absolute path")
    source = Path(file_value)
    if not source.is_file():
        raise DashboardRequestError("Task source no longer exists")
    if not isinstance(line_value, int) or isinstance(line_value, bool) or line_value < 1:
        raise DashboardRequestError("Task line is invalid")

    return [emacsclient, "--no-wait", f"+{line_value}", str(source)]


def refresh_command(emacsclient: str) -> list[str]:
    """Return the fixed command used to refresh generated Org task data."""
    return [emacsclient, "--eval", "(my/org-dashboard-export)"]


class DashboardServer(ThreadingHTTPServer):
    """HTTP server carrying immutable dashboard configuration."""

    daemon_threads = True

    def __init__(self, address: tuple[str, int], assets: Path, data_file: Path, emacsclient: str):
        super().__init__(address, DashboardHandler)
        self.assets = assets
        self.data_file = data_file
        self.emacsclient = emacsclient


class DashboardHandler(BaseHTTPRequestHandler):
    """Serve fixed assets and two narrow local API operations."""

    server: DashboardServer

    def _send_bytes(
        self,
        status: HTTPStatus,
        content: bytes,
        content_type: str,
        *,
        cache_control: str = "no-store",
    ) -> None:
        self.send_response(status)
        self.send_header("Content-Type", content_type)
        self.send_header("Content-Length", str(len(content)))
        self.send_header("Cache-Control", cache_control)
        self.send_header("X-Content-Type-Options", "nosniff")
        self.send_header("Content-Security-Policy", "default-src 'self'; frame-ancestors 'none'")
        self.send_header("Referrer-Policy", "no-referrer")
        self.end_headers()
        self.wfile.write(content)

    def _send_json(self, status: HTTPStatus, payload: dict[str, Any]) -> None:
        content = json.dumps(payload, ensure_ascii=False).encode("utf-8")
        self._send_bytes(status, content, "application/json; charset=utf-8")

    def _read_json(self) -> dict[str, Any]:
        content_type = self.headers.get("Content-Type", "").split(";", maxsplit=1)[0].strip()
        if content_type != "application/json":
            raise DashboardRequestError("Content-Type must be application/json")
        try:
            length = int(self.headers.get("Content-Length", "0"))
        except ValueError as error:
            raise DashboardRequestError("Invalid Content-Length") from error
        if length < 0 or length > MAX_BODY_BYTES:
            raise DashboardRequestError("Request body is too large")
        try:
            payload = json.loads(self.rfile.read(length).decode("utf-8"))
        except (UnicodeDecodeError, json.JSONDecodeError) as error:
            raise DashboardRequestError("Invalid JSON body") from error
        if not isinstance(payload, dict):
            raise DashboardRequestError("JSON body must be an object")
        return payload

    def _same_origin(self) -> bool:
        origin = self.headers.get("Origin")
        allowed = {
            f"http://127.0.0.1:{self.server.server_port}",
            f"http://localhost:{self.server.server_port}",
        }
        return origin is None or origin in allowed

    def do_GET(self) -> None:  # noqa: N802
        try:
            if self.path == "/api/tasks":
                self._send_json(HTTPStatus.OK, load_manifest(self.server.data_file))
                return
            static_file = STATIC_FILES.get(self.path)
            if static_file is None:
                self._send_json(HTTPStatus.NOT_FOUND, {"error": "Not found"})
                return
            filename, content_type = static_file
            content = (self.server.assets / filename).read_bytes()
            self._send_bytes(HTTPStatus.OK, content, content_type, cache_control="no-cache")
        except (DashboardRequestError, OSError) as error:
            self._send_json(HTTPStatus.INTERNAL_SERVER_ERROR, {"error": str(error)})

    def do_POST(self) -> None:  # noqa: N802
        try:
            if not self._same_origin():
                raise DashboardRequestError("Cross-origin request rejected")
            payload = self._read_json()
            if self.path == "/api/open":
                command = command_for_task(payload.get("id"), self.server.data_file, self.server.emacsclient)
                subprocess.Popen(
                    command,
                    stdin=subprocess.DEVNULL,
                    stdout=subprocess.DEVNULL,
                    stderr=subprocess.DEVNULL,
                    close_fds=True,
                )
                self._send_json(HTTPStatus.ACCEPTED, {"ok": True})
                return
            if self.path == "/api/refresh":
                subprocess.run(
                    refresh_command(self.server.emacsclient),
                    check=True,
                    capture_output=True,
                    text=True,
                    timeout=20,
                )
                self._send_json(HTTPStatus.OK, load_manifest(self.server.data_file))
                return
            self._send_json(HTTPStatus.NOT_FOUND, {"error": "Not found"})
        except DashboardRequestError as error:
            self._send_json(HTTPStatus.BAD_REQUEST, {"error": str(error)})
        except (OSError, subprocess.SubprocessError) as error:
            self._send_json(HTTPStatus.BAD_GATEWAY, {"error": f"Emacs request failed: {error}"})


def parse_args() -> argparse.Namespace:
    """Parse command-line arguments."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--data", required=True, type=Path, help="Generated Org task JSON")
    parser.add_argument("--port", type=int, default=8765, help="Loopback TCP port")
    return parser.parse_args()


def main() -> None:
    """Run the dashboard server until interrupted."""
    args = parse_args()
    emacsclient = shutil.which("emacsclient")
    if emacsclient is None:
        raise SystemExit("emacsclient is required")
    server = DashboardServer(
        ("127.0.0.1", args.port),
        assets=Path(__file__).resolve().parent,
        data_file=args.data.expanduser().resolve(),
        emacsclient=emacsclient,
    )
    print(f"Org dashboard listening on http://127.0.0.1:{args.port}/", flush=True)
    try:
        server.serve_forever()
    except KeyboardInterrupt:
        pass
    finally:
        server.server_close()


if __name__ == "__main__":
    main()
