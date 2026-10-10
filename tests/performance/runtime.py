#!/usr/bin/env python3
"""Start an isolated graphical session and run editing probes via emacsclient."""

import argparse
import json
from pathlib import Path
import subprocess
import shutil
import time

from run import LIVE, ROOT, elisp_string, launch, manifest, measurement_display, prepare


def evaluate(socket, expression, timeout=60):
    result = subprocess.run(["emacsclient", "--socket-name", str(socket), "--eval", expression],
                            text=True, capture_output=True, timeout=timeout)
    if result.returncode:
        raise RuntimeError(result.stderr or result.stdout)
    return result.stdout.strip()


def fixtures(folder, large=False, long_lines=False):
    count = 1500 if large else 150
    texts = {
        "sample.clj": "(ns sample)\n" + "\n".join(f"(defn value-{i} [x] (+ x {i}))" for i in range(count)),
        "sample.py": "import math\n" + "\n".join(f"def value_{i}(x: float) -> float:\n    return x + {i}\n" for i in range(count // 3)),
        "sample.nix": "{ lib, ... }: {\n" + "\n".join(f"  value{i} = {i};" for i in range(count)) + "\n}\n",
        "main.tofu": "\n".join(f'locals {{ value{i} = {i} }}' for i in range(count)),
        "Dockerfile": "FROM scratch\n" + "\n".join(f"ENV VALUE{i}={i}" for i in range(count)),
        "sample.sh": "#!/usr/bin/env bash\n" + "\n".join(f"value_{i}() {{ echo {i}; }}" for i in range(count)),
        "sample.org": "#+TITLE: Performance fixture\n" + "\n".join(f"* TODO Task {i}\nText with *emphasis*, [[https://example.com][a link]], and =code=.\n" for i in range(count)),
    }
    if long_lines:
        # Comments exercise fontification and long-line guards without
        # inventing invalid language syntax or allocating huge source trees.
        for name in texts:
            prefix = ";; " if name.endswith(".clj") else "# "
            first, _, remainder = texts[name].partition("\n")
            texts[name] = first + "\n" + "\n".join(
                prefix + "long line " * 1000 for _ in range(20)) + "\n" + remainder
    paths = []
    for name, content in texts.items():
        project = folder / name.replace(".", "-")
        project.mkdir(parents=True, exist_ok=True)
        subprocess.run(["git", "init", "-q", str(project)], check=True)
        path = project / name
        path.write_text(content + "\n")
        if name.endswith(".clj"):
            (project / "deps.edn").write_text('{:paths ["."]}\n')
        if name.endswith(".py"):
            (project / "pyproject.toml").write_text('[project]\nname = "emacs-perf"\nversion = "0.0.0"\n')
        paths.append(path)
    return paths


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--source", type=Path, default=ROOT)
    parser.add_argument("--output", type=Path, required=True)
    parser.add_argument("--seconds", type=int, default=60)
    parser.add_argument("--profile", action="store_true")
    parser.add_argument("--profile-delay", type=float, default=0)
    parser.add_argument("--gc-mib", type=int)
    parser.add_argument("--large", action="store_true")
    parser.add_argument("--long-lines", action="store_true")
    parser.add_argument("--copy", type=Path, action="append", default=[])
    parser.add_argument("--interval", type=float, default=.5)
    parser.add_argument("--user-display", action="store_true",
                        help="draw frames on the inherited display, not on Xvfb")
    args = parser.parse_args()
    folder = args.output.resolve()
    if args.seconds < 1 or args.interval <= 0:
        parser.error("seconds and interval must be positive")
    if not 0 <= args.profile_delay < args.seconds:
        parser.error("profile delay must be nonnegative and shorter than the workload")
    folder.mkdir(parents=True, exist_ok=False)
    with measurement_display(user_display=args.user_display):
        metadata = manifest(args.source.resolve())
        metadata.update(seconds=args.seconds, interval=args.interval, large=args.large,
                        long_lines=args.long_lines, profiled=args.profile,
                        profile_delay=args.profile_delay, gc_mib=args.gc_mib)
        (folder / "manifest.json").write_text(json.dumps(metadata, indent=2) + "\n")
        session = prepare(args.source.resolve(), folder / "session", LIVE)
        _, process = launch(session, folder, 0, keep=True)
        socket = Path((session / "socket-directory").read_text()) / "perf-session"
        try:
            deadline = time.monotonic() + 15
            while not socket.exists():
                if time.monotonic() > deadline:
                    raise TimeoutError("The private Emacs server did not start")
                time.sleep(.1)
            evaluate(socket, f"(load {elisp_string(ROOT / 'tests/performance/runtime.el')} nil t)")
            evaluate(socket, f"(load {elisp_string(ROOT / 'tests/performance/post-workload.el')} nil t)")
            if args.gc_mib:
                evaluate(socket, f"(setq gc-cons-threshold {args.gc_mib * 1024 * 1024})")
            files = fixtures(folder / "fixtures", args.large, args.long_lines)
            for index, original in enumerate(args.copy):
                project = folder / "fixtures" / f"copied-{index}"
                project.mkdir()
                subprocess.run(["git", "init", "-q", str(project)], check=True)
                destination = project / original.name
                shutil.copy2(original, destination)
                files.append(destination)
            evaluate(socket, f"(setq perf-runtime-interval {args.interval} perf-runtime-profile-delay {args.profile_delay})")
            expression = f"(perf-runtime-start '( {' '.join(elisp_string(p) for p in files)} ) {elisp_string(folder / 'runtime.jsonl')} {args.seconds} {'t' if args.profile else 'nil'})"
            print(evaluate(socket, expression), flush=True)
            deadline = time.monotonic() + args.seconds + 120
            while not (folder / "runtime.jsonl.done").exists():
                if process.poll() is not None:
                    raise RuntimeError(f"The isolated editor exited {process.returncode} during the workload")
                if time.monotonic() > deadline:
                    raise TimeoutError("Workload exceeded its deadline; inspect the isolated editor's log")
                time.sleep(1)
            print((folder / "runtime.jsonl.done").read_text(), flush=True)
            print(evaluate(socket, "(list :gc-threshold gc-cons-threshold :gc-time gc-elapsed :gcs gcs-done)"), flush=True)
        finally:
            try:
                evaluate(socket, "(kill-emacs 0)", timeout=5)
            except (RuntimeError, subprocess.TimeoutExpired):
                process.terminate()
            process.wait(timeout=10)


if __name__ == "__main__":
    main()
