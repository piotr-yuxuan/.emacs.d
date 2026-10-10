#!/usr/bin/env python3
"""Copy installed Straight state once; never install or update packages."""
import argparse
from pathlib import Path
import subprocess
import sys

from run import LIVE, ROOT

parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument("--output", type=Path, default=ROOT / "tmp/package-snapshot")
args = parser.parse_args()
destination = args.output.resolve()
if destination.exists():
    raise SystemExit(f"Snapshot already exists; choose a new directory: {destination}")
if sys.platform.startswith("linux"):
    flags = ["-a", "--reflink=auto"]
elif sys.platform == "darwin":
    flags = ["-a"]
else:
    raise SystemExit(f"Snapshot copying is untested on {sys.platform}")
destination.mkdir(parents=True)
subprocess.run(["cp", *flags, str(LIVE / "straight"), str(destination / "straight")], check=True)
print(destination)
