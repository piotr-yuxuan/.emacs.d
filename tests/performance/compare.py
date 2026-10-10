#!/usr/bin/env python3
"""Alternate startup variants to distinguish changes from time/order effects."""
import argparse
import json
from pathlib import Path
import statistics

from run import LIVE, launch, manifest, measurement_display, prepare, quantile

parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument("--output", type=Path, required=True)
parser.add_argument("sources", type=Path, nargs="+")
parser.add_argument("--daemon", action="store_true",
                    help="time daemon boot to readiness instead of a framed start")
parser.add_argument("--user-display", action="store_true",
                    help="draw frames on the inherited display, not on Xvfb")
args = parser.parse_args()
args.output.mkdir(parents=True, exist_ok=False)
with measurement_display(user_display=args.user_display):
    variants = []
    for index, source in enumerate(args.sources):
        folder = (args.output / str(index)).resolve()
        folder.mkdir()
        (folder / "manifest.json").write_text(json.dumps(manifest(source.resolve()), indent=2))
        session = prepare(source.resolve(), folder / "session", LIVE)
        variants.append((folder, session, []))
    for index in range(32):
        order = variants if index % 2 == 0 else list(reversed(variants))
        for folder, session, rows in order:
            report, process = launch(session, folder, index, keep=args.daemon, daemon=args.daemon)
            if args.daemon:
                process.terminate()
                process.wait(timeout=10)
            report["warmup"] = index < 2
            (folder / f"{index:03d}.json").write_text(json.dumps(report, indent=2) + "\n")
            rows.append(report)
            print(folder.name, index, report["ready_seconds"], flush=True)
    for folder, _, rows in variants:
        values = [r["ready_seconds"] for r in rows if not r["warmup"]]
        summary = {"samples": len(values), "median": statistics.median(values),
                   "p95": quantile(values, .95), "maximum": max(values), "failures": []}
        (folder / "summary.json").write_text(json.dumps(summary, indent=2) + "\n")
        print(folder.name, summary)
