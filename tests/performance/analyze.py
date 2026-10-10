#!/usr/bin/env python3
"""Summarize raw runtime samples; errors and warm-up exclusions stay explicit."""
import argparse
import collections
import gzip
import json
from pathlib import Path
import statistics

from run import quantile


def distribution(values):
    return {"count": len(values), "median_ms": statistics.median(values) * 1000,
            "p95_ms": quantile(values, .95) * 1000, "maximum_ms": max(values) * 1000} if values else {}


def summarize(path, skip=0):
    content = gzip.open(path, "rt").read() if path.suffix == ".gz" else path.read_text()
    rows = [json.loads(line) for line in content.splitlines()]
    valid = [r for r in rows if "samples" in r]
    warm = [r for r in rows[skip:] if "samples" in r and not r.get("first_open")]
    commands = [(r["file"], *s) for r in warm for s in r["samples"]]
    errors = collections.Counter(e for r in rows for e in ([r["error"]] if "error" in r else r.get("errors", [])))
    files = sorted({r["file"] for r in valid})
    return {
        "rows": len(rows), "excluded_initial_rows": skip,
        "errors": dict(errors),
        "subsequent_commands": distribution([s[2] for s in commands]),
        "per_file": {name: distribution([s[2] for s in commands if s[0] == name]) for name in files},
        "first_open_ms": {r["file"]: r["open_seconds"] * 1000 for r in valid if r.get("first_open")},
        "over_50ms": len([s for s in commands if s[2] > .05]),
        "largest_commands": sorted(commands, key=lambda s: s[2], reverse=True)[:20],
        "rss_kb": {"first": valid[0]["rss_kb"], "last": valid[-1]["rss_kb"],
                   "maximum": max(r["rss_kb"] for r in valid)},
        "gc_seconds_last": valid[-1]["gc_seconds"],
        "gc_count_last": valid[-1]["gc_count"],
        "loop_lag_max_ms": max(r.get("loop_lag_max", 0) for r in warm) * 1000 if warm else None,
        "last_workspaces": {name: next(r.get("workspaces", []) for r in reversed(valid) if r["file"] == name) for name in files},
    }


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("input", type=Path)
    parser.add_argument("--skip-samples", type=int, default=0)
    args = parser.parse_args()
    print(json.dumps(summarize(args.input, args.skip_samples), indent=2))
