#!/usr/bin/env bash
# A golden fixture of shell: back up a directory, keeping a few copies.
set -euo pipefail

readonly KEEP=3
target="${1:-$HOME/notes}"

backup() {
  local source="$1" stamp
  stamp=$(date +%Y%m%d)
  tar -czf "${source}-${stamp}.tar.gz" "$source"
  echo "saved $source at $stamp"
}

if [[ ! -d "$target" ]]; then
  echo "no directory: $target" >&2
  exit 1
fi

count=0
for archive in "$target"-*.tar.gz; do
  case "$archive" in
    *-old-*) rm -- "$archive" ;;
    *) count=$((count + 1)) ;;
  esac
done

while read -r line; do
  printf '%s\n' "$line"
done <<EOF
kept $KEEP copies
EOF

backup "$target"
