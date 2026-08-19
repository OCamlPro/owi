#!/usr/bin/env bash
# Opens the trace viewer in a browser with a given trace JSON file already
# loaded, so you don't have to open index.html and click "Load trace"
# by hand.
#
# Usage: trace-viewer/open-trace.sh path/to/trace.json

set -euo pipefail

if [ "$#" -ne 1 ]; then
  echo "Usage: $0 <trace.json>" >&2
  exit 1
fi

json="$1"

if [ ! -f "$json" ]; then
  echo "No such file: $json" >&2
  exit 1
fi

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
tmp_dir="$(mktemp -d)"

cp "$script_dir/style.css" "$script_dir/app.js" "$tmp_dir/"
cp -r "$script_dir/prism" "$tmp_dir/"

{
  echo '<script type="application/json" id="embedded-trace">'
  cat "$json"
  echo '</script>'
} > "$tmp_dir/embedded-trace.html"

awk -v inc="$tmp_dir/embedded-trace.html" '
  /<script src="app.js"><\/script>/ { while ((getline line < inc) > 0) print line; close(inc) }
  { print }
' "$script_dir/index.html" > "$tmp_dir/index.html"

rm "$tmp_dir/embedded-trace.html"

opener=""
for candidate in xdg-open open start; do
  if command -v "$candidate" >/dev/null 2>&1; then
    opener="$candidate"
    break
  fi
done

if [ -z "$opener" ]; then
  echo "Could not find a way to open a browser; open this file manually:" >&2
  echo "$tmp_dir/index.html" >&2
  exit 1
fi

"$opener" "$tmp_dir/index.html" >/dev/null 2>&1 &
