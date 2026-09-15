"""Self-contained searchable HTML from nonexecuting source declarations."""
import hashlib
from html import escape
import json
import os
from pathlib import Path
import tempfile


def render(report):
    articles = []
    for number, symbol in enumerate(report["symbols"]):
        title = escape(symbol["symbol"])
        signature = escape(symbol["signature"])
        location = escape(f"{symbol['path']}:{symbol['start']['line']}")
        articles.append(
            f'<article id="symbol-{number}"><h2>{title} <code>{signature}</code></h2>'
            f'<p>{escape(symbol["kind"])} · {location}</p>'
            f'<pre>{escape(symbol["documentation"])}</pre></article>')
    return ('''<!doctype html>
<html lang="en"><meta charset="utf-8"><meta name="viewport" content="width=device-width">
<title>''' + escape(report["package"]) + ''' API</title>
<style>body{font:17px system-ui;max-width:960px;margin:3rem auto;padding:0 1rem;color:#202532}
input{font:inherit;padding:.6rem;width:calc(100% - 1.4rem)}article{border-top:1px solid #ccd2dc;padding:1rem 0}
pre{white-space:pre-wrap;font:inherit;line-height:1.6}code{font-size:.8em}p{color:#4d5666}</style>
<h1>''' + escape(report["package"]) + ''' API</h1>
<p>Source declarations only; macros are not expanded and code is not executed.</p>
<label for="search">Search names, signatures, documentation, and source paths</label>
<input id="search" type="search" autocomplete="off"><p id="count" role="status"></p>
<main>''' + "\n".join(articles) + '''</main>
<script>
const search = document.getElementById('search');
const entries = Array.from(document.querySelectorAll('article'));
const index = entries.map(entry => entry.textContent.toLowerCase());
function filter() {
  const query = search.value.toLowerCase().trim();
  let count = 0;
  entries.forEach((entry, i) => {
    entry.hidden = !index[i].includes(query);
    if (!entry.hidden) count++;
  });
  document.getElementById('count').textContent = `${count} / ${entries.length} declarations`;
}
search.addEventListener('input', filter);
filter();
</script></html>
''')


def generate(root, manifest, snapshots, plan):
    """Publish deterministic HTML and API JSON after all sources were parsed."""
    report = {"schema_version": 1, "scope": "top-level-source-declarations",
              "package": manifest["package"]["name"], "files": [], "symbols": []}
    for item, (path, content) in zip(plan, snapshots.items(), strict=True):
        relative = path.relative_to(root).as_posix()
        report["files"].append({"path": relative, "sha256": hashlib.sha256(content).hexdigest()})
        for symbol in item["symbols"]:
            report["symbols"].append(dict(symbol, path=relative))
    output = root / "target" / "doc"
    if not output.resolve().is_relative_to(root):
        raise ValueError("documentation output must resolve inside the project")
    page = render(report).encode("utf-8")
    data = (json.dumps(report, ensure_ascii=False, indent=2, sort_keys=True) + "\n").encode("utf-8")
    output.mkdir(parents=True, exist_ok=True)
    with tempfile.TemporaryDirectory(prefix=".doc-", dir=output) as temporary:
        stage = Path(temporary)
        (stage / "index.html").write_bytes(page)
        (stage / "api.json").write_bytes(data)
        os.replace(stage / "api.json", output / "api.json")
        os.replace(stage / "index.html", output / "index.html")
    return output / "index.html", len(report["symbols"])
