# Vendored Emacs `etc/images` Tree

This directory contains the `etc/images/` tree from GNU Emacs, vendored
as GUI icon and splash-screen assets for nelisp-emacs's GUI/TUI frontend
work (toolbar icons, mode-line indicators, splash screen).

## Source

GNU Emacs 30.1, distributed under the GNU General Public License v3 or
later (individual icon sets carry their own upstream license — see
"License" below and `images/README`).

- Upstream tarball: `https://ftp.gnu.org/gnu/emacs/emacs-30.1.tar.xz`
- Retrieved: 2026-07-06
- `sha256sum emacs-30.1.tar.xz`:
  `6ccac1ae76e6af93c6de1df175e8eb406767c23da3dd2a16aa67e3124a6f138f`
- Extracted subtree: `emacs-30.1/etc/images/` (verbatim, all
  subdirectories included) plus `emacs-30.1/etc/COPYING`.

This mirrors the existing `vendor/emacs-lisp/` vendoring (GNU Emacs
30.1 `lisp/` tree) so that the `lisp/` and `etc/images/` provenance stay
version-matched for the same upstream release; `vendor/tramp` in this
tree is `2.7.1.30.1`, i.e. also derived from Emacs 30.1.

## Why vendor

Task #18 session C (GUI icon + splash asset prep) needs the classic
toolbar icon set (save/open/new/cut/copy/paste/search/undo/close/
diropen/...), the `symbols/` SVG+PBM icon pairs used by `icons.el`, and
`splash.svg`/`splash.png`/... for the nelisp-emacs GUI bridge and splash
screen. Rather than hand-recreating or re-licensing new icon art,
vendor the upstream Emacs asset tree exactly as `vendor/emacs-lisp/`
already vendors the upstream Elisp tree.

## Modifications

None. Files are byte-identical to the upstream tarball's
`etc/images/` subtree. File count and per-file SHA-256 were diffed
against the tarball extraction before commit (604 files under
`images/`, 35 directories including the top-level one) and match
exactly.

## Layout

Mirrors upstream `etc/images/`:

```
images/
  *.xpm, *.pbm, *.png, *.bmp, *.svg, *.xbm  — top-level classic toolbar
                                               icons + splash assets
  README                — upstream provenance / recipe notes (per-icon
                           source attribution: Emacs-original / GTK+ 2.x /
                           GNOME 2.x / Adwaita icon theme)
  custom/, ezimage/, gnus/, gud/, icons/, low-color/, mail/, mpc/,
  newsticker/, smilies/, symbols/, tabs/, tree-widget/
                         — subsystem-specific icon sets, each with its
                           own README where license/attribution differs
                           from the top-level set
COPYING                  — GPL-3.0, copied from `emacs-30.1/etc/COPYING`
```

Format breakdown at the top level: 75 `.pbm`, 65 `.xpm`, 15 `.svg`,
1 `.png` (`icons.png`), 1 `.bmp` (`splash.bmp`). `splash.{svg,png,pbm,
xpm,bmp}` (all 5 formats) live at `images/splash.*`. The classic
toolbar icon set is XPM (24-bit) with PBM (monochrome, low-color
fallback) pairs, e.g. `images/save.xpm` + `images/save.pbm`.
`images/symbols/` holds 24 SVG+PBM icon pairs used by `icons.el`
(e.g. `check-mark_16.svg` / `check-mark_16.pbm`).

## License

Mixed, all copyleft, inherited from upstream Emacs — see
`images/README` for full per-file attribution:

- Emacs-original icons (e.g. `mh-logo.xpm`, `gnus.pbm`, `splash.*`,
  `checked.xpm`/`unchecked.xpm`): GPL-3.0-or-later, `COPYING` at this
  directory's top.
- GTK+ 2.x icons (classic toolbar set: `close.xpm`, `copy.xpm`,
  `cut.xpm`, `new.xpm`, `open.xpm`, `save.xpm`, `search.xpm`,
  `undo.xpm`, ...): LGPL-2.0-or-later (GTK+ source license).
- GNOME 2.x icon-theme derived icons (`attach.xpm`, `delete.xpm`,
  `refresh.xpm`, `zoom-in.xpm`, ...): GPL-2.0-or-later.
- Adwaita Icon Theme derived SVGs (`checked.svg`, `radio.svg`,
  `conceal.svg`, ...): LGPL-3.0-or-later or CC-BY-SA-3.0.

Downstream consumers selecting individual icon files for redistribution
should check `images/README` (and subdirectory READMEs) for the
applicable license per file; this vendor copy as a whole is safe to
carry under the repository's existing GPL-3.0 vendoring precedent.
