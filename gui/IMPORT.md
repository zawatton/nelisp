# nelisp-gui import note

This tree was imported from the sibling `nelisp-gui` checkout during the
`nelisp-emacs` library-first monorepo phase.

- Original branch: `feat/org-roam-lite-indexer`
- Original HEAD: `8ff3450 feat(launcher): default org-roam corpus probe so link-following works out of box`
- Import location: `gui/`
- Original repository backup: `../nelisp-gui.pre-monorepo-backup`

The `gui/` tree is intended to be a consumer of the reusable
`nelisp-emacs` libraries.  GUI code should own transport and rendering,
while buffer state, command dispatch, keymaps, minibuffer semantics,
undo, file command behavior, and window layout belong in shared runtime
modules.
