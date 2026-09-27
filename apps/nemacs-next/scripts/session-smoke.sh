#!/usr/bin/env sh
set -eu

ROOT=${NEMACS_NEXT_ROOT:-$(CDPATH= cd -- "$(dirname -- "$0")/../../.." && pwd -P)}
NELISP_BIN=${NELISP_BIN:-${NELISP:-}}
BOOTSTRAP_REPL=${NEMACS_BOOTSTRAP_REPL:-$ROOT/build/nemacs-bootstrap.repl}

if [ -z "$NELISP_BIN" ]; then
  if [ -x "$ROOT/../nelisp/target/nelisp" ]; then
    NELISP_BIN=$ROOT/../nelisp/target/nelisp
  elif [ -x "$ROOT/vendor/nelisp/target/nelisp" ]; then
    NELISP_BIN=$ROOT/vendor/nelisp/target/nelisp
  else
    NELISP_BIN=$ROOT/../nelisp/target/nelisp
  fi
fi

if [ ! -x "$NELISP_BIN" ]; then
  echo "nemacs-next-session-smoke: nelisp binary is not executable: $NELISP_BIN" >&2
  exit 1
fi

if [ ! -r "$BOOTSTRAP_REPL" ]; then
  echo "nemacs-next-session-smoke: bootstrap REPL is not readable: $BOOTSTRAP_REPL" >&2
  echo "nemacs-next-session-smoke: run make build-nelisp-bootstrap first" >&2
  exit 1
fi

tmp=${TMPDIR:-/tmp}/nemacs-next-session-smoke.$$.repl
out=${TMPDIR:-/tmp}/nemacs-next-session-smoke.$$.out
marker=${TMPDIR:-/tmp}/nemacs-next-session-smoke.$$.sentinel
m3_file=${TMPDIR:-/tmp}/nemacs-next-session-smoke.$$.txt
org_file=${TMPDIR:-/tmp}/nemacs-next-session-smoke.$$.org
mx_file=${TMPDIR:-/tmp}/nemacs-next-session-smoke.$$.mx.txt
mx_ff_file=${TMPDIR:-/tmp}/nemacs-next-session-smoke.$$.mx-ff.txt
rm -f "$tmp" "$out" "$marker" "$m3_file" "$org_file" "$mx_file" "$mx_ff_file"
trap 'rm -f "$tmp" "$out" "$marker" "$m3_file" "$org_file" "$mx_file" "$mx_ff_file"' EXIT
printf 'seed' > "$m3_file"
printf '' > "$mx_file"
printf '' > "$mx_ff_file"
printf '* TODO Project\nBody\n** NEXT Child\n' > "$org_file"

cat "$BOOTSTRAP_REPL" > "$tmp"
cat >> "$tmp" <<EOF
(setq load-path (list "$ROOT/src" "$ROOT/apps/nemacs-next/lisp"))
(setq nemacs-next-session-smoke-count 0)
(load "$ROOT/apps/nemacs-next/lisp/nemacs-next-protocol.el")
(load "$ROOT/src/emacs-startup-screen.el")
(setq nemacs-next-session-smoke-splash-buffer (nemacs-next-session-apply-startup-screen))
(setq nemacs-next-session-smoke-splash-snapshot (nemacs-next-session-buffer-snapshot))
(setq nemacs-next-session-smoke-count (+ nemacs-next-session-smoke-count 1))
(setq nemacs-next-session-smoke-plan (nemacs-next-session-plan))
(setq nemacs-next-session-smoke-count (+ nemacs-next-session-smoke-count 1))
(setq nemacs-next-session-smoke-missing (nemacs-next-missing-required-packages))
(setq nemacs-next-session-smoke-hello (nemacs-next-session-hello))
(setq nemacs-next-session-smoke-buffer
      (nelisp-ec-generate-new-buffer "nemacs-next-session-smoke"))
(nelisp-ec-set-buffer nemacs-next-session-smoke-buffer)
(nelisp-ec-insert "abc")
(setq nemacs-next-session-smoke-count (+ nemacs-next-session-smoke-count 1))
(setq nemacs-next-session-smoke-snapshot
      (nemacs-next-session-buffer-snapshot))
(setq nemacs-next-session-smoke-line
      (nemacs-next-protocol-encode-line nemacs-next-session-smoke-snapshot))
(setq nemacs-next-session-smoke-goto
      (nemacs-next-session-handle-message
       (quote (:type command :name goto-char :position 1))))
(setq nemacs-next-session-smoke-fwd
      (nemacs-next-session-handle-message
       (quote (:type command :name forward-char :count 2))))
(setq nemacs-next-session-smoke-back
      (nemacs-next-session-handle-message
       (quote (:type command :name backward-char :count 1))))
(setq nemacs-next-session-smoke-del
      (nemacs-next-session-handle-message
       (quote (:type command :name delete-char :count 1))))
(setq nemacs-next-session-smoke-oor
      (nemacs-next-session-handle-message
       (quote (:type command :name forward-char :count 999))))
(setq nemacs-next-session-smoke-yank-empty
      (nemacs-next-session-handle-message
       (quote (:type command :name yank))))
(setq nemacs-next-session-smoke-nl
      (nemacs-next-session-handle-message
       (quote (:type command :name newline))))
(setq nemacs-next-session-smoke-undo-nl
      (nemacs-next-session-handle-message
       (quote (:type command :name undo))))
(setq nemacs-next-session-smoke-kill-region
      (nemacs-next-session-handle-message
       (quote (:type command :name kill-region :start 1 :end 2))))
(setq nemacs-next-session-smoke-yank-1
      (nemacs-next-session-handle-message
       (quote (:type command :name yank))))
(setq nemacs-next-session-smoke-kill-line
      (nemacs-next-session-handle-message
       (quote (:type command :name kill-line))))
(setq nemacs-next-session-smoke-yank-2
      (nemacs-next-session-handle-message
       (quote (:type command :name yank))))
(setq nemacs-next-session-smoke-bad-kill
      (nemacs-next-session-handle-message
       (quote (:type command :name kill-region))))
(setq nemacs-next-session-smoke-fresh
      (nemacs-next-session-handle-message
       (quote (:type command :name create-buffer
               :buffer-name "nemacs-next-session-smoke-undo-empty"))))
(setq nemacs-next-session-smoke-undo-empty
      (nemacs-next-session-handle-message
       (quote (:type command :name undo))))
(setq nemacs-next-session-smoke-alive
      (nemacs-next-session-handle-message
       (quote (:type command :name snapshot))))
(setq nemacs-next-session-smoke-find-file
      (nemacs-next-session-handle-message
       (quote (:type command :name find-file :path "$m3_file"))))
(setq nemacs-next-session-smoke-file-append
      (nemacs-next-session-handle-message
       (quote (:type command :name insert-text :text "!"))))
(setq nemacs-next-session-smoke-save
      (nemacs-next-session-handle-message
       (quote (:type command :name save-buffer))))
(setq nemacs-next-session-smoke-buffer-complete
      (nemacs-next-session-handle-message
       (quote (:type command :name complete :purpose buffer
               :input "nemacs-next-session-smoke."))))
(setq nemacs-next-session-smoke-generic-complete
      (nemacs-next-session-handle-message
       (quote (:type command :name complete :input "fi"
               :collection ("find-file" "save-buffer" "switch-to-buffer")))))
(setq nemacs-next-session-smoke-switch
      (nemacs-next-session-handle-message
       (quote (:type command :name switch-to-buffer
               :buffer-name "nemacs-next-session-smoke"))))
(setq nemacs-next-session-smoke-kill-file
      (nemacs-next-session-handle-message
       (list :type (quote command) :name (quote kill-buffer)
             :buffer-name
             (plist-get nemacs-next-session-smoke-find-file :buffer-name))))
(setq nemacs-next-session-smoke-missing-kill
      (nemacs-next-session-handle-message
       (quote (:type command :name kill-buffer
               :buffer-name "missing-buffer"))))
(setq nemacs-next-session-smoke-frame
      (nemacs-next-session-handle-message
       (quote (:type command :name frame-snapshot :width 24 :height 4))))
(setq nemacs-next-session-smoke-rendered-frame
      (nemacs-next-session-render-frame-text
       nemacs-next-session-smoke-frame))
(setq nemacs-next-session-smoke-menu
      (nemacs-next-session-handle-message
       (quote (:type command :name menu))))
(setq nemacs-next-session-smoke-resize
      (nemacs-next-session-handle-message
       (quote (:type resize :width 22 :height 3))))
(setq nemacs-next-session-smoke-input
      (nemacs-next-session-handle-message
       (quote (:type input :event (:text "x")))))
(setq nemacs-next-session-smoke-ime
      (nemacs-next-session-handle-message
       (quote (:type input :event (:commit "y")))))
(setq nemacs-next-session-smoke-clipboard
      (nemacs-next-session-handle-message
       (quote (:type command :name clipboard-read))))
(setq nemacs-next-session-smoke-org-open
      (nemacs-next-session-handle-message
       (quote (:type command :name find-file :path "$org_file"))))
(setq nemacs-next-session-smoke-toolbar-unicode
      (let ((emacs-toolbar-icon-force-mode (quote unicode)))
        (nemacs-next-session-toolbar-render-line 0 t 140)))
(setq nemacs-next-session-smoke-toolbar-ascii
      (let ((emacs-toolbar-icon-force-mode (quote ascii)))
        (nemacs-next-session-toolbar-render-line 0 t 140)))
(setq nemacs-next-session-smoke-mx-visit
      (nemacs-next-session-handle-message
       (quote (:type command :name find-file :path "$mx_file"))))
(setq nemacs-next-session-smoke-mx-seed
      (nemacs-next-session-handle-message
       (quote (:type command :name insert-text :text "hello"))))
(setq nemacs-next-session-smoke-mx-enter
      (nemacs-next-session-handle-message
       (quote (:type command :name execute-extended-command))))
(setq nemacs-next-session-smoke-mx-state-0
      (nemacs-next-session-handle-message
       (quote (:type command :name minibuffer-state))))
(setq nemacs-next-session-smoke-mx-input
      (nemacs-next-session-handle-message
       (quote (:type input :event (:text "sav")))))
(setq nemacs-next-session-smoke-mx-state-1
      (nemacs-next-session-handle-message
       (quote (:type command :name minibuffer-state))))
(setq nemacs-next-session-smoke-mx-tab
      (nemacs-next-session-handle-message
       (quote (:type input :event (:key tab)))))
(setq nemacs-next-session-smoke-mx-state-2
      (nemacs-next-session-handle-message
       (quote (:type command :name minibuffer-state))))
(setq nemacs-next-session-smoke-mx-commit
      (nemacs-next-session-handle-message
       (quote (:type input :event (:key return)))))
(setq nemacs-next-session-smoke-mx-state-3
      (nemacs-next-session-handle-message
       (quote (:type command :name minibuffer-state))))
(setq nemacs-next-session-smoke-mx-enter-2
      (nemacs-next-session-handle-message
       (quote (:type command :name execute-extended-command))))
(setq nemacs-next-session-smoke-mx-abort-input
      (nemacs-next-session-handle-message
       (quote (:type input :event (:text "x")))))
(setq nemacs-next-session-smoke-mx-abort
      (nemacs-next-session-handle-message
       (quote (:type input :event (:key C-g)))))
(setq nemacs-next-session-smoke-mx-state-abort
      (nemacs-next-session-handle-message
       (quote (:type command :name minibuffer-state))))
(setq nemacs-next-session-smoke-mx-enter-3
      (nemacs-next-session-handle-message
       (quote (:type command :name execute-extended-command))))
(setq nemacs-next-session-smoke-mx-unknown-input
      (nemacs-next-session-handle-message
       (quote (:type input :event (:text "totally-bogus-cmd")))))
(setq nemacs-next-session-smoke-mx-unknown
      (nemacs-next-session-handle-message
       (quote (:type input :event (:key return)))))
(setq nemacs-next-session-smoke-mx-enter-4
      (nemacs-next-session-handle-message
       (quote (:type command :name execute-extended-command))))
(setq nemacs-next-session-smoke-mx-ff-input
      (nemacs-next-session-handle-message
       (quote (:type input :event (:text "find-file")))))
(setq nemacs-next-session-smoke-mx-ff-commit
      (nemacs-next-session-handle-message
       (quote (:type input :event (:key return)))))
(setq nemacs-next-session-smoke-mx-ff-state
      (nemacs-next-session-handle-message
       (quote (:type command :name minibuffer-state))))
(setq nemacs-next-session-smoke-mx-ff-path
      (nemacs-next-session-handle-message
       (list :type (quote input) :event (list :text "$mx_ff_file"))))
(setq nemacs-next-session-smoke-mx-ff-final
      (nemacs-next-session-handle-message
       (quote (:type input :event (:key return)))))
(setq nemacs-next-session-smoke-org-parse-ok nil)
(require (quote org))
(when (and (fboundp (quote org-mode))
           (fboundp (quote org-element-parse-buffer)))
  (org-mode)
  (setq nemacs-next-session-smoke-org-parse-ok
        (if (org-element-parse-buffer) t nil)))
(if (and (= nemacs-next-session-smoke-count 3)
         (not nemacs-next-session-smoke-missing)
         (fboundp (quote nemacs-next-session-plan))
         (fboundp (quote nemacs-next-session-buffer-snapshot))
         (eq (plist-get nemacs-next-session-smoke-hello :type)
             (quote hello))
         (= (plist-get nemacs-next-session-smoke-hello :protocol-version)
            nemacs-next-protocol-version)
         (eq (plist-get nemacs-next-session-smoke-snapshot :type)
             (quote snapshot))
         (equal (plist-get nemacs-next-session-smoke-snapshot :buffer-name)
                "nemacs-next-session-smoke")
         (equal (plist-get nemacs-next-session-smoke-snapshot :text)
                "abc")
         (string-match-p "\"type\":\"snapshot\""
                         nemacs-next-session-smoke-line)
         (string-match-p "\"text\":\"abc\""
                         nemacs-next-session-smoke-line)
         (= (plist-get nemacs-next-session-smoke-goto :point) 1)
         (= (plist-get nemacs-next-session-smoke-fwd :point) 3)
         (= (plist-get nemacs-next-session-smoke-back :point) 2)
         (equal (plist-get nemacs-next-session-smoke-del :text) "ac")
         (= (plist-get nemacs-next-session-smoke-del :point) 2)
         (eq (plist-get nemacs-next-session-smoke-oor :type) (quote error))
         (eq (plist-get nemacs-next-session-smoke-oor :code) (quote out-of-range))
         (eq (plist-get nemacs-next-session-smoke-yank-empty :type) (quote error))
         (eq (plist-get nemacs-next-session-smoke-yank-empty :code)
             (quote empty-kill-ring))
         (equal (plist-get nemacs-next-session-smoke-nl :text) "a\nc")
         (= (plist-get nemacs-next-session-smoke-nl :point) 3)
         (equal (plist-get nemacs-next-session-smoke-undo-nl :text) "ac")
         (= (plist-get nemacs-next-session-smoke-undo-nl :point) 2)
         (equal (plist-get nemacs-next-session-smoke-kill-region :text) "c")
         (= (plist-get nemacs-next-session-smoke-kill-region :point) 1)
         (equal (plist-get nemacs-next-session-smoke-yank-1 :text) "ac")
         (= (plist-get nemacs-next-session-smoke-yank-1 :point) 2)
         (equal (plist-get nemacs-next-session-smoke-kill-line :text) "a")
         (= (plist-get nemacs-next-session-smoke-kill-line :point) 2)
         (equal (plist-get nemacs-next-session-smoke-yank-2 :text) "ac")
         (= (plist-get nemacs-next-session-smoke-yank-2 :point) 3)
         (eq (plist-get nemacs-next-session-smoke-bad-kill :type) (quote error))
         (eq (plist-get nemacs-next-session-smoke-bad-kill :code)
             (quote bad-command))
         (eq (plist-get nemacs-next-session-smoke-undo-empty :type) (quote error))
         (eq (plist-get nemacs-next-session-smoke-undo-empty :code)
             (quote no-further-undo-information))
         (eq (plist-get nemacs-next-session-smoke-alive :type) (quote snapshot))
         (eq (plist-get nemacs-next-session-smoke-find-file :type)
             (quote snapshot))
         (equal (plist-get nemacs-next-session-smoke-find-file :file-name)
                "$m3_file")
         (equal (plist-get nemacs-next-session-smoke-find-file :text) "seed")
         (equal (plist-get nemacs-next-session-smoke-file-append :text)
                "seed!")
         (equal (plist-get nemacs-next-session-smoke-save :saved-file)
                "$m3_file")
         (equal (rdf "$m3_file") "seed!")
         (eq (plist-get nemacs-next-session-smoke-buffer-complete :type)
             (quote minibuffer))
         (member (plist-get nemacs-next-session-smoke-find-file :buffer-name)
                 (plist-get nemacs-next-session-smoke-buffer-complete
                            :candidates))
         (equal (plist-get nemacs-next-session-smoke-generic-complete
                           :candidates)
                (quote ("find-file")))
         (equal (plist-get nemacs-next-session-smoke-switch :buffer-name)
                "nemacs-next-session-smoke")
         (eq (plist-get nemacs-next-session-smoke-kill-file :type)
             (quote snapshot))
         (eq (plist-get nemacs-next-session-smoke-missing-kill :type)
             (quote error))
         (eq (plist-get nemacs-next-session-smoke-missing-kill :code)
             (quote no-such-buffer))
         (eq (plist-get nemacs-next-session-smoke-frame :type)
             (quote snapshot))
         (plist-get nemacs-next-session-smoke-frame :frame)
         (string-match-p "nemacs-next-session-smoke"
                         nemacs-next-session-smoke-rendered-frame)
         (eq (plist-get nemacs-next-session-smoke-menu :type)
             (quote menu))
         (equal (plist-get (car (plist-get nemacs-next-session-smoke-menu
                                           :items))
                           :command)
                "find-file")
         (eq (plist-get nemacs-next-session-smoke-resize :type)
             (quote delta))
         (eq (plist-get nemacs-next-session-smoke-input :type)
             (quote delta))
         (eq (plist-get nemacs-next-session-smoke-ime :type)
             (quote delta))
         (eq (plist-get nemacs-next-session-smoke-clipboard :type)
             (quote request))
         (eq (plist-get nemacs-next-session-smoke-org-open :type)
             (quote snapshot))
         nemacs-next-session-smoke-org-parse-ok
         nemacs-next-session-smoke-splash-buffer
         (equal (plist-get nemacs-next-session-smoke-splash-snapshot
                           :buffer-name)
                "*GNU Emacs*")
         (string-match-p "Welcome to nemacs"
                         (plist-get nemacs-next-session-smoke-splash-snapshot
                                    :text))
         (string-match-p "ABSOLUTELY NO WARRANTY"
                         (plist-get nemacs-next-session-smoke-splash-snapshot
                                    :text))
         (string-match-p ">{✚ New File}<" nemacs-next-session-smoke-toolbar-unicode)
         (string-match-p "▶\\|▤\\|✕\\|▣\\|↺\\|✂\\|❐\\|❏\\|⌕"
                         nemacs-next-session-smoke-toolbar-unicode)
         (= 1 (string-width (emacs-toolbar-icon-glyph "new")))
         (string-match-p ">{\\[N\\] New File}<" nemacs-next-session-smoke-toolbar-ascii)
         (string-match-p "\\[O\\]\\|\\[D\\]\\|\\[X\\]\\|\\[S\\]\\|\\[U\\]"
                         nemacs-next-session-smoke-toolbar-ascii)
         (eq (plist-get nemacs-next-session-smoke-mx-enter :type)
             (quote delta))
         (plist-get nemacs-next-session-smoke-mx-state-0 :active)
         (eq (plist-get nemacs-next-session-smoke-mx-state-0 :purpose)
             (quote exec))
         (equal (plist-get nemacs-next-session-smoke-mx-state-0 :prompt)
                "M-x ")
         (equal (plist-get nemacs-next-session-smoke-mx-state-0 :contents)
                "")
         (member "save-buffer"
                 (plist-get nemacs-next-session-smoke-mx-state-0 :candidates))
         (equal (plist-get nemacs-next-session-smoke-mx-state-1 :contents)
                "sav")
         (member "save-buffer"
                 (plist-get nemacs-next-session-smoke-mx-state-1 :candidates))
         (equal (plist-get nemacs-next-session-smoke-mx-state-2 :contents)
                "save-buffer")
         (eq (plist-get nemacs-next-session-smoke-mx-commit :type)
             (quote snapshot))
         (equal (plist-get nemacs-next-session-smoke-mx-commit :saved-file)
                "$mx_file")
         (equal (rdf "$mx_file") "hello")
         (not (plist-get nemacs-next-session-smoke-mx-state-3 :active))
         (not (plist-get nemacs-next-session-smoke-mx-state-abort :active))
         (eq (plist-get nemacs-next-session-smoke-mx-unknown :type)
             (quote error))
         (eq (plist-get nemacs-next-session-smoke-mx-unknown :code)
             (quote unknown-command))
         (eq (plist-get nemacs-next-session-smoke-mx-ff-state :type)
             (quote minibuffer))
         (plist-get nemacs-next-session-smoke-mx-ff-state :active)
         (eq (plist-get nemacs-next-session-smoke-mx-ff-state :purpose)
             (quote file))
         (equal (plist-get nemacs-next-session-smoke-mx-ff-final :file-name)
                "$mx_ff_file")
         (not (featurep (quote emacs-init)))
         (not (featurep (quote nemacs-main)))
         (not (featurep (quote nemacs-gtk-frontend)))
         (not (featurep (quote nemacs-gui-file-bridge-runtime))))
    (nl-write-file "$marker" "ok")
  (nl-write-file
   "$marker"
   (format "fail count=%s missing=%s fbound=%s facade=%s init=%s main=%s gtk=%s bridge=%s splash=%S splash-snapshot=%S goto=%S fwd=%S back=%S del=%S oor=%S yank-empty=%S nl=%S undo-nl=%S kill-region=%S yank-1=%S kill-line=%S yank-2=%S bad-kill=%S undo-empty=%S alive=%S find-file=%S file-append=%S save=%S buffer-complete=%S generic-complete=%S switch=%S kill-file=%S missing-kill=%S org-open=%S org-parse-ok=%S toolbar-unicode=%S toolbar-ascii=%S mx-enter=%S mx-state-0=%S mx-state-1=%S mx-state-2=%S mx-commit=%S mx-state-3=%S mx-state-abort=%S mx-unknown=%S mx-ff-state=%S mx-ff-final=%S mx-file=%S"
           nemacs-next-session-smoke-count
           nemacs-next-session-smoke-missing
           (fboundp (quote nemacs-next-session-plan))
           (featurep (quote nelisp-emacs))
           (featurep (quote emacs-init))
           (featurep (quote nemacs-main))
           (featurep (quote nemacs-gtk-frontend))
           (featurep (quote nemacs-gui-file-bridge-runtime))
           nemacs-next-session-smoke-splash-buffer
           nemacs-next-session-smoke-splash-snapshot
           nemacs-next-session-smoke-goto
           nemacs-next-session-smoke-fwd
           nemacs-next-session-smoke-back
           nemacs-next-session-smoke-del
           nemacs-next-session-smoke-oor
           nemacs-next-session-smoke-yank-empty
           nemacs-next-session-smoke-nl
           nemacs-next-session-smoke-undo-nl
           nemacs-next-session-smoke-kill-region
           nemacs-next-session-smoke-yank-1
           nemacs-next-session-smoke-kill-line
           nemacs-next-session-smoke-yank-2
           nemacs-next-session-smoke-bad-kill
           nemacs-next-session-smoke-undo-empty
           nemacs-next-session-smoke-alive
           nemacs-next-session-smoke-find-file
           nemacs-next-session-smoke-file-append
           nemacs-next-session-smoke-save
           nemacs-next-session-smoke-buffer-complete
           nemacs-next-session-smoke-generic-complete
           nemacs-next-session-smoke-switch
           nemacs-next-session-smoke-kill-file
           nemacs-next-session-smoke-missing-kill
           nemacs-next-session-smoke-frame
           nemacs-next-session-smoke-rendered-frame
           nemacs-next-session-smoke-menu
           nemacs-next-session-smoke-resize
           nemacs-next-session-smoke-input
           nemacs-next-session-smoke-ime
           nemacs-next-session-smoke-clipboard
           nemacs-next-session-smoke-org-open
           nemacs-next-session-smoke-org-parse-ok
           nemacs-next-session-smoke-toolbar-unicode
           nemacs-next-session-smoke-toolbar-ascii
           nemacs-next-session-smoke-mx-enter
           nemacs-next-session-smoke-mx-state-0
           nemacs-next-session-smoke-mx-state-1
           nemacs-next-session-smoke-mx-state-2
           nemacs-next-session-smoke-mx-commit
           nemacs-next-session-smoke-mx-state-3
           nemacs-next-session-smoke-mx-state-abort
           nemacs-next-session-smoke-mx-unknown
           nemacs-next-session-smoke-mx-ff-state
           nemacs-next-session-smoke-mx-ff-final
           (rdf "$mx_file"))))
,quit
EOF

set +e
timeout 120s "$NELISP_BIN" --repl --no-prompt --no-print < "$tmp" > "$out" 2>&1
rc=$?
set -e

sentinel=
if [ -r "$marker" ]; then
  sentinel=$(cat "$marker")
fi

if [ "$rc" -ne 0 ] || [ "$sentinel" != "ok" ]; then
  cat "$out" >&2
  echo "nemacs-next-session-smoke: fail rc=$rc sentinel=$sentinel" >&2
  exit 1
fi

echo "nemacs-next-session-smoke: persistent-repl ok"
