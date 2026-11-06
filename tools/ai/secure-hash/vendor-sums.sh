#!/usr/bin/env bash
# Usage: SHB_BIN=BINARY vendor-sums.sh
# Exit 0 iff the binary's (secure-hash 'sha256) of each file listed in
# vendor/SHA256SUMS, read with insert-file-contents-literally, equals the
# listed digest.  Prints the mismatch count.
here=$(cd "$(dirname "$0")" && pwd) || exit 2
root=$(cd "$here/../../.." && pwd) || exit 2
cd "$root" || exit 2
probe=$(mktemp --suffix=.el) || exit 2
trap 'rm -f "$probe"' EXIT
cat > "$probe" <<'ELISP'
(let ((bad 0) (n 0))
  (with-temp-buffer
    (insert-file-contents "vendor/SHA256SUMS")
    (dolist (line (split-string (buffer-string) "\n" t))
      (when (string-match "\\`\\([0-9a-f]\\{64\\}\\)  \\(.*\\)\\'" line)
        (let ((want (match-string 1 line)) (file (match-string 2 line)))
          (setq n (1+ n))
          (unless (equal want (with-temp-buffer
                                (insert-file-contents-literally file)
                                (secure-hash 'sha256 (current-buffer))))
            (setq bad (1+ bad)))))))
  (princ (format "VENDOR-SUMS checked=%d mismatched=%d\n" n bad)))
ELISP
out=$( (ulimit -v 8388608; timeout 600 "$SHB_BIN" --load "$probe" 2>/dev/null) | grep -a '^VENDOR-SUMS')
echo "$out"
printf '%s\n' "$out" | grep -q 'checked=[1-9][0-9]* mismatched=0$'
