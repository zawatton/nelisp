#!/usr/bin/env bash
# Doc 76 後段 cross-arch 実機検証 — M1 MacBook Air (aarch64 Darwin) 用 smoke。
#
# 想定: nelisp repo を git clone / pull 済 + bin に bake-images.sh が
# 通る環境 (= make + cargo + Rust toolchain).  Linux 上で書いた
# Linux/Darwin 切替の正しさを実際の Darwin 上で確認するためのもの。
#
# 使い方:
#   cd /path/to/nelisp
#   bash tests/m1-darwin-cross-arch-smoke.sh
#
# 期待結果: 出力末尾が「[m1-smoke] all PASS」になれば検証成功。
# どこかで FAIL すれば、そこより手前の output で原因を確認。

set -e

# 色付け (ログ可読性のため)
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
NC='\033[0m'
pass()  { printf "${GREEN}[PASS]${NC} %s\n" "$*"; }
fail()  { printf "${RED}[FAIL]${NC} %s\n" "$*"; exit 1; }
info()  { printf "${YELLOW}[info]${NC} %s\n" "$*"; }

# ---------------------------------------------------------------------------
# 0. 環境確認
# ---------------------------------------------------------------------------

info "uname -sm = $(uname -sm)"
[[ "$(uname -s)" == "Darwin" ]] || fail "this script must run on macOS (uname says: $(uname -s))"
[[ "$(uname -m)" == "arm64" ]]  || info "uname -m = $(uname -m) (= not arm64; offsets target Darwin arm64+x86_64 共通だが確認しておく)"

# ---------------------------------------------------------------------------
# 1. Build
# ---------------------------------------------------------------------------

info "make bake-images"
make bake-images 2>&1 | tail -3

info "cargo build --release --bin nelisp"
cargo build --release --bin nelisp 2>&1 | tail -3

NELISP=./target/release/nelisp
[[ -x "$NELISP" ]] || fail "binary not built at $NELISP"

# ---------------------------------------------------------------------------
# 2. Platform detection + cross-arch defconst 値
# ---------------------------------------------------------------------------

info "step 2: platform detection + Darwin defconst values"
out=$($NELISP eval '(progn
  (require (quote nelisp-stdlib-os))
  (princ (format "PLATFORM=%S\n" nelisp-os--platform))
  (princ (format "O-CREAT=%d\n" nelisp-os-O-CREAT))
  (princ (format "O-EXCL=%d\n" nelisp-os-O-EXCL))
  (princ (format "O-TRUNC=%d\n" nelisp-os-O-TRUNC))
  (princ (format "O-APPEND=%d\n" nelisp-os-O-APPEND))
  (princ (format "O-NONBLOCK=%d\n" nelisp-os-O-NONBLOCK))
  (princ (format "AF-INET6=%d\n" nelisp-os-AF-INET6))
  (princ (format "SOL-SOCKET=%d\n" nelisp-os-SOL-SOCKET))
  (princ (format "SO-REUSEADDR=%d\n" nelisp-os-SO-REUSEADDR))
  (princ (format "SO-KEEPALIVE=%d\n" nelisp-os-SO-KEEPALIVE))
  (princ (format "SOCK-NONBLOCK=%d\n" nelisp-os-SOCK-NONBLOCK))
  (princ (format "SOCK-CLOEXEC=%d\n" nelisp-os-SOCK-CLOEXEC))
  (princ (format "SUN-PATH-MAX=%d\n" nelisp-os-SUN-PATH-MAX))
  (princ (format "SOCKADDR-UN-LEN=%d\n" nelisp-os--sockaddr-un-len))
  (princ (format "SOCKADDR-IN6-LEN=%d\n" nelisp-os--sockaddr-in6-len))
  (princ (format "MSGHDR-LEN=%d\n" nelisp-os--msghdr-len))
  (princ (format "CMSGHDR-LEN=%d\n" nelisp-os--cmsghdr-len))
  (princ (format "SIGSET-LEN=%d\n" nelisp-os--sigset-len))
  (princ (format "SIGCHLD=%d\n" nelisp-os-SIGCHLD))
  (princ (format "SIGUSR1=%d\n" nelisp-os-SIGUSR1))
  (princ (format "SIGUSR2=%d\n" nelisp-os-SIGUSR2)))')

echo "$out"

# Darwin canonical values check
expect_kv() {
  local key="$1"; local want="$2"
  local got; got=$(echo "$out" | grep -E "^${key}=" | head -1 | cut -d= -f2)
  if [[ "$got" == "$want" ]]; then pass "$key=$got"; else fail "$key expected $want, got $got"; fi
}
expect_kv PLATFORM darwin
expect_kv O-CREAT 512
expect_kv O-EXCL 2048
expect_kv O-TRUNC 1024
expect_kv O-APPEND 8
expect_kv O-NONBLOCK 4
expect_kv AF-INET6 30
expect_kv SOL-SOCKET 65535
expect_kv SO-REUSEADDR 4
expect_kv SO-KEEPALIVE 8
expect_kv SOCK-NONBLOCK 0
expect_kv SOCK-CLOEXEC 0
expect_kv SUN-PATH-MAX 104
expect_kv SOCKADDR-UN-LEN 106
expect_kv SOCKADDR-IN6-LEN 28
expect_kv MSGHDR-LEN 48
expect_kv CMSGHDR-LEN 12
expect_kv SIGSET-LEN 4
expect_kv SIGCHLD 20
expect_kv SIGUSR1 30
expect_kv SIGUSR2 31

# ---------------------------------------------------------------------------
# 3. fstat round-trip — /etc/hosts (= macOS canonical readable file)
# ---------------------------------------------------------------------------

info "step 3: fstat round-trip on /etc/hosts"
$NELISP eval '(progn
  (require (quote nelisp-stdlib-os))
  (let* ((fd (nelisp-os-open "/etc/hosts" nelisp-os-O-RDONLY 0))
         (st (nelisp-os-fstat fd)))
    (princ (format "fstat=%S\n" st))
    (let ((size  (nelisp-os-stat-size st))
          (mode  (nelisp-os-stat-mode st))
          (nlink (nth 8 st))
          (uid   (nth 9 st))
          (gid   (nth 10 st))
          (mtime (nelisp-os-stat-mtime st)))
      (princ (format "SIZE=%d\n" size))
      (princ (format "MODE-OCTAL=%o\n" mode))
      (princ (format "MODE-IS-REGULAR=%s\n" (if (= (logand mode #o170000) #o100000) "YES" "NO")))
      (princ (format "NLINK=%d\n" nlink))
      (princ (format "UID=%d\n" uid))
      (princ (format "GID=%d\n" gid))
      (princ (format "MTIME-PLAUSIBLE=%s\n" (if (and (> mtime 1700000000) (< mtime 2000000000)) "YES" "NO")))
      (when (= size 0) (error "size 0 — file probably has wrong offset on Darwin"))
      (when (not (= (logand mode #o170000) #o100000))
        (error "mode non-regular — IFMT bits wrong, likely Darwin u16 vs i32 width mismatch"))
      (when (= nlink 0) (error "nlink 0 — Darwin u16 width might be wrong"))
      (when (zerop mtime) (error "mtime 0 — atime/mtime offset wrong"))
      (princ "[step3] PASS — fstat all field plausible\n"))
    (nelisp-os-close fd)))'

# ---------------------------------------------------------------------------
# 4. AF_INET TCP socket — bind to 127.0.0.1:0 + getsockname
# ---------------------------------------------------------------------------

info "step 4: AF_INET TCP bind+listen+getsockname"
$NELISP eval '(progn
  (require (quote nelisp-stdlib-os))
  (let ((s (nelisp-os-socket nelisp-os-AF-INET nelisp-os-SOCK-STREAM 0)))
    (nelisp-os-bind-inet s nelisp-os-INADDR-LOOPBACK 0)
    (nelisp-os-listen s 1)
    (let ((nm (nelisp-os-getsockname-inet s)))
      (princ (format "INET-BOUND=%S\n" nm))
      (let ((host (car nm)) (port (cdr nm)))
        (when (not (= host nelisp-os-INADDR-LOOPBACK))
          (error "getsockname returned wrong host: %d (expected %d)" host nelisp-os-INADDR-LOOPBACK))
        (when (not (and (> port 1023) (< port 65535)))
          (error "getsockname returned invalid port: %d" port))
        (princ "[step4] PASS\n")))
    (nelisp-os-close s)))'

# ---------------------------------------------------------------------------
# 5. AF_UNIX filesystem socket — bind + getsockname
# ---------------------------------------------------------------------------

info "step 5: AF_UNIX bind+getsockname (= verifies SUN_PATH_MAX 104 + sockaddr_un layout)"
TMPSOCK=$(mktemp -t m1smoke-XXXXX)
rm -f "$TMPSOCK"
$NELISP eval "(progn
  (require (quote nelisp-stdlib-os))
  (let ((s (nelisp-os-socket nelisp-os-AF-UNIX nelisp-os-SOCK-STREAM 0)))
    (nelisp-os-bind-unix s \"$TMPSOCK\")
    (let ((nm (nelisp-os-getsockname-unix s)))
      (princ (format \"UNIX-BOUND=%S\n\" nm))
      (when (not (string= nm \"$TMPSOCK\"))
        (error \"getsockname-unix returned wrong path: %S (expected %S)\" nm \"$TMPSOCK\"))
      (princ \"[step5] PASS\n\"))
    (nelisp-os-close s)))"
rm -f "$TMPSOCK"

# ---------------------------------------------------------------------------
# 6. AF_INET6 TCP socket — bind to ::1 + getsockname-inet6
# ---------------------------------------------------------------------------

info "step 6: AF_INET6 TCP bind+listen+getsockname-inet6"
$NELISP eval '(progn
  (require (quote nelisp-stdlib-os))
  (let ((s (nelisp-os-socket nelisp-os-AF-INET6 nelisp-os-SOCK-STREAM 0)))
    (nelisp-os-bind-inet6 s nelisp-os-IN6ADDR-LOOPBACK 0)
    (nelisp-os-listen s 1)
    (let ((nm (nelisp-os-getsockname-inet6 s)))
      (princ (format "INET6-BOUND=%S\n" nm))
      (let ((host (car nm)) (port (cdr nm)))
        (when (not (equal host nelisp-os-IN6ADDR-LOOPBACK))
          (error "getsockname-inet6 returned wrong host: %S (expected %S)"
                 host nelisp-os-IN6ADDR-LOOPBACK))
        (when (not (and (> port 1023) (< port 65535)))
          (error "getsockname-inet6 returned invalid port: %d" port))
        (princ "[step6] PASS\n")))
    (nelisp-os-close s)))'

# ---------------------------------------------------------------------------
# 7. --require-linux gate fires for Linux-only surfaces
# ---------------------------------------------------------------------------

info "step 7: --require-linux gate fires for Linux-only wrappers"
$NELISP eval '(progn
  (require (quote nelisp-stdlib-os))
  (dolist (call (list (cons (quote nelisp-os-inotify-init) (list 0))
                      (cons (quote nelisp-os-eventfd)      (list 0 0))
                      (cons (quote nelisp-os-pidfd-open)   (list 0 0))
                      (cons (quote nelisp-os-timerfd-create) (list 0 0))))
    (let ((fn (car call)) (args (cdr call)))
      (condition-case err
          (progn (apply fn args)
                 (error "%S did not gate on darwin" fn))
        (nelisp-os-error
         (princ (format "GATED %S err=%S\n" fn (cadr err)))))))
  (princ "[step7] PASS\n"))'

# ---------------------------------------------------------------------------
# 8. cargo / make test (= bench)
# ---------------------------------------------------------------------------

info "step 8: cargo serial test (= existing test suite still passes on Darwin)"
cargo test --release -p nelisp-build-tool --lib -- --test-threads=1 2>&1 | tail -3

# ---------------------------------------------------------------------------
# Done
# ---------------------------------------------------------------------------

printf "${GREEN}[m1-smoke] all PASS${NC}\n"
