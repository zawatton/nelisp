;;; regexp-opt-test.el --- Focused NeLisp regexp-opt tests  -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'regexp-opt)

(defconst regexp-opt-test--prompt-headings
  '("Enter" "enter" "Enter same" "enter same" "Enter the" "enter the"
    "Current" "Enter Auth" "enter auth" "Old" "old" "New" "new" "'s"
    "login" "Kerberos" "CVS" "UNIX" " SMB" "LDAP" "PEM" "SUDO"
    "[sudo]" "doas" "Repeat" "Bad" "Retype" "Verify"))

(defconst regexp-opt-test--password-words
  '("password" "passcode" "passphrase" "pass phrase" "pin"
    "decryption key" "encryption key" "암호" "パスワード" "ପ୍ରବେଶ ସଙ୍କେତ"
    "ពាក្យសម្ងាត់" "adgangskode" "contraseña" "contrasenya" "geslo"
    "hasło" "heslo" "iphasiwedi" "jelszó" "lösenord" "lozinka"
    "mật khẩu" "mot de passe" "parola" "pasahitza" "passord"
    "passwort" "pasvorto" "salasana" "senha" "slaptažodis"
    "wachtwoord" "كلمة السر" "ססמה" "лозинка" "пароль" "गुप्तशब्द"
    "शब्दकूट" "પાસવર્ડ" "సంకేతపదము" "ਪਾਸਵਰਡ" "ಗುಪ್ತಪದ" "கடவுச்சொல்"
    "അടയാളവാക്ക്" "গুপ্তশব্দ" "পাসওয়ার্ড" "රහස්පදය" "密码" "密碼"))

;; Capture GNU Emacs's implementation before explicitly loading the vendored
;; NeLisp-compatible copy below.
(defconst regexp-opt-test--host-reference
  (list (regexp-opt regexp-opt-test--prompt-headings t)
        (regexp-opt regexp-opt-test--password-words)
        (regexp-opt '("a" "ab" "ba" "bb"))
        (regexp-opt nil t)))

(defconst regexp-opt-test--repo-root
  (expand-file-name ".."
                    (file-name-directory
                     (or load-file-name buffer-file-name))))

(load (expand-file-name
       "vendor/emacs-lisp/emacs-lisp/regexp-opt.el"
       regexp-opt-test--repo-root)
      nil t)

(ert-deftest regexp-opt-test/comint-inputs-match-host-output ()
  "The pure workhorse retains GNU Emacs output for both comint inputs."
  (should
   (equal
    (list (regexp-opt regexp-opt-test--prompt-headings t)
          (regexp-opt regexp-opt-test--password-words)
          (regexp-opt '("a" "ab" "ba" "bb"))
          (regexp-opt nil t))
    regexp-opt-test--host-reference)))

(ert-deftest regexp-opt-test/comint-inputs-match-every-member ()
  "Generated expressions match every fixed string and no prefixed variant."
  (dolist (strings (list regexp-opt-test--prompt-headings
                         regexp-opt-test--password-words))
    (let ((regexp (concat "\\`" (regexp-opt strings t) "\\'")))
      (dolist (string strings)
        (should (string-match-p regexp string)))
      (should-not (string-match-p regexp
                                  (concat (car strings) "-extra"))))))

(ert-deftest regexp-opt-test/workhorse-has-no-completion-core-dependency ()
  "The recursive optimizer uses only its pure prefix and partition helpers."
  (let ((source (prin1-to-string (symbol-function 'regexp-opt-group))))
    (should-not (string-match-p "try-completion" source))
    (should-not (string-match-p "all-completions" source)))
  (should (equal (regexp-opt--common-prefix '("enter" "entry" "ent"))
                 "ent"))
  (should (equal (regexp-opt--split-first-char
                  '("alpha" "atom" "beta" "charlie"))
                 '(("alpha" "atom") "beta" "charlie"))))

(provide 'regexp-opt-test)

;;; regexp-opt-test.el ends here
