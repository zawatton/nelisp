(comp--compile-ctxt-to-file0
 (condition-case e (comp--compile-ctxt-to-file0 nil) (error e))
 (condition-case e (comp--compile-ctxt-to-file0 "x") (error e)))
(comp-el-to-eln-filename
 (condition-case e (comp-el-to-eln-filename nil) (error e))
 (condition-case e (comp-el-to-eln-filename 4 "/tmp") (error e)))
(comp--init-ctxt
 (comp--init-ctxt)
 (progn (comp--release-ctxt) (comp--init-ctxt)))
(comp--install-trampoline
 (condition-case e (comp--install-trampoline nil nil) (error e))
 (comp--install-trampoline (symbol-function 'car) (symbol-function 'cdr)))
(comp--late-register-subr
 (condition-case e (comp--late-register-subr) (error e))
 (condition-case e (comp--late-register-subr) (error e)))
(comp-libgccjit-version
 (comp-libgccjit-version)
 (comp-libgccjit-version))
(comp-native-compiler-options-effective-p
 (comp-native-compiler-options-effective-p)
 (let ((native-comp-compiler-options nil)) (comp-native-compiler-options-effective-p)))
(comp-native-driver-options-effective-p
 (comp-native-driver-options-effective-p)
 (let ((native-comp-driver-options nil)) (comp-native-driver-options-effective-p)))
(comp--register-lambda
 (condition-case e (comp--register-lambda) (error e))
 (condition-case e (comp--register-lambda) (error e)))
(comp--register-subr
 (condition-case e (comp--register-subr) (error e))
 (condition-case e (comp--register-subr) (error e)))
(comp--release-ctxt
 (comp--release-ctxt)
 (progn (comp--init-ctxt) (comp--release-ctxt)))
(comp--subr-signature
 (condition-case e (comp--subr-signature nil) (error e))
 (comp--subr-signature (symbol-function 'car)))
