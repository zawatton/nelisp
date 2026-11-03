;;; standalone-gnu-bytecode-vm-driver.el --- ELC VM cold-process setup -*- lexical-binding: t; -*-

(load (expand-file-name "src/nelisp-gnu-bytecode-vm.el"
                        (getenv "NELISP_REPO_ROOT")) nil nil t)

(provide 'standalone-gnu-bytecode-vm-driver)
;;; standalone-gnu-bytecode-vm-driver.el ends here
