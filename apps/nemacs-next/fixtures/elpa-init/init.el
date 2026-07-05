(setq nemacs-next-elpa-init-autoload-before-require
      (fboundp 'nemacs-next-elpa-dependent-value))
(setq nemacs-next-elpa-init-load-path-has-base
      (member (expand-file-name "nemacs-next-elpa-base-1.0" package-user-dir)
              load-path))
(setq nemacs-next-elpa-init-load-path-has-dependent
      (member (expand-file-name "nemacs-next-elpa-dependent-1.0" package-user-dir)
              load-path))
(setq nemacs-next-elpa-init-activation-order
      (and (boundp 'nemacs-next-elpa-activation-order)
           nemacs-next-elpa-activation-order))
(require 'nemacs-next-elpa-dependent)
(setq nemacs-next-elpa-init-required-value
      (nemacs-next-elpa-dependent-value))
