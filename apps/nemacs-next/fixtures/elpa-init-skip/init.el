(setq nemacs-next-elpa-skip-autoload-before-require
      (fboundp 'nemacs-next-elpa-dependent-value))
(setq nemacs-next-elpa-skip-package-activated
      (and (boundp 'package--activated) package--activated))
