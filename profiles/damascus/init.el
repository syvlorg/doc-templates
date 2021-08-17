;; [[file:README.org::20210811234927547343000][20210811234927547343000]]
;;; $EMACSDIR/init.el -*- lexical-binding: t; -*-
(when (version< emacs-version "27") (load (meq/ued "early-init.el")))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(meq/var/current-theme 'dracula-purple-dark)
 '(meq/var/current-theme-mode "dark")
 '(safe-local-variable-values '((eval message "eval 1"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(load (meq/ued "late-init.el"))
;; 20210811234927547343000 ends here
