;; [[file:../../meta.aiern.org::20210811234927547343000][20210811234927547343000]]
;;; $EMACSDIR/init.el -*- lexical-binding: t; -*-
(when (version< emacs-version "27") (load (meq/ued "early-init.el")))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(meq/var/current-theme 'ghostfreak-green-dark)
 '(meq/var/current-theme-mode "dark")
 '(safe-local-variable-values '((eval message "eval 1"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(defun meq/reload-init nil (interactive)
    (org-babel-load-file (meq/ued "README.org")))
(meq/reload-init)
;; 20210811234927547343000 ends here
