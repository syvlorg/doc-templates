;; [[file:README.org::20210804184605617540800][20210804184605617540800]]
;;; $EMACSDIR/init.el -*- lexical-binding: t; -*-
(when (version< emacs-version "27") (load (meq/ued1 "early-init.el")))
(when meq/var/udei (meq/cl "lisp" (concat meq/var/profile-name ".el")))
(meq/cl "init.el")
;; 20210804184605617540800 ends here
