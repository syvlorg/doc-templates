;;; $EMACSDIR/early-init.el -*- lexical-binding: t; -*-

(setq pre-user-emacs-directory (file-name-directory load-file-name))

(mapc #'(lambda (lib) (interactive)
            (load (format "%slib/%s/%s" pre-user-emacs-directory (symbol-name lib) (symbol-name lib))))
    '(a.el dash.el s.el f.el))

(defun meq/ued1 (&rest args) (apply #'f-join pre-user-emacs-directory args))
(defun meq/ued2 (&rest args) (apply #'f-join user-emacs-directory args))

(defun meq/cl (&rest args) (let* ((path (apply #'meq/ued2 args))) (when (f-exists? path) (load path))))
(defun meq/cle nil (meq/cl "early-init.el"))
(defun meq/cli nil (meq/cl "init.el"))

(defun meq/ps (&rest args)
	(setq user-emacs-directory (apply #'meq/ued1 "profiles" args))
	(setq custom-file (apply #'meq/ued1 "profiles" (-snoc args "init.el")))
	(meq/cle) (meq/cli))

(cond ((member "--damascus" command-line-args)
            (delete "--damascus" command-line-args)
            (meq/ps "damascus"))
                ((member "--mecca" command-line-args)
            (delete "--mecca" command-line-args)
            (meq/ps "mecca"))
                ((member "--spacemacs" command-line-args)
            (delete "--spacemacs" command-line-args)
            (meq/ps "spacemacs"))
                ((member "--nano" command-line-args)
            (delete "--nano" command-line-args)
            (meq/ps "nano"))
        ((or
                (member "--doom" command-line-args)
                (member "--udoom" command-line-args))
            (delete "--doom" command-line-args)
            (when (or
                    (member "--update" command-line-args)
                    (member "--udoom" command-line-args))
                (delete "--udoom" command-line-args)
                (delete "--update" command-line-args)
                (call-process (meq/ued1 "doom" "bin" "doom") nil nil nil "update")
                (call-process (meq/ued1 "doom" "bin" "doom") nil nil nil "sync")
                (call-process (meq/ued1 "doom" "bin" "doom") nil nil nil "doctor"))
            (meq/ps "doom"))
        (t (meq/ps "damascus")))
