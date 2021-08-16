;;; $EMACSDIR/early-init.el -*- lexical-binding: t; -*-

(setq pre-user-emacs-directory (file-name-directory load-file-name))
(load (format "%s/lib/f.el/f.el" pre-user-emacs-directory))

(mapc #'(lambda (lib) (interactive)
            (load (f-join pre-user-emacs-directory "lib" (symbol-name lib) (symbol-name lib))))
    '(a.el dash.el s.el))

(defun meq/ued1 (&rest args) (apply #'f-join pre-user-emacs-directory args))
(defun meq/ued2 (&rest args) (apply #'f-join user-emacs-directory args))

(defun meq/cl (&rest args) (let* ((path (apply #'meq/ued2 args))) (when (f-exists? path) (load path))))
(defun meq/cle (&rest args) (apply #'meq/cl (-snoc args "early-init.el")))
(defun meq/cli (&rest args) (apply #'meq/cl (-snoc args "init.el")))

(defun meq/ps (&rest args)
    (setq user-emacs-directory (apply #'meq/ued1 "profiles" args))
    (apply #'meq/cle args) (apply #'meq/cli args))

(cond ((member "--damascus" command-line-args)
            (delete "--damascus" command-line-args)
            (meq/ps "damascus"))
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
