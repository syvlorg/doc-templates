;;; $EMACSDIR/early-init.el -*- lexical-binding: t; -*-

(when (string= (car (last command-line-args)) "--") (delete "--" command-line-args))

(defvar meq/var/profiled t)
(defvar pre-user-emacs-directory (file-name-directory load-file-name))

(mapc #'(lambda (lib) (interactive)
            (load (format "%slib/%s/%s" pre-user-emacs-directory (symbol-name lib) (symbol-name lib))))
    '(a.el dash.el s.el f.el))

(defun meq/ued1 (&rest args) (f-full (apply #'f-join pre-user-emacs-directory args)))

(byte-recompile-directory (meq/ued1 "lib") nil t)
(byte-recompile-directory (meq/ued1 "themes") nil t)
(add-to-list 'load-path (meq/ued1 "lib"))
(add-to-list 'custom-theme-load-path (meq/ued1 "themes"))
(setq custom-safe-themes t)

(defvar meq/var/udei-profiles '(nano graphene))
(defvar meq/var/profile-name (if (member "--profile" command-line-args)
    (nth (1+ (seq-position command-line-args "--profile")) command-line-args) "damascus"))
(defvar meq/var/udei (or (member "--udei" command-line-args)
                            (member (intern meq/var/profile-name) meq/var/udei-profiles)))

(when (or (string= meq/var/profile-name "damascus") meq/var/udei)
    (byte-recompile-directory (meq/ued1 "profiles" "damascus") nil t))

(delete "--profile" command-line-args)
(delete meq/var/profile-name command-line-args)
(delete "--udei" command-line-args)

(setq user-emacs-directory (f-full (funcall #'meq/ued1 "profiles" meq/var/profile-name)))

(byte-recompile-directory user-emacs-directory nil t)

(defun meq/ued2 (&rest args) (f-full (apply #'f-join user-emacs-directory args)))

(setq custom-file (funcall #'meq/ued2 "init.el"))

;; Adapted From:
;; Answer: https://emacs.stackexchange.com/a/18682/31428
;; User: https://emacs.stackexchange.com/users/2731/ebpa
(setq auto-save-list-file-prefix user-emacs-directory)

(defun meq/cl (&rest args) (let* ((path (apply #'meq/ued2 args))) (when (f-exists? path) (load path))))

(defun meq/use-damascus-early-init nil (interactive) (load (meq/ued1 "profiles" "damascus" "early-init.el")))

(when (and
        (string= meq/var/profile-name "doom")
        (member "--update" command-line-args))
    (delete "--update" command-line-args)
    (call-process (meq/ued1 "profiles" "doom" "bin" "doom") nil nil nil "update")
    (call-process (meq/ued1 "profiles" "doom" "bin" "doom") nil nil nil "sync")
    (call-process (meq/ued1 "profiles" "doom" "bin" "doom") nil nil nil "doctor"))

(when meq/var/udei (meq/use-damascus-early-init) (add-to-list 'load-path (meq/ued2 "lisp")))
(meq/cl "early-init.el")
