;; early-init.el

;; #+call: hash() :exports none

;; #+RESULTS:
;; : 20210604182053300746900

;; #+name: 20210604182053300746900

;; [[file:meta.aiern.org::20210604182053300746900][20210604182053300746900]]
;;; $EMACSDIR/early-init.el -*- lexical-binding: t; -*-

(defvar pre-user-emacs-directory (file-name-directory load-file-name))

;; To get the latest version of `org-mode', require the load-file before
;; byte-compilation of `lib'
(add-to-list 'load-path (concat pre-user-emacs-directory "lib/org/lisp"))
(require 'org-loaddefs)

(defun meq/reload-early-init nil (interactive)
    (org-babel-load-file (concat pre-user-emacs-directory "README.org")))
(meq/reload-early-init)
;; 20210604182053300746900 ends here

(defvar meq/var/phone (member "-p" command-line-args))
(delete "-p" command-line-args)

;; Emacs 27.1 introduced early-init.el, which is run before init.el, before
;; package and UI initialization happens, and before site files are loaded.

;; A big contributor to startup times is garbage collection. We up the gc
;; threshold to temporarily prevent it from running, then reset it later by
;; enabling `gcmh-mode'. Not resetting it will cause stuttering/freezes.
(setq gc-cons-threshold most-positive-fixnum)

;; In noninteractive sessions, prioritize non-byte-compiled source files to
   ;; prevent the use of stale byte-code. Otherwise, it saves us a little IO time
;; to skip the mtime checks on every *.elc file.
(setq load-prefer-newer noninteractive)

;; Adapted From:
;; Answer: https://emacs.stackexchange.com/a/31662/31428
;; User: https://emacs.stackexchange.com/users/1979/stefan
(setq initial-directory default-directory)

;; Adapted From: https://www.reddit.com/r/emacs/comments/dppmqj/do_i_even_need_to_leverage_earlyinitel_if_i_have/?utm_source=amp&utm_medium=&utm_content=post_body
(defvar default-file-name-handler-alist file-name-handler-alist)

(setq-default auto-window-vscroll nil
              file-name-handler-alist nil
              frame-inhibit-implied-resize t
              gc-cons-percentage 0.6
              inhibit-compacting-font-caches t
              package-enable-at-startup nil)

(add-hook 'after-init-hook
          (lambda ()
            (setq file-name-handler-alist default-file-name-handler-alist)
            (setq gc-cons-percentage 0.1)

            (defun meq/gc-on-lose-focus ()
              (unless (frame-focus-state)
                (garbage-collect)))

            (if (boundp 'after-focus-change-function)
                (add-function :after after-focus-change-function #'meq/gc-on-lose-focus))))

(fset 'yes-or-no-p 'y-or-n-p)
(fset 'view-hello-file 'ignore)
(fset 'display-startup-echo-area-message 'ignore)

(put 'narrow-to-region 'disabled nil)
(put 'up-case-rgion 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'erase-buffer 'disabled nil)

(push '(ns-transparent-titlebar . t) default-frame-alist)
(push '(ns-appearance . nil) default-frame-alist)
(push '(internal-border . 0) default-frame-alist)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars . 0) default-frame-alist)
(push '(left-fringe . 0) default-frame-alist)
(push '(right-fringe . 0) default-frame-alist)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Get rid of double dashes in scripts ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (string= (car (last command-line-args)) "--") (delete "--" command-line-args))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define preliminary variables ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar meq/var/profiled t)
(defvar meq/var/profile-name (if (member "--profile" command-line-args)
    (let* ((value (nth (1+ (seq-position command-line-args "--profile")) command-line-args)))
        (unwind-protect
            value
            (delete "--profile" command-line-args)
            (delete value command-line-args))) "damascus"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Add to the `command-line-args' ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'cl)
(defun meq/push-to-cla (args)
    (dolist (arg* args)
        (let* ((arg (if (stringp arg*) arg* (symbol-name arg*)))
                (already-in-list (member arg command-line-args)))
            (unless already-in-list (add-to-list 'command-line-args arg t)))))
(cl-case (intern meq/var/profile-name)
    (nano (meq/push-to-cla '(--profile-lib profiles/nano/lisp/nano.el)))
    (graphene (meq/push-to-cla '(--profile-lib profiles/graphene/lisp/graphene.el))))
(remove-duplicates command-line-args :test 'string=)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define other variables ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Adapted From: https://www.emacswiki.org/emacs/LoadPath#h5o-2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; And: ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Answer: https://emacs.stackexchange.com/a/55415/31428 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User: https://emacs.stackexchange.com/users/14825/nickd ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let ((default-directory (concat pre-user-emacs-directory "lib")))
    ;; (byte-recompile-directory default-directory nil)
    (normal-top-level-add-to-load-path '("."))
    (normal-top-level-add-subdirs-to-load-path))
(let ((default-directory (concat pre-user-emacs-directory "siluam")))
    ;; (byte-recompile-directory default-directory nil)
    (normal-top-level-add-to-load-path '("."))
    (normal-top-level-add-subdirs-to-load-path))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; We are `borg' ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq package-enable-at-startup nil)
(require 'borg)
(setq borg-rewrite-urls-alist '(("git@github.com:" . "https://github.com/")
                                ("git@gitlab.com:" . "https://gitlab.com/")))
(borg-initialize)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Tell straight.el about the profiles we are going to be using.
(setq straight-profiles
      '((nil . "default.el")
        ;; Packages which are pinned to a specific commit.
        (pinned . "pinned.el")))

(with-no-warnings
    (setq straight-vc-git-default-clone-depth 1)
    (setq straight-base-dir (meq/ued ".local/"))
    (setq straight-repository-branch "develop")
    (setq straight-build-dir (format "build-%s" emacs-version))
    (setq straight-cache-autoloads t)
    (setq straight-check-for-modifications '(check-on-save))
    (setq straight-repository-branch "develop")
    (setq straight-use-package-by-default t)
    ;; From: https://github.com/hartzell/straight.el/commit/882649137f73998d60741c7c8c993c7ebbe0f77a#diff-b335630551682c19a781afebcf4d07bf978fb1f8ac04c6bf87428ed5106870f5R1649
    (setq straight-disable-byte-compilation (member "--no-byte-compilation" command-line-args)))
(delete "--no-byte-compilation" command-line-args)

(eval-and-compile
  (setq straight-recipes-gnu-elpa-use-mirror t)
  (setq straight-recipes-emacsmirror-use-mirror t)
  (setq bootstrap-version 5)
  (setq bootstrap-file (concat straight-base-dir "straight/repos/straight.el/bootstrap.el")))

;; (unless (file-exists-p bootstrap-file)
;;   (with-current-buffer
;;       (url-retrieve-synchronously
;;        "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
;;        'silent 'inhibit-cookies)
;;     (goto-char (point-max))
;;     (eval-print-last-sexp)))

;; (load bootstrap-file nil 'nomessage)

;; (autoload #'straight-x-pull-all "straight-x")
;; (autoload #'straight-x-freeze-versions "straight-x")

(unless (member system-type '(windows-nt ms-dos))
    (meq/up exec-path-from-shell
        :custom
            (exec-path-from-shell-check-startup-files nil)
            (exec-path-from-shell-variables '("PATH" "MANPATH" "CACHE_HOME" "FPATH" "PYENV_ROOT"))
            (exec-path-from-shell-arguments '("-l"))
        :config
            (exec-path-from-shell-initialize)))

;; Adapted From: https://github.com/daviwil/dotfiles/blob/master/Emacs.org#native-compilation
(ignore-errors
    ;; Silence compiler warnings as they can be pretty disruptive
    (setq native-comp-async-report-warnings-errors nil)
    ;; Set the right directory to store the native comp cache
    (add-to-list 'native-comp-eln-load-path (meq/ued "eln-cache/")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set up `use-package' ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(with-no-warnings
  (setq use-package-verbose t)
  (setq use-package-enable-imenu-support t))
(require 'use-package)

;; use-package
(setq use-package-always-defer (member "--always-defer" command-line-args))
(delete "--always-defer" command-line-args)
(setq use-package-always-demand (or (member "--always-demand" command-line-args) (daemonp)))
(delete "--always-demand" command-line-args)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set up cleanup mechanisms ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package no-littering :demand t)
(use-package gcmh :demand t :config (gcmh-mode 1))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set up my `use-package-extras' ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package use-package-extras
    :demand t
    :init (require 'a) (require 'dash) (require 's) (require 'f)
    :config
        (meq/up meq :load-emacs-file-preconfig ("naked"))
        (meq/up leaf :use-package-preconfig
            (use-package-ensure-system-package)
            (leaf-keywords)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set up the `user-emacs-directory' ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq user-emacs-directory (f-full (meq/ued* "profiles" meq/var/profile-name)))
;; (unless (string= meq/var/profile-name "doom") (byte-recompile-directory user-emacs-directory nil))
(setq custom-file (meq/ued "init.el"))
;; Adapted From:
;; Answer: https://emacs.stackexchange.com/a/18682/31428
;; User: https://emacs.stackexchange.com/users/2731/ebpa
(setq auto-save-list-file-prefix user-emacs-directory)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implement `doom-emacs' updating ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (and
        (string= meq/var/profile-name "doom")
        (member "--update" command-line-args))
    (delete "--update" command-line-args)
    (call-process (meq/ued* "profiles" "doom" "bin" "doom") nil nil nil "update")
    (call-process (meq/ued* "profiles" "doom" "bin" "doom") nil nil nil "sync")
    (call-process (meq/ued* "profiles" "doom" "bin" "doom") nil nil nil "doctor"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set up theming ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (byte-recompile-directory (meq/ued* "themes") nil)
(add-to-list 'custom-theme-load-path (meq/ued* "themes"))
(setq custom-safe-themes t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Perform any last-minute steps for specific profiles ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let* ((spacemacs-path (meq/ued-profiles
                            "spacemacs"
                            "layers"
                            "+distributions"
                            "spacemacs-bootstrap")))
    (cl-case (intern meq/var/profile-name)
        (doom (load (meq/ued-lib "ido-completing-read+" "ido-completing-read+.el")))
        (spacemacs (progn
                        (load (concat spacemacs-path "packages.el"))
                        (load (concat spacemacs-path "funcs.el"))
                        (spacemacs-bootstrap/init-use-package)))
        (patrick (advice-add #'reload-config :override #'(lambda nil (interactive)
                    (org-babel-load-file (meq/ued-profiles "patrick" "readme.org")))))
        (alhassy (require 'quelpa-use-package))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load the profile's `early-init' if it exists ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun meq/load-from-cla (arg &optional byte-compile)
    (eval `(meq/when-item-in-cla ,arg
        (let* ((item (meq/get-next-in-cla ,arg))
                (file (expand-file-name item))
                (exists (f-exists? file))
                (is-dir (and exists (f-directory? file)))
                (dir (if is-dir file (f-dirname file))))
            (message file)
            (if (not exists)
                (eval (intern item))
                ;; (when ,byte-compile (byte-recompile-directory dir nil))
                (add-to-list 'load-path dir)
                (unless is-dir (load file)))))))
(if (member "--profile-early-init" command-line-args)
    (meq/load-from-cla "--profile-early-init")
    (meq/cl "early-init.el"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Use an alternate `early-lib' ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(meq/load-from-cla "--profile-early-lib" t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
