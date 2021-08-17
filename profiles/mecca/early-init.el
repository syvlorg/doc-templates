;;; $EMACSDIR/early-init.el -*- lexical-binding: t; -*-

(setq meq/var/phone (member "-p" command-line-args))
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

;; In Emacs 27+, package initialization occurs before `user-init-file' is
;; loaded, but after `early-init-file'. Doom handles package initialization, so
;; we must prevent Emacs from doing it early!
(setq package-enable-at-startup nil)

;; Adapted From:
;; Answer: https://emacs.stackexchange.com/a/31662/31428
;; User: https://emacs.stackexchange.com/users/1979/stefan
(setq initial-directory default-directory)

;; Adapted From: https://www.reddit.com/r/emacs/comments/dppmqj/do_i_even_need_to_leverage_earlyinitel_if_i_have/?utm_source=amp&utm_medium=&utm_content=post_body
(eval-and-compile
  (defun meq/emacs-path (path)
    (expand-file-name path user-emacs-directory)))

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

;; Tell straight.el about the profiles we are going to be using.
(setq straight-profiles
      '((nil . "default.el")
        ;; Packages which are pinned to a specific commit.
        (pinned . "pinned.el")))

(with-no-warnings
    (setq straight-cache-autoloads t)
    (setq straight-check-for-modifications '(check-on-save))
    (setq straight-repository-branch "develop")
    (setq straight-use-package-by-default t)
    ;; From: https://github.com/hartzell/straight.el/commit/882649137f73998d60741c7c8c993c7ebbe0f77a#diff-b335630551682c19a781afebcf4d07bf978fb1f8ac04c6bf87428ed5106870f5R1649
    (setq straight-disable-byte-compilation (member "--no-byte-compilation" command-line-args)))
(delete "--no-byte-compilation" command-line-args)
(unless straight-disable-byte-compilation
    (byte-recompile-directory (meq/ued1 "lib") nil t)
    (byte-recompile-directory (meq/ued1 "themes") nil t))

(with-no-warnings
  (setq use-package-verbose t)
  (setq use-package-enable-imenu-support t))

(eval-and-compile
  (defvar straight-recipes-gnu-elpa-use-mirror t)
  (defvar straight-recipes-emacsmirror-use-mirror t)
  (defvar bootstrap-version 5)
  (defvar bootstrap-file (meq/emacs-path "straight/repos/straight.el/bootstrap.el")))

(unless (file-exists-p bootstrap-file)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
       'silent 'inhibit-cookies)
    (goto-char (point-max))
    (eval-print-last-sexp)))

(load bootstrap-file nil 'nomessage)

(autoload #'straight-x-pull-all "straight-x")
(autoload #'straight-x-freeze-versions "straight-x")

(straight-use-package '(use-package :type git :host github :repo "jwiegley/use-package" :branch "master"))

(defmacro meq/up* (&rest args) (interactive) `(use-package ,@args :demand ,(cl-getf args :demand t)))

(meq/up* no-littering)
(meq/up* gcmh
    :straight (gcmh :type git :host gitlab :repo "koral/gcmh" :branch "master")
    :config (gcmh-mode 1))

;; use-package
(setq use-package-always-defer (member "--always-defer" command-line-args))
(delete "--always-defer" command-line-args)
(setq use-package-always-demand (or (member "--always-demand" command-line-args) (daemonp)))
(delete "--always-demand" command-line-args)
(meq/up* use-package-extras :straight nil :load-path "../../lib/use-package-extras")
(meq/up leaf :use-package-preconfig (use-package-ensure-system-package) (leaf-keywords))

(meq/upnsd meq
    :load-path "../../lib/meq"
    :load-emacs-file-preconfig ("naked"))

(unless (or
            (eq system-type 'windows-nt)
            (eq system-type 'ms-dos))
    (meq/up exec-path-from-shell
        :straight (exec-path-from-shell
            :type git
            :host github
            :repo "purcell/exec-path-from-shell"
            :branch "master")
        :custom
            (exec-path-from-shell-check-startup-files nil)
            (exec-path-from-shell-variables '("PATH" "MANPATH" "CACHE_HOME" "FPATH" "PYENV_ROOT"))
            (exec-path-from-shell-arguments '("-l"))
        :config
            (exec-path-from-shell-initialize)))

(add-to-list 'load-path (meq/ued1 "lib"))
(add-to-list 'custom-theme-load-path (meq/ued1 "themes"))
(setq custom-safe-themes t)

;; Adapted From: https://github.com/daviwil/dotfiles/blob/master/Emacs.org#native-compilation
(ignore-errors
    ;; Silence compiler warnings as they can be pretty disruptive
    (setq native-comp-async-report-warnings-errors nil)
    ;; Set the right directory to store the native comp cache
    (add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache/" user-emacs-directory)))
