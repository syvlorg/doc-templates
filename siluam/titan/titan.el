;;; titan.el --- a simple package                     -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Jeet Ray

;; Author: Jeet Ray <aiern@protonmail.com>
;; Keywords: lisp
;; Version: 0.0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Adapted From: https://github.com/AndreaCrotti/yasnippet-snippets/blob/master/yasnippet-snippets.el

;;; Code:

(require 'meq)
(require 'yasnippet)
(require 'f)

(defvar meq/var/mecons '((org . org)
    (markdown . md)
    (adoc . adoc)))
(defun meq/titan-ph-func nil)

(defun meq/ddm (name mecons)
    (let* ((snippets (meq/inconcat name "-snippets")))
        (eval `(setq-local
                ,snippets
                (expand-file-name
                    "snippets"
                    (file-name-directory
                        ;; Copied from ‘f-this-file’ from f.el.
                        (cond
                        (load-in-progress load-file-name)
                        ((and (boundp 'byte-compile-current-file) byte-compile-current-file)
                        byte-compile-current-file)
                        ;; Adapted From:
                        ;; Answer: https://stackoverflow.com/a/1344894/10827766
                        ;; User: https://stackoverflow.com/users/8355/cjm
                        (:else ,(symbol-file (meq/inconcat "meq/" name "-ph-func"))))))))
        (when (eval `(f-exists? ,snippets))
            (add-to-list 'yas-snippet-dirs (symbol-value snippets) t)
            (eval `(yas-load-directory ,snippets t)))))

(defun meq/mapc-ddm (name &optional mecons* &rest args) (mapc #'(lambda (mecons) (interactive)
    (let* ((mode (symbol-name (car mecons)))
            (ext (symbol-name (cdr mecons))))
        (eval `(defun ,(meq/inconcat "meq/dired-create-" name "-" mode) nil (interactive)
            (when (derived-mode-p 'dired-mode) (let* ((file (f-join

                            ;; Adapted From:
                            ;; Answer: https://stackoverflow.com/a/11046990/10827766
                            ;; User: https://stackoverflow.com/users/324105/phils
                            default-directory

                            (format "%s.%s.%s" (meq/timestamp) ,name ,ext))))
                (with-current-buffer (find-file-noselect file)
                    (meq/insert-snippet (concat (if (featurep 'riot) "org" ,mode) " titan template"))
                    (save-buffer)
                    (kill-buffer))
                (revert-buffer)))))
        (eval `(defun ,(meq/inconcat "meq/dired-create-and-open-" name "-" mode) nil (interactive)
            (when (derived-mode-p 'dired-mode) (let* ((file (f-join

                            ;; Adapted From:
                            ;; Answer: https://stackoverflow.com/a/11046990/10827766
                            ;; User: https://stackoverflow.com/users/324105/phils
                            default-directory

                            (format "%s.%s.%s" (meq/timestamp) ,name ,ext))))
                
                ;; Adapted From:
                ;; Answer: https://stackoverflow.com/a/17984479/10827766
                ;; User: https://stackoverflow.com/users/2321928/bnzmnzhnz
                (unless (display-graphic-p) (other-window -1))

                (find-file file)
                (meq/insert-snippet (concat (if (featurep 'riot) "org" ,mode) " titan template"))))))
        (eval `(define-derived-mode
            ,(intern (concat name "-" mode "-mode"))
            ,(intern (concat "titan-" mode "-mode"))
            (meq/ddm ,name ',mecons)
            ,@args)))) (or mecons* meq/var/mecons)))

(mapc #'(lambda (mecons) (interactive)
    (let* ((mode (symbol-name (car mecons)))
            (ext (symbol-name (cdr mecons))))
        (eval `(define-derived-mode
            ,(intern (concat "titan-" mode "-mode"))
            ,(intern (concat mode "-mode"))
            ;; ,(concat "titan-" mode)
            (meq/ddm "titan" ',mecons)
            )))) meq/var/mecons)

(provide 'titan)
;;; titan.el ends here
