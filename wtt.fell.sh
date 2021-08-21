#!/usr/bin/env sh
":"; exec emacs --quick --script "$0" -- "$@" # -*- mode: emacs-lisp; lexical-binding: t; -*-
(load (concat (getenv "HOME") "/.emacs.d/early-init.el"))
(load (concat (getenv "HOME") "/.emacs.d/init.el"))
(meq/initialize-everything-else)
(find-file (concat (meq/timestamp) ".fell.md"))
(meq/insert-snippet "org titan template")
(save-buffer)
