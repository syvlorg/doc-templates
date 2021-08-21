#!/usr/bin/env sh
":"; exec emacs --quick --script "$0" -- "$@" # -*- mode: emacs-lisp; lexical-binding: t; -*-
(load (concat (getenv "HOME") "/.emacs.d/early-init.el"))
(meq/up markdown-mode :mode ("\\.md\\'")
    :use-package-postconfig (yasnippet)
    :upnsd-postconfig (titan :load-path "../../lib/titan"))
(meq/upnsd doc :load-path "../../lib/doc" :mode ("\\.doc\\.md\\'" . doc-md-mode))
(find-file (concat (meq/timestamp) ".doc.md"))
(meq/insert-snippet "markdown titan template")
(save-buffer)
