;; -*- no-byte-compile: t; -*-
;;; lang/scala/packages.el

(package! sbt-mode :pin "e29464a82bf706ef921f4e0052ce04fc74c34c84")
(package! scala-mode :pin "598cb680f321d9609295aa9b4679040cc703b602")

(when (featurep! +lsp)
  (package! lsp-metals :pin "9f82ebee48d32cd7bbc3e64b84d1ef5b0926195b"))
