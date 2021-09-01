;; [[file:../../meta.aiern.org::20210818012945822500800][20210818012945822500800]]
(meq/up project-persist)
(meq/up ido-completing-read+)
(meq/up ppd-sr-speedbar)
(meq/up smartparens)
(meq/up company)
(meq/up web-mode)
(meq/up smex)
(meq/up flycheck)

(with-eval-after-load 'graphene (cond ((or
            (equal system-type 'windows-nt)
            (equal system-type 'ms-dos))
        (require 'graphene-windows-defaults))
    ((equal system-type 'darwin) (require 'graphene-osx-defaults))
    (t (require 'graphene-linux-defaults))))
(load-theme 'dracula-purple-dark)
;; 20210818012945822500800 ends here
