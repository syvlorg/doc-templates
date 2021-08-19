;; [[file:README.org::20210818012945822500800][20210818012945822500800]]
(cond ((or
            (equal system-type 'windows-nt)
            (equal system-type 'ms-dos))
        (require 'graphene-windows-defaults))
    ((equal system-type 'darwin) (require 'graphene-osx-defaults))
    (t (require 'graphene-linux-defaults)))
(load-theme 'dracula-purple-dark)
;; 20210818012945822500800 ends here
