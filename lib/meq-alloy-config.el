(meq/up alloy
    :use-package-postconfig (lode) (prime)
        (uru :config (prime "u u" uru "uru")
                    (prime "u m" minoru "minoru"))
    :use-package-preconfig (command-log-mode)
        ;; Important: https://github.com/noctuid/general.el/issues/53#issuecomment-307262154
        (use-package-chords)
    :config
        (alloy-auto-unbind-keys)
        (alloy-def :keymaps demon-run
            ;; Adapted From:
            ;; Answer: https://stackoverflow.com/a/4557027/10827766
            ;; User: https://stackoverflow.com/users/387076/gilles-so-stop-being-evil
            "\eOA" [up]
            "\e[A" [up]
            "\eOB" [down]
            "\e[B" [down]
            "\eOD" [left]
            "\e[D" [left]
            "\eOC" [right]
            "\e[C" [right]
            "M-x" 'meq/M-x)
    :deino (deino-restart (:color blue) "r"
            ("`" nil "cancel")
            ("l" meq/reload-emacs "reload")
            ("s" restart-emacs "restart"))
    :custom (alloy-implicit-naked t))
