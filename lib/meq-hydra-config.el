(meq/up hydra
    :custom (hydra-hint-display-type 'lv)
    :bind (:map hydra-base-map ("~" . hydra--universal-argument))
    :use-package-preconfig (use-package-hydra) (janus)
    :use-package-postconfig (use-package-deino) (deino :custom (deino-hint-display-type 'lv)))
