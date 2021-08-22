(use-package! hydra
    :custom (hydra-hint-display-type 'lv)
    :bind (:map hydra-base-map ("~" . hydra--universal-argument))
    :use-package-postconfig (use-package-hydra)
    :upnsd-preconfig (janus)
    :upnsd-postconfig (use-package-deino) (deino :custom (deino-hint-display-type 'lv)))
