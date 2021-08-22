(meq/up sorrow
    :primer+ ("t" "toggles")
    :config ;; From: https://github.com/shadowrylander/sorrow#which-key-integration
        (push '((nil . "sorrow:.*:") . (nil . "")) which-key-replacement-alist))
