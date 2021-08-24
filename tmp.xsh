for lib in [
    "alamode",
    "use-package-extras",
    "use-package-deino",
    "lode",
    "meq",
    "janus",
    "titan",
    "fell",
    "doc",
    "cosmog",
    "uru",
    "prime",
    "meta",
    "riot",
]:
    git subtree add --squash --prefix siluam/@(lib) @(lib) main

for lib in [
    "aiern",
    "doom-aiern-modeline",
    "alloy",
    "deino",
    "sorrow",
    "aiern-god-state",
    "cosmoem",
    "evil-evilified-state",
    "helm-ido-like",
]:
    git subtree add --squash --prefix siluam/@(lib) @(lib) master

