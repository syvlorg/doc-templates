.RECIPEPREFIX := |
.DEFAULT_GOAL := emacs

# Adapted From: https://www.systutorials.com/how-to-get-the-full-path-and-directory-of-a-makefile-itself/
mkfilePath := $(abspath $(lastword $(MAKEFILE_LIST)))
mkfileDir := $(dir $(mkfilePath))

subinit:
|git -C $(mkfileDir) submodule add --depth 1 -f https://github.com/shadowrylander/shadowrylander.github.io.git lib/shadowrylander.github.io
|git -C $(mkfileDir)/lib/shadowrylander.github.io checkout master
