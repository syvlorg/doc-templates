.RECIPEPREFIX := |
.DEFAULT_GOAL := emacs

# Adapted From: https://www.systutorials.com/how-to-get-the-full-path-and-directory-of-a-makefile-itself/
mkfilePath := $(abspath $(lastword $(MAKEFILE_LIST)))
mkfileDir := $(dir $(mkfilePath))
test := emacs --bg-daemon=test
killTest := emacsclient -s test -e "(kill-emacs)"

init:
|-git -C $(mkfileDir) config include.path "$(mkfileDir)/.gitconfig"

subinit:
|-git -C $(mkfileDir) submodule update --init --depth 1 --recursive
|-git -C $(mkfileDir) submodule sync
|-git -C $(mkfileDir) submodule add --depth 1 -f https://code.orgmode.org/bzg/org-mode lib/org
|-git -C $(mkfileDir) submodule add --depth 1 -f https://github.com/shadowrylander/shadowrylander.github.io.git lib/shadowrylander.github.io
|-git -C $(mkfileDir) submodule add --depth 1 -f https://github.com/emacscollective/borg.git lib/borg
|-git -C $(mkfileDir) submodule add --depth 1 -f https://github.com/emacscollective/closql.git lib/closql
|-git -C $(mkfileDir) submodule add --depth 1 -f https://github.com/emacscollective/epkg.git lib/epkg
|-git -C $(mkfileDir) submodule add --depth 1 -f https://github.com/magnars/dash.el.git lib/dash
|-git -C $(mkfileDir) submodule add --depth 1 -f https://github.com/magnars/s.el.git lib/s
|-git -C $(mkfileDir) submodule add --depth 1 -f https://github.com/plexus/a.el.git lib/a
|-git -C $(mkfileDir) submodule add --depth 1 -f https://github.com/rejeep/f.el.git lib/f
|-git -C $(mkfileDir) submodule add --depth 1 -f https://github.com/skeeto/emacsql.git lib/emacsql
# |git -C $(mkfileDir) submodule foreach 'git -C $$toplevel config submodule.$$name.ignore all'
|-cd $(mkfileDir)/lib/org; make all; make autoloads

pull: init subinit
|git -C $(mkfileDir) pull

add: init
|git -C $(mkfileDir) add .

commit: init
|-git -C $(mkfileDir) commit --allow-empty-message -am ""

cammit: add commit

push: cammit
|-git -C $(mkfileDir) push

tangle-setup:
|cp $(mkfileDir)/org-tangle.sh $(mkfileDir)/backup-tangle.sh
|chmod +x $(mkfileDir)/org-tangle.sh $(mkfileDir)/backup-tangle.sh

tangle: tangle-setup
|yes yes | fd . $(mkfileDir) \
    -HId 1 -e org \
    -E testing.aiern.org \
    -E resting.aiern.org \
    -x $(mkfileDir)/backup-tangle.sh
|fd . $(mkfileDir) \
    -HId 1 -e sh \
    -x chmod +x

clean:
|fd . $(mkfileDir) -HIe elc -x rm

pre-test: subinit

test: pre-test
|emacs

nw-test: pre-test
|emacs -nw

test-doom: pre-test
|emacs --doom

test-graphene: pre-test
|emacs --graphene

test-nano: pre-test
|emacs --nano

pest: pre-test
|emacs -p

update-test: pre-test
|emacs --update

no-config-test:
|emacs -Q

test-and-kill-pre: pre-test
|-emacsclient -s test -e "(kill-emacs)"

test-and-kill: test-and-kill-pre
|$(test)
|$(killTest)

test-new-and-kill: test-and-kill-pre
|$(test) -Q
|$(killTest)

test-update-and-kill: test-and-kill-pre
|$(test) --update
|$(killTest)

test-update-doom-and-kill: test-and-kill-pre
|$(test) --udoom
|$(killTest)

test-update-graphene-and-kill: test-and-kill-pre
|$(test) --graphene --update
|$(killTest)

test-update-nano-and-kill: test-and-kill-pre
|$(test) --nano --update
|$(killTest)

delete-doom:
|rm -rf $(mkfileDir)/profiles/doom/.local

delete:
|rm -rf $(mkfileDir)/profiles/damascus/.local

delete-graphene:
|rm -rf $(mkfileDir)/profiles/graphene/.local

delete-nano:
|rm -rf $(mkfileDir)/profiles/nano/.local

emacs: tangle test
emacs-nw: tangle nw-test
remacs: delete tangle test-update-and-kill test
doom-remacs: delete-doom tangle test-update-doom-and-kill test-doom
graphene-remacs: delete-graphene tangle test-update-graphene-and-kill test-graphene
nano-remacs: delete-nano tangle test-update-nano-and-kill test-nano
super-push: tangle push
