.RECIPEPREFIX := |
.DEFAULT_GOAL := emacs

# Adapted From: https://www.systutorials.com/how-to-get-the-full-path-and-directory-of-a-makefile-itself/
mkfilePath := $(abspath $(lastword $(MAKEFILE_LIST)))
mkfileDir := $(dir $(mkfilePath))
test := emacs -nw --bg-daemon=test
killTest := emacsclient -s test -e "(kill-emacs)"


init:
|-sudo cp $(mkfileDir)/git-subtree $$(git --exec-path)/

pull: init
|git -C $(mkfileDir) pull
|git -C $(mkfileDir) subtree pull-all

add:
|git -C $(mkfileDir) add .

commit:
|-git -C $(mkfileDir) commit --allow-empty-message -am ""

cammit: add commit

push-only: add commit
|-git -C $(mkfileDir) push

push: push-only init
|git -C $(mkfileDir) subtree prune
|-git -C $(mkfileDir) subtree push-all

tangle-setup:
|cp $(mkfileDir)/org-tangle.sh $(mkfileDir)/backup-tangle.sh
|chmod +x $(mkfileDir)/org-tangle.sh $(mkfileDir)/backup-tangle.sh

tangle: tangle-setup
|yes yes | fd . $(mkfileDir) \
    -HIe org \
    -E yankpad.org \
    -E testing.aiern.org \
    -E resting.aiern.org \
	-E profiles \
    -E straight \
    -x $(mkfileDir)/backup-tangle.sh
|fd . $(mkfileDir) \
    -HIe sh \
    -E straight \
    -x chmod +x

subtree-prep: tangle push-only

test:
|emacs -nw

pest:
|emacs -nw -p

test-and-kill-pre:
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

test-disable-yankpad-and-kill: test-and-kill-pre
|$(test) --disable-yankpad
|$(killTest)

delete:
|rm -rf $(mkfileDir)/straight \
        $(mkfileDir)/auto-save-list \
        $(mkfileDir)/eln-cache \
        $(mkfileDir)/etc \
        $(mkfileDir)/transient \
        $(mkfileDir)/var \
        $(mkfileDir)/.org-id-locations \
        $(mkfileDir)/elpa \
        $(mkfileDir)/quelpa

update-test:
|emacs -nw --update

no-config-test:
|emacs -nw -Q

emacs: tangle test
remacs: delete no-config-test tangle test-disable-yankpad-and-kill test-update-and-kill test
super-push: tangle push
super-push-only: tangle push-only
