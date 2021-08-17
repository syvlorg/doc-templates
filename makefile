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
|yes yes | fd . $(mkfileDir)/profiles/damascus \
    -HIe org \
    -E straight \
    -x $(mkfileDir)/backup-tangle.sh
|yes yes | fd . $(mkfileDir)/profiles/mecca \
    -HIe org \
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

delete:
|for dir in "damascus mecca doom spacemacs nano" ; do \
	rm -rf "$(mkfileDir)/profiles/$${dir}/straight" \
			"$(mkfileDir)/profiles/$${dir}/auto-save-list" \
			"$(mkfileDir)/profiles/$${dir}/eln-cache" \
			"$(mkfileDir)/profiles/$${dir}/etc" \
			"$(mkfileDir)/profiles/$${dir}/transient" \
			"$(mkfileDir)/profiles/$${dir}/var" \
			"$(mkfileDir)/profiles/$${dir}/.org-id-locations" \
			"$(mkfileDir)/profiles/$${dir}/elpa" \
			"$(mkfileDir)/profiles/$${dir}/quelpa" ; \
done

update-test:
|emacs -nw --update

no-config-test:
|emacs -nw -Q

emacs: tangle test
remacs: delete test-update-and-kill test
super-push: tangle push
super-push-only: tangle push-only
