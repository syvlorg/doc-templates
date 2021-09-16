.RECIPEPREFIX := |
.DEFAULT_GOAL := emacs
test := emacs --bg-daemon=test
killTest := emacsclient -s test -e "(kill-emacs)"
profile := "damascus"

echo:
|echo $(profile)

pre-test: subinit

pest: pre-test
|emacs -p

no-config-test:
|emacs -Q

test-and-kill-pre: pre-test
|-emacsclient -s test -e "(kill-emacs)"

test-new-and-kill: test-and-kill-pre
|$(test) -Q
|$(killTest)

test-new-nw-and-kill: test-and-kill-pre
|$(test) -Q -nw
|$(killTest)

clean-all:
|fd . $(mkfileDir) -HIe elc -x rm

pre-clean:
|fd . $(mkfileDir) -HId 1 -e elc -x rm

clean: pre-clean
|fd . $(mkfileDir)/profiles/$(profile) -HIe elc -x rm

delete: clean
|rm -rf $(mkfileDir)/profiles/$(profile)/.local

test: pre-test
|emacs --profile $(profile)

nw-test: pre-test
|emacs -nw --profile $(profile)

test-and-kill: test-and-kill-pre
|$(test) --profile $(profile)
|$(killTest)

test-nw-and-kill: test-and-kill-pre
|$(test) -nw --profile $(profile)
|$(killTest)

emacs: tangle test
emacs-nw: tangle nw-test
remacs: delete emacs
remacs-nw: delete emacs-nw
