#!/usr/bin/env mdsh

> Like Chemacs, but worse! — John Oliver (… Yeah, no; I wish! Maybe some
> day!)

# হ্যালো! Hello!

This is the beginning of my version of a literate configuration for [GNU
Emacs](https://www.gnu.org/software/emacs/).

They were inspired primarily by the configurations of [Musa
Al-hassy](https://alhassy.github.io/emacs.d/index.html) and [Patrick
Thomson](https://github.com/patrickt/emacs).

# Wheee!

First of all, I don't know how to check if I'm running on my phone, so I
pass in a command-line argument:

``` commonlisp
(defvar meq/var/phone (member "-p" command-line-args))
(delete "-p" command-line-args)
```

------------------------------------------------------------------------

Then I'll remove the double dashes from scripts:

``` commonlisp
(when (string= (car (last command-line-args)) "--") (delete "--" command-line-args))
```

------------------------------------------------------------------------

These next few lines are unabashedly stolen from [Henrik
Lissner's](https://github.com/hlissner) [Doom Emacs'
`early-init.el`](https://github.com/hlissner/doom-emacs/blob/develop/early-init.el):

> Emacs 27.1 introduced early-init.el, which is run before init.el,
> before package and UI initialization happens, and before site files
> are loaded.

> A big contributor to startup times is garbage collection. We up the gc
> threshold to temporarily prevent it from running, then reset it later
> by enabling \`gcmh-mode'. Not resetting it will cause
> stuttering/freezes.

``` commonlisp
(setq gc-cons-threshold most-positive-fixnum)
```

And for the `file-name-handler-alist`:

``` commonlisp
(unless (or (daemonp) noninteractive)
  (let ((old-file-name-handler-alist file-name-handler-alist))
```

> \`file-name-handler-alist' is consulted on each \`require', \`load'
> and various path/io functions. You get a minor speed up by unsetting
> this. Some warning, however: this could cause problems on builds of
> Emacs where its site lisp files aren't byte-compiled and we're forced
> to load the \*.el.gz files (e.g. on Alpine).

``` commonlisp
(setq-default file-name-handler-alist nil)
```

> …but restore \`file-name-handler-alist' later, because it is needed
> for handling encrypted or compressed files, among other things.

``` commonlisp
(defun meq/reset-file-handler-alist-h ()
  (setq file-name-handler-alist
```

> Merge instead of overwrite because there may have bene changes to
> \`file-name-handler-alist' since startup we want to preserve.

``` commonlisp
        (delete-dups (append file-name-handler-alist
                             old-file-name-handler-alist))))
(add-hook 'emacs-startup-hook #'meq/reset-file-handler-alist-h 101)))
```

------------------------------------------------------------------------

I would like to always prefer newer byte-compiled files, therefore, I
use [this answer](https://emacs.stackexchange.com/a/186/31428), by
[Malabarba](https://emacs.stackexchange.com/users/50/malabarba):

``` commonlisp
(setq load-prefer-newer t)
```

------------------------------------------------------------------------

If I ever need it, this will give me the initial directory I was in; the
code is adapted from
[Stefan's](https://emacs.stackexchange.com/users/1979/stefan) [answer
here](https://emacs.stackexchange.com/a/31662/31428):

``` commonlisp
(setq meq/var/initial-directory default-directory)
```

------------------------------------------------------------------------

The next few bits are adapted from
[here](https://www.reddit.com/r/emacs/comments/dppmqj/do_i_even_need_to_leverage_earlyinitel_if_i_have/?utm_source=amp&utm_medium=&utm_content=post_body),
with a few quotes from myself and other scattered here and there, such
as this bit [about
`gc-cons-percentage`](https://www.reddit.com/r/emacs/comments/41m7x3/why_are_you_changing_gcconsthreshold/cz3t775?utm_source=share&utm_medium=web2x&context=3):

> … There's also gc-cons-percentage which performs a gc if the amount of
> new memory used as a percentage of the total has increased by a
> certain amount. If you set gc-cons-threshold to a large number that
> effectively puts gc-cons-percentage into the driving seat. The default
> gc-cons-threshold is 400000 bytes, not 800000. …

``` commonlisp
(defvar meq/var/gc-cons-percentage gc-cons-percentage)

(add-hook 'after-init-hook
          (lambda ()
            (setq gc-cons-percentage meq/var/gc-cons-percentage)

            (defun meq/gc-on-lose-focus ()
              (unless (frame-focus-state)
                (garbage-collect)))

            (if (boundp 'after-focus-change-function)
                (add-function :after after-focus-change-function #'meq/gc-on-lose-focus))))

(setq-default gc-cons-percentage 0.6
```

Dunno *quite* what this bit does…

``` commonlisp
auto-window-vscroll nil
frame-inhibit-implied-resize t
inhibit-compacting-font-caches t)
```

I don't like typing `yes` or `no` all the time, so we'll shorten the
answer statement a bit.

``` commonlisp
(fset 'yes-or-no-p 'y-or-n-p)
```

Dunno what *this* bit does either…

``` commonlisp
(fset 'view-hello-file 'ignore)
(fset 'display-startup-echo-area-message 'ignore)

(put 'narrow-to-region 'disabled nil)
(put 'up-case-rgion 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'erase-buffer 'disabled nil)

(push '(ns-transparent-titlebar . t) default-frame-alist)
(push '(ns-appearance . nil) default-frame-alist)
(push '(internal-border . 0) default-frame-alist)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars . 0) default-frame-alist)
(push '(left-fringe . 0) default-frame-alist)
(push '(right-fringe . 0) default-frame-alist)
```

------------------------------------------------------------------------

Now that that's over with, let's get the profile name; this is done by
searching through the `command-line-args` list for the `--profile`
argument. If found, get the profile name from the index of the
`--profile` argument plus 1, otherwise, set it to the default name of
`damascus`.

``` commonlisp
(defvar meq/var/profiled t)
(defvar meq/var/profile-name (if (member "--profile" command-line-args)
    (let* ((value (nth (1+ (seq-position command-line-args "--profile")) command-line-args)))
        (unwind-protect
            value
```

While we're at it, we'll delete the the appropriate command-line
arguments as well:

``` commonlisp
(delete "--profile" command-line-args)
(delete value command-line-args))) "damascus"))
```

------------------------------------------------------------------------

This next bit defines a function which will add arguments to the
`command-line-args` list, if and only if it doesn't already exist in the
list and the argument is an option, as when prefixed by `-` or `--`.

``` commonlisp
(require 'cl)
(defun meq/push-to-cla (args)
    (dolist (arg* args)
        (let* ((arg (if (stringp arg*) arg* (symbol-name arg*)))
                (already-in-list (member arg command-line-args)))
            (when (and
                    (or (string-prefix-p "-" arg) (string-prefix-p "--" arg))
                    (not already-in-list)) (add-to-list 'command-line-args arg t)))))
```

Next, for specific profiles, if applicable, we will add any arguments
necessary to be able to run the profile.

``` commonlisp
(cl-case (intern meq/var/profile-name)
    (nano (meq/push-to-cla '(--profile-lib profiles/nano/lisp/nano.el)))
    (graphene (meq/push-to-cla '(--profile-lib profiles/graphene/lisp/graphene.el))))
```

------------------------------------------------------------------------

Let's byte-compile the library directories and add them to the load-path
now; the following bits are adapted from [NickD's
answer](https://emacs.stackexchange.com/users/14825/nickd)
[here](https://emacs.stackexchange.com/a/55415/31428), and [from this
section of the Emacs
Wiki](https://www.emacswiki.org/emacs/LoadPath#h5o-2).

The first directory to compile and add is the directory of emacs
packages, as git submodules, of my project, managed by… well… you'll
see.

``` commonlisp
(let ((default-directory (concat pre-user-emacs-directory "lib")))
    (byte-recompile-directory default-directory nil)
    (normal-top-level-add-to-load-path '("."))
    (normal-top-level-add-subdirs-to-load-path))
```

The second directory consists of the packages I develop, as git
subtrees:

``` commonlisp
(let ((default-directory (concat pre-user-emacs-directory "siluam")))
    (byte-recompile-directory default-directory nil)
    (normal-top-level-add-to-load-path '("."))
    (normal-top-level-add-subdirs-to-load-path))
```

------------------------------------------------------------------------

<center>[![](borg.gif)](https://github.com/emacscollective/borg)</center>
<https://youtu.be/rQxluLOMcVE?t=33>

# Addendum

These are just a few blocks I use regularly in my `org` files, whether
in `noweb`, naming, or otherwise:

## username

``` text
shadowrylander
```

## hash-deprecated

``` commonlisp
(md5 (concat (replace-regexp-in-string "/" "" (
    org-format-outline-path (org-get-outline-path))) (
        nth 4 (org-heading-components)) name))
```

## hash

``` commonlisp
(format-time-string "%Y%m%d%H%M%S%N")
```
