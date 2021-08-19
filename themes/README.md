---
author: Jeet Ray
---

#!/usr/bin/env mdsh

The tangle functions are adapted from [this answer on the emacs Stack
Exchange](https://emacs.stackexchange.com/a/29884/31428), which was
written by [Andrew
Swann](https://emacs.stackexchange.com/users/2710/andrew-swann).

# As John Oliver Would Say: "Like Chemacs\[2\], but worse."

# wtt.sh

``` commonlisp
#!/usr/bin/env sh
":"; exec emacs --quick --script "$0" -- "$@" # -*- mode: emacs-lisp; lexical-binding: t; -*-
(princ (format-time-string "%Y%m%d%H%M%S%N")) (terpri)
```

# vterm-start.sh

``` bash
tmux new -As0
```

# org-tangle.sh

Adapted from:
<https://github.com/hlissner/doom-emacs/blob/develop/bin/org-tangle>

``` commonlisp
":"; exec emacs --quick --script "$0" -- "$@" # -*- mode: emacs-lisp; lexical-binding: t; -*-

(setq org-confirm-babel-evaluate nil)

(require 'cl-lib)
(require 'ox)
(require 'ob-tangle)

(when (file-exists-p "~/.emacs.d/README.org")
    (org-babel-lob-ingest "~/.emacs.d/README.org"))
(when (file-exists-p "~/.emacs.d/strange.aiern.org")
    (org-babel-lob-ingest "~/.emacs.d/strange.aiern.org"))

(defun meq/get-header nil (interactive)
    (nth 4 (org-heading-components)))
(defun meq/tangle-path nil (interactive)
    (string-remove-prefix "/" (concat
        (org-format-outline-path (org-get-outline-path)) "/"
            (meq/get-header))))
(defun meq/get-theme-from-header nil (interactive)
    (string-remove-suffix "-theme.el" (meq/get-header)))

(defun usage ()
  (with-temp-buffer
    (insert (format "%s %s [OPTIONS] [TARGETS...]\n"
                    "[1mUsage:[0m"
                    (file-name-nondirectory load-file-name))
            "\n"
            "A command line interface for tangling org-mode files. TARGETS can be\n"
            "files or folders (which are searched for org files recursively).\n"
            "\n"
            "This is useful for literate configs that rely on command line\n"
            "workflows to build it.\n"
            "\n"
            "[1mExample:[0m\n"
            "  org-tangle some-file.org\n"
            "  org-tangle literate/config/\n"
            "  org-tangle -p -l sh scripts.org > do_something.sh\n"
            "  org-tangle -p -l python -t tagA -t tagB file.org | python\n"
            "\n"
            "[1mOptions:[0m\n"
            "  -a --all\t\tTangle all blocks by default\n"
            "  -l --lang LANG\tOnly tangle blocks written in LANG\n"
            "  -p --print\t\tPrint tangled output to stdout than to files\n"
            "  -t --tag TAG\n"
            "     --and TAG\n"
            "     --or TAG\n"
            "    Lets you tangle org blocks by tag. You may have more than one\n"
            "    of these options.\n")
    (princ (buffer-string))))

(defun *org-babel-tangle (orig-fn &rest args)
  "Don't write tangled blocks to files, print them to stdout."
  (cl-letf (((symbol-function 'write-region)
             (lambda (start end filename &optional append visit lockname mustbenew)
               (princ (buffer-string)))))
    (apply orig-fn args)))

(defun *org-babel-tangle-collect-blocks (&optional language tangle-file)
  "Like `org-babel-tangle-collect-blocks', but will ignore blocks that are in
trees with the :notangle: tag."
  (let ((counter 0) last-heading-pos blocks)
    (org-babel-map-src-blocks (buffer-file-name)
      (let ((current-heading-pos
             (org-with-wide-buffer
              (org-with-limited-levels (outline-previous-heading)))))
        (if (eq last-heading-pos current-heading-pos) (cl-incf counter)
          (setq counter 1)
          (setq last-heading-pos current-heading-pos)))
      (unless (org-in-commented-heading-p)
        (require 'org)
        (let* ((tags (org-get-tags-at))
               (info (org-babel-get-src-block-info 'light))
               (src-lang (nth 0 info))
               (src-tfile (cdr (assq :tangle (nth 2 info)))))
          (cond ((member "notangle" tags))

                ((and (or or-tags and-tags)
                      (or (not and-tags)
                          (let ((a (cl-intersection and-tags tags :test #'string=))
                                (b and-tags))
                            (not (or (cl-set-difference a b :test #'equal)
                                     (cl-set-difference b a :test #'equal)))))
                      (or (not or-tags)
                          (cl-intersection or-tags tags :test #'string=))
                      t))

                ((or (not (or all-blocks src-tfile))
                     (string= src-tfile "no")  ; tangle blocks by default
                     (and tangle-file (not (equal tangle-file src-tfile)))
                     (and language (not (string= language src-lang)))))

                ;; Add the spec for this block to blocks under its language.
                ((let ((by-lang (assoc src-lang blocks))
                       (block (org-babel-tangle-single-block counter)))
                   (if by-lang
                       (setcdr by-lang (cons block (cdr by-lang)))
                     (push (cons src-lang (list block)) blocks))))))))
    ;; Ensure blocks are in the correct order.
    (mapcar (lambda (b) (cons (car b) (nreverse (cdr b)))) blocks)))
(advice-add #'org-babel-tangle-collect-blocks :override #'*org-babel-tangle-collect-blocks)

(defvar all-blocks nil)
(defvar and-tags nil)
(defvar or-tags nil)
(let (lang srcs and-tags or-tags)
  (pop argv)
  (while argv
    (let ((arg (pop argv)))
      (pcase arg
        ((or "-h" "--help")
         (usage)
         (error ""))
        ((or "-a" "--all")
         (setq all-blocks t))
        ((or "-l" "--lang")
         (setq lang (pop argv)))
        ((or "-p" "--print")
         (advice-add #'org-babel-tangle :around #'*org-babel-tangle))
        ((or "-t" "--tag" "--and")
         (push (pop argv) and-tags))
        ("--or"
         (push (pop argv) or-tags))
        ((guard (string-match-p "^--lang=" arg))
         (setq lang (cadr (split-string arg "=" t t))))
        ((guard (file-directory-p arg))
         (setq srcs
               (append (directory-files-recursively arg "\\.org$")
                       srcs)))
        ((guard (file-exists-p arg))
         (push arg srcs))
        (_ (error "Unknown option or file: %s" arg)))))

  (dolist (file srcs)
    (message (format "\n\nNow tangling %s:\n" file))
    (let ((backup (make-temp-file (file-name-base file) nil ".backup.org")))
      (unwind-protect
          ;; Prevent slow hooks from interfering
          (let (org-mode-hook)
            ;; We do the ol' switcheroo because `org-babel-tangle' writes
            ;; changes to the current file, which would be imposing on the user.
            (copy-file file backup t)
            (with-current-buffer (find-file-noselect file)

              ;; Tangling doesn't expand #+INCLUDE directives, so we do it
              ;; ourselves, since includes are so useful for literate configs!
              ;; (org-export-expand-include-keyword)

              (org-babel-tangle nil nil lang)))
        (ignore-errors (copy-file backup file t))
        (ignore-errors (delete-file backup)))))
  (kill-emacs 0))
```

# org-tangle-functions.el

``` commonlisp
(when (file-exists-p "~/.emacs.d/README.org")
    (org-babel-lob-ingest "~/.emacs.d/README.org"))
(when (file-exists-p "~/.emacs.d/strange.aiern.org")
    (org-babel-lob-ingest "~/.emacs.d/strange.aiern.org"))

(defun meq/get-header nil (interactive)
    (nth 4 (org-heading-components)))
(defun meq/tangle-path nil (interactive)
    (string-remove-prefix "/" (concat
        (org-format-outline-path (org-get-outline-path)) "/"
            (meq/get-header))))
(defun meq/get-theme-from-header nil (interactive)
    (string-remove-suffix "-theme.el" (meq/get-header)))
```

# git-subtree

``` bash
#
# git-subtree.sh: split/join git repositories in subdirectories of this one
#
# Copyright (C) 2009 Avery Pennarun <apenwarr@gmail.com>
#
if [ $# -eq 0 ]; then
    set -- -h
fi
OPTS_SPEC="\
git subtree add   --prefix=<prefix> <repository> <refspec>
git subtree merge --prefix=<prefix> <commit>
git subtree pull  --prefix=<prefix> [<repository> [<refspec>...]]
git subtree pull-all
git subtree push-all
git subtree push  --prefix=<prefix> [<repository> [<refspec>...]]
git subtree list
git subtree split --prefix=<prefix> <commit...>
git subtree from-submodule --prefix=<prefix>
git subtree prune
git subtree diff  --prefix=<prefix> [<repository> [<refspec>...]]
--
h,help        show the help
q             quiet
d             show debug messages
P,prefix=     the name of the subdir to split out
m,message=    use the given message as the commit message for the merge commit
 options for 'split'
annotate=     add a prefix to commit message of new commits
b,branch=     create a new branch from the split subtree
ignore-joins  ignore prior --rejoin commits
onto=         try connecting new tree to an existing one
rejoin        merge the new branch back into HEAD
 options for 'push'
f,force       use force push
 options for 'add', 'merge', 'pull' and 'push'
squash        merge subtree changes as a single commit
"
eval "$(echo "$OPTS_SPEC" | git rev-parse --parseopt -- "$@" || echo exit $?)"

PATH=$PATH:$(git --exec-path)
. git-sh-setup

require_work_tree

quiet=
branch=
debug=
command=
onto=
rejoin=
ignore_joins=
annotate=
squash=
message=

debug()
{
        if [ -n "$debug" ]; then
                echo "$@" >&2
        fi
}

say()
{
        if [ -z "$quiet" ]; then
                echo "$@" >&2
        fi
}

assert()
{
        if "$@"; then
                :
        else
                die "assertion failed: " "$@"
        fi
}


#echo "Options: $*"

while [ $# -gt 0 ]; do
        opt="$1"
        shift
        case "$opt" in
                -q) quiet=1 ;;
                -d) debug=1 ;;
                --annotate) annotate="$1"; shift ;;
                --no-annotate) annotate= ;;
                -b) branch="$1"; shift ;;
                -P|--prefix) prefix="$1"; shift ;;
                -m) message="$1"; shift ;;
                -f|--force) force=1 ;;
                --no-prefix) prefix= ;;
                --onto) onto="$1"; shift ;;
                --no-onto) onto= ;;
                --rejoin) rejoin=1 ;;
                --no-rejoin) rejoin= ;;
                --ignore-joins) ignore_joins=1 ;;
                --no-ignore-joins) ignore_joins= ;;
                --squash) squash=1 ;;
                --no-squash) squash= ;;
                --) break ;;
                *) die "Unexpected option: $opt" ;;
        esac
done

# Remove trailing slash
prefix="${prefix%/}";

command="$1"
shift
case "$command" in
        add|merge|pull|pull-all|push-all|from-submodule|prune) default= ;;
        split|push|diff|list) default="--default HEAD" ;;
        *) die "Unknown command '$command'" ;;
esac

if [ -z "$prefix" -a "$command" != "pull-all" -a "$command" != "push-all" -a "$command" != "list" -a "$command" != "prune" ]; then
        die "You must provide the --prefix option."
fi

case "$command" in
        pull-all);;
        push-all);;
        list);;
        prune);;
        add) [ -e "$prefix" ] && 
                die "prefix '$prefix' already exists." ;;
        *)   [ -e "$prefix" ] || 
                die "'$prefix' does not exist; use 'git subtree add'" ;;
esac

dir="$(dirname "$prefix/.")"

if [ "$command" != "pull" -a "$command" != "add" -a "$command" != "push" -a "$command" != "pull-all" -a "$command" != "diff" ]; then
        revs=$(git rev-parse $default --revs-only "$@") || exit $?
        dirs="$(git rev-parse --no-revs --no-flags "$@")" || exit $?
        if [ -n "$dirs" ]; then
                die "Error: Use --prefix instead of bare filenames."
        fi
fi

debug "command: {$command}"
debug "quiet: {$quiet}"
debug "revs: {$revs}"
debug "dir: {$dir}"
debug "opts: {$*}"
debug

cache_setup()
{
        cachedir="$GIT_DIR/subtree-cache/$$"
        rm -rf "$cachedir" || die "Can't delete old cachedir: $cachedir"
        mkdir -p "$cachedir" || die "Can't create new cachedir: $cachedir"
        mkdir -p "$cachedir/notree" || die "Can't create new cachedir: $cachedir/notree"
        debug "Using cachedir: $cachedir" >&2
}

cache_get()
{
        for oldrev in $*; do
                if [ -r "$cachedir/$oldrev" ]; then
                        read newrev <"$cachedir/$oldrev"
                        echo $newrev
                fi
        done
}

cache_miss()
{
        for oldrev in $*; do
                if [ ! -r "$cachedir/$oldrev" ]; then
                        echo $oldrev
                fi
        done
}

check_parents()
{
        missed=$(cache_miss $*)
        for miss in $missed; do
                if [ ! -r "$cachedir/notree/$miss" ]; then
                        debug "  incorrect order: $miss"
                fi
        done
}

set_notree()
{
        echo "1" > "$cachedir/notree/$1"
}

cache_set()
{
        oldrev="$1"
        newrev="$2"
        if [ "$oldrev" != "latest_old" \
             -a "$oldrev" != "latest_new" \
             -a -e "$cachedir/$oldrev" ]; then
                die "cache for $oldrev already exists!"
        fi
        echo "$newrev" >"$cachedir/$oldrev"
}

rev_exists()
{
        if git rev-parse "$1" >/dev/null 2>&1; then
                return 0
        else
                return 1
        fi
}

rev_is_descendant_of_branch()
{
        newrev="$1"
        branch="$2"
        branch_hash=$(git rev-parse $branch)
        match=$(git rev-list -1 $branch_hash ^$newrev)

        if [ -z "$match" ]; then
                return 0
        else
                return 1
        fi
}

# if a commit doesn't have a parent, this might not work.  But we only want
# to remove the parent from the rev-list, and since it doesn't exist, it won't
# be there anyway, so do nothing in that case.
try_remove_previous()
{
        if rev_exists "$1^"; then
                echo "^$1^"
        fi
}

find_latest_squash()
{
        debug "Looking for latest squash ($dir)..."
        dir="$1"
        sq=
        main=
        sub=
        git log --grep="^git-subtree-dir: $dir/*\$" \
                --pretty=format:'START %H%n%s%n%n%b%nEND%n' HEAD |
        while read a b junk; do
                debug "$a $b $junk"
                debug "{{$sq/$main/$sub}}"
                case "$a" in
                        START) sq="$b" ;;
                        git-subtree-mainline:) main="$b" ;;
                        git-subtree-split:) sub="$b" ;;
                        END)
                                if [ -n "$sub" ]; then
                                        if [ -n "$main" ]; then
                                                # a rejoin commit?
                                                # Pretend its sub was a squash.
                                                sq="$sub"
                                        fi
                                        debug "Squash found: $sq $sub"
                                        echo "$sq" "$sub"
                                        break
                                fi
                                sq=
                                main=
                                sub=
                                ;;
                esac
        done
}

find_existing_splits()
{
        debug "Looking for prior splits..."
        dir="$1"
        revs="$2"
        main=
        sub=
        git log --grep="^git-subtree-dir: $dir/*\$" \
                --pretty=format:'START %H%n%s%n%n%b%nEND%n' $revs |
        while read a b junk; do
                case "$a" in
                        START) sq="$b" ;;
                        git-subtree-mainline:) main="$b" ;;
                        git-subtree-split:) sub="$b" ;;
                        END)
                                debug "  Main is: '$main'"
                                if [ -z "$main" -a -n "$sub" ]; then
                                        # squash commits refer to a subtree
                                        debug "  Squash: $sq from $sub"
                                        cache_set "$sq" "$sub"
                                fi
                                if [ -n "$main" -a -n "$sub" ]; then
                                        debug "  Prior: $main -> $sub"
                                        cache_set $main $sub
                                        cache_set $sub $sub
                                        try_remove_previous "$main"
                                        try_remove_previous "$sub"
                                fi
                                main=
                                sub=
                                ;;
                esac
        done
}

copy_commit()
{
        # We're going to set some environment vars here, so
        # do it in a subshell to get rid of them safely later
        debug copy_commit "{$1}" "{$2}" "{$3}"
        git log -1 --pretty=format:'%an%n%ae%n%ad%n%cn%n%ce%n%cd%n%s%n%n%b' "$1" |
        (
                read GIT_AUTHOR_NAME
                read GIT_AUTHOR_EMAIL
                read GIT_AUTHOR_DATE
                read GIT_COMMITTER_NAME
                read GIT_COMMITTER_EMAIL
                read GIT_COMMITTER_DATE
                export  GIT_AUTHOR_NAME \
                        GIT_AUTHOR_EMAIL \
                        GIT_AUTHOR_DATE \
                        GIT_COMMITTER_NAME \
                        GIT_COMMITTER_EMAIL \
                        GIT_COMMITTER_DATE
                (echo -n "$annotate"; cat ) |
                git commit-tree "$2" $3  # reads the rest of stdin
        ) || die "Can't copy commit $1"
}

add_msg()
{
        dir="$1"
        latest_old="$2"
        latest_new="$3"
        if [ -n "$message" ]; then
                commit_message="$message"
        else
                commit_message="Add '$dir/' from commit '$latest_new'"
        fi
        cat <<-EOF
                                $commit_message

                                git-subtree-dir: $dir
                                git-subtree-mainline: $latest_old
                                git-subtree-split: $latest_new
                EOF
}

add_squashed_msg()
{
        if [ -n "$message" ]; then
                echo "$message"
        else
                echo "Merge commit '$1' as '$2'"
        fi
}

rejoin_msg()
{
        dir="$1"
        latest_old="$2"
        latest_new="$3"
        if [ -n "$message" ]; then
                commit_message="$message"
        else
                commit_message="Split '$dir/' into commit '$latest_new'"
        fi
        cat <<-EOF
                $commit_message

                git-subtree-dir: $dir
                git-subtree-mainline: $latest_old
                git-subtree-split: $latest_new
                EOF
}

squash_msg()
{
        dir="$1"
        oldsub="$2"
        newsub="$3"
        newsub_short=$(git rev-parse --short "$newsub")

        if [ -n "$oldsub" ]; then
                oldsub_short=$(git rev-parse --short "$oldsub")
                echo "Squashed '$dir/' changes from $oldsub_short..$newsub_short"
                echo
                git log --pretty=tformat:'%h %s' "$oldsub..$newsub"
                git log --pretty=tformat:'REVERT: %h %s' "$newsub..$oldsub"
        else
                echo "Squashed '$dir/' content from commit $newsub_short"
        fi

        echo
        echo "git-subtree-dir: $dir"
        echo "git-subtree-split: $newsub"
}

toptree_for_commit()
{
        commit="$1"
        git log -1 --pretty=format:'%T' "$commit" -- || exit $?
}

subtree_for_commit()
{
        commit="$1"
        dir="$2"
        git ls-tree "$commit" -- "$dir" |
        while read mode type tree name; do
                assert [ "$name" = "$dir" ]
                assert [ "$type" = "tree" -o "$type" = "commit" ]
                [ "$type" = "commit" ] && continue  # ignore submodules
                echo $tree
                break
        done
}

tree_changed()
{
        tree=$1
        shift
        if [ $# -ne 1 ]; then
                return 0   # weird parents, consider it changed
        else
                ptree=$(toptree_for_commit $1)
                if [ "$ptree" != "$tree" ]; then
                        return 0   # changed
                else
                        return 1   # not changed
                fi
        fi
}

new_squash_commit()
{
        old="$1"
        oldsub="$2"
        newsub="$3"
        tree=$(toptree_for_commit $newsub) || exit $?
        if [ -n "$old" ]; then
                squash_msg "$dir" "$oldsub" "$newsub" | 
                        git commit-tree "$tree" -p "$old" || exit $?
        else
                squash_msg "$dir" "" "$newsub" |
                        git commit-tree "$tree" || exit $?
        fi
}

copy_or_skip()
{
        rev="$1"
        tree="$2"
        newparents="$3"
        assert [ -n "$tree" ]

        identical=
        nonidentical=
        p=
        gotparents=
        for parent in $newparents; do
                ptree=$(toptree_for_commit $parent) || exit $?
                [ -z "$ptree" ] && continue
                if [ "$ptree" = "$tree" ]; then
                        # an identical parent could be used in place of this rev.
                        identical="$parent"
                else
                        nonidentical="$parent"
                fi

                # sometimes both old parents map to the same newparent;
                # eliminate duplicates
                is_new=1
                for gp in $gotparents; do
                        if [ "$gp" = "$parent" ]; then
                                is_new=
                                break
                        fi
                done
                if [ -n "$is_new" ]; then
                        gotparents="$gotparents $parent"
                        p="$p -p $parent"
                fi
        done

        if [ -n "$identical" ]; then
                echo $identical
        else
                copy_commit $rev $tree "$p" || exit $?
        fi
}

ensure_clean()
{
        if ! git diff-index HEAD --exit-code --quiet 2>&1; then
                die "Working tree has modifications.  Cannot add."
        fi
        if ! git diff-index --cached HEAD --exit-code --quiet 2>&1; then
                die "Index has modifications.  Cannot add."
        fi
}

cmd_add()
{
        if [ -e "$dir" ]; then
                die "'$dir' already exists.  Cannot add."
        fi

        ensure_clean

        if [ $# -eq 1 ]; then
                "cmd_add_commit" "$@"
        elif [ $# -eq 2 ]; then
                "cmd_add_repository" "$@"
        else
            say "error: parameters were '$@'"
            die "Provide either a refspec or a repository and refspec."
        fi
}

cmd_add_repository()
{
        echo "git fetch" "$@"
        repository=$1
        refspec=$2
        git fetch "$@" || exit $?
        revs=FETCH_HEAD
        set -- $revs
        cmd_add_commit "$@"

        # now add it to our list of repos
        git config -f .gittrees --unset subtree.$dir.url
        git config -f .gittrees --add subtree.$dir.url $repository
        git config -f .gittrees --unset subtree.$dir.path
        git config -f .gittrees --add subtree.$dir.path $dir
        git config -f .gittrees --unset subtree.$dir.branch
        git config -f .gittrees --add subtree.$dir.branch $refspec
}

cmd_add_commit()
{
        revs=$(git rev-parse $default --revs-only "$@") || exit $?
        set -- $revs
        rev="$1"

        debug "Adding $dir as '$rev'..."
        git read-tree --prefix="$dir" $rev || exit $?
        git checkout -- "$dir" || exit $?
        tree=$(git write-tree) || exit $?

        headrev=$(git rev-parse HEAD) || exit $?
        if [ -n "$headrev" -a "$headrev" != "$rev" ]; then
                headp="-p $headrev"
        else
                headp=
        fi

        if [ -n "$squash" ]; then
                rev=$(new_squash_commit "" "" "$rev") || exit $?
                commit=$(add_squashed_msg "$rev" "$dir" |
                         git commit-tree $tree $headp -p "$rev") || exit $?
        else
                commit=$(add_msg "$dir" "$headrev" "$rev" |
                         git commit-tree $tree $headp -p "$rev") || exit $?
        fi
        git reset "$commit" || exit $?

        say "Added dir '$dir'"
}

cmd_split()
{
        debug "Splitting $dir..."
        cache_setup || exit $?

        if [ -n "$onto" ]; then
                debug "Reading history for --onto=$onto..."
                git rev-list $onto |
                while read rev; do
                        # the 'onto' history is already just the subdir, so
                        # any parent we find there can be used verbatim
                        debug "  cache: $rev"
                        cache_set $rev $rev
                done
        fi

        if [ -n "$ignore_joins" ]; then
                unrevs=
        else
                unrevs="$(find_existing_splits "$dir" "$revs")"
        fi

        # We can't restrict rev-list to only $dir here, because some of our
        # parents have the $dir contents the root, and those won't match.
        # (and rev-list --follow doesn't seem to solve this)
        grl='git rev-list --topo-order --reverse --parents $revs $unrevs'
        revmax=$(eval "$grl" | wc -l)
        revcount=0
        createcount=0
        eval "$grl" |
        while read rev parents; do
                revcount=$(($revcount + 1))
                say -n "$revcount/$revmax ($createcount)
"
                debug "Processing commit: $rev"
                exists=$(cache_get $rev)
                if [ -n "$exists" ]; then
                        debug "  prior: $exists"
                        continue
                fi
                createcount=$(($createcount + 1))
                debug "  parents: $parents"
                newparents=$(cache_get $parents)
                debug "  newparents: $newparents"

                tree=$(subtree_for_commit $rev "$dir")
                debug "  tree is: $tree"

                check_parents $parents

                # ugly.  is there no better way to tell if this is a subtree
                # vs. a mainline commit?  Does it matter?
                if [ -z $tree ]; then
                        set_notree $rev
                        if [ -n "$newparents" ]; then
                                cache_set $rev $rev
                        fi
                        continue
                fi

                newrev=$(copy_or_skip "$rev" "$tree" "$newparents") || exit $?
                debug "  newrev is: $newrev"
                cache_set $rev $newrev
                cache_set latest_new $newrev
                cache_set latest_old $rev
        done || exit $?
        latest_new=$(cache_get latest_new)
        if [ -z "$latest_new" ]; then
                die "No new revisions were found"
        fi

        if [ -n "$rejoin" ]; then
                debug "Merging split branch into HEAD..."
                latest_old=$(cache_get latest_old)
                git merge -s ours \
                        -m "$(rejoin_msg $dir $latest_old $latest_new)" \
                        $latest_new >&2 || exit $?
        fi
        if [ -n "$branch" ]; then
                if rev_exists "refs/heads/$branch"; then
                        if ! rev_is_descendant_of_branch $latest_new $branch; then
                                die "Branch '$branch' is not an ancestor of commit '$latest_new'."
                        fi
                        action='Updated'
                else
                        action='Created'
                fi
                git update-ref -m 'subtree split' "refs/heads/$branch" $latest_new || exit $?
                say "$action branch '$branch'"
        fi
        echo $latest_new
        exit 0
}

cmd_merge()
{
        revs=$(git rev-parse $default --revs-only "$@") || exit $?
        ensure_clean

        set -- $revs
        if [ $# -ne 1 ]; then
                die "You must provide exactly one revision.  Got: '$revs'"
        fi
        rev="$1"

        if [ -n "$squash" ]; then
                first_split="$(find_latest_squash "$dir")"
                if [ -z "$first_split" ]; then
                        die "Can't squash-merge: '$dir' was never added."
                fi
                set $first_split
                old=$1
                sub=$2
                if [ "$sub" = "$rev" ]; then
                        say "Subtree is already at commit $rev."
                        exit 0
                fi
                new=$(new_squash_commit "$old" "$sub" "$rev") || exit $?
                debug "New squash commit: $new"
                rev="$new"
        fi

        version=$(git version)
        if [ "$version" \< "git version 1.7" ]; then
                if [ -n "$message" ]; then
                        git merge -s subtree --message="$message" $rev
                else
                        git merge -s subtree $rev
                fi
        else
                if [ -n "$message" ]; then
                        git merge -Xsubtree="$prefix" --message="$message" $rev
                else
                        git merge -Xsubtree="$prefix" $rev
                fi
        fi
}

cmd_pull()
{
        if [ $# -gt 2 ]; then
                die "You should provide either <refspec> or <repository> <refspec>"
        fi
        if [ -e "$dir" ]; then
                ensure_clean
                if [ $# -eq 1 ]; then
                        repository=$(git config -f .gittrees subtree.$prefix.url)
                        refspec=$1
                elif [ $# -eq 2 ]; then
                        repository=$1
                        refspec=$2
                else
                        repository=$(git config -f .gittrees subtree.$prefix.url)
                        refspec=$(git config -f .gittrees subtree.$prefix.branch)
                fi
                git fetch $repository $refspec || exit $?
                echo "git fetch using: " $repository $refspec
                revs=FETCH_HEAD
                set -- $revs
                cmd_merge "$@"
        else
                die "'$dir' must already exist. Try 'git subtree add'."
        fi
}

cmd_diff()
{
        if [ -e "$dir" ]; then
                if [ $# -eq 1 ]; then
                        repository=$(git config -f .gittrees subtree.$prefix.url)
                        refspec=$1
                elif [ $# -eq 2 ]; then
                        repository=$1
                        refspec=$2
                else
                        repository=$(git config -f .gittrees subtree.$prefix.url)
                        refspec=$(git config -f .gittrees subtree.$prefix.branch)
                fi
                # this is ugly, but I don't know of a better way to do it. My git-fu is weak.
                # git diff-tree expects a treeish, but I have only a repository and branch name.
                # I don't know how to turn that into a treeish without creating a remote.
                # Please change this if you know a better way!
                tmp_remote=__diff-tmp
                git remote rm $tmp_remote > /dev/null 2>&1
                git remote add -t $refspec $tmp_remote $repository > /dev/null
                # we fetch as a separate step so we can pass -q (quiet), which isn't an option for "git remote"
                # could this instead be "git fetch -q $repository $refspec" and leave aside creating the remote?
                # Still need a treeish for the diff-tree command...
                git fetch -q $tmp_remote
                git diff-tree -p refs/remotes/$tmp_remote/$refspec
                git remote rm $tmp_remote > /dev/null 2>&1
        else
                die "Cannot resolve directory '$dir'. Please point to an existing subtree directory to diff. Try 'git subtree add' to add a subtree."
        fi
}
cmd_push()
{
        if [ $# -gt 2 ]; then
                die "You shold provide either <refspec> or <repository> <refspec>"
        fi
        if [ -e "$dir" ]; then
                if [ $# -eq 1 ]; then
                        repository=$(git config -f .gittrees subtree.$prefix.url)
                        refspec=$1
                elif [ $# -eq 2 ]; then
                        repository=$1
                        refspec=$2
                else
                        repository=$(git config -f .gittrees subtree.$prefix.url)
                        refspec=$(git config -f .gittrees subtree.$prefix.branch)
                fi

                push_opts=
                if [ "$force" == "1" ]; then
                  push_opts="$push_opts --force"
                fi

                echo "git push using: " $repository $refspec
                rev=$(git subtree split --prefix=$prefix)
                if [ -n "$rev" ]; then
                        git push $push_opts $repository $rev:refs/heads/$refspec
                else
                        die "Couldn't push, 'git subtree split' failed."
                fi
        else
            die "'$dir' must already exist. Try 'git subtree add'."
        fi
}

subtree_list()
{
        git config -f .gittrees -l | grep subtree | grep path | sed "s/.*=//g" |
        while read path; do
                repository=$(git config -f .gittrees subtree.$path.url)
                refspec=$(git config -f .gittrees subtree.$path.branch)
                echo "  $path           (merged from $repository branch $refspec) "
        done
}

cmd_list()
{
  subtree_list
}

cmd_from-submodule()
{
        ensure_clean

        local submodule_sha=$(git submodule status $prefix | cut -d ' ' -f 2)
        local submodule_orig_repo=$(git config --file .gitmodules submodule.$prefix.url)

        # Remove references to submodule.
        git config --remove-section submodule.$prefix
        git config --file .gitmodules --remove-section submodule.$prefix
        git add .gitmodules

        # Move submodule aside.
        local tmp_repo="$(mktemp -d /tmp/git-subtree.XXXXX)"
        rm -r $tmp_repo
        mv $prefix $tmp_repo
        git rm $prefix

        # Commit changes.
        git commit -m "Remove '$prefix/' submodule"

        # subtree add from submodule repo.
        # TODO: Could be determin HEAD to be a specific branch
        cmd_add_repository $tmp_repo HEAD

        # Update .gittrees with the original repo url
        git config --file .gittrees --unset subtree.$prefix.url
        git config --file .gittrees subtree.$prefix.url $submodule_orig_repo

        # Remove submodule repo.
        rm -rf $tmp_repo
}

cmd_prune()
{
        git config -f .gittrees -l | grep subtree | grep path | sed "s/.*=//g" |
        while read path; do
                if [ ! -e "$path" ]; then
                        echo "pruning $path"
                        git config -f .gittrees --remove-section subtree.$path
                fi
        done
}

cmd_pull-all()
{
        git config -f .gittrees -l | grep subtree | grep path | sed "s/.*=//g" |
        while read path; do
                git subtree pull -P $path $(git config -f .gittrees subtree.$path.url) $(git config -f .gittrees subtree.$path.branch) || exit $?
        done
}

cmd_push-all()
{
        git config -f .gittrees -l | grep subtree | grep path | sed "s/.*=//g" |
        while read path; do
                git subtree push -P $path $(git config -f .gittrees subtree.$path.url) $(git config -f .gittrees subtree.$path.branch) || exit $?
        done
}

"cmd_$command" "$@"
```

# .gitconfig

``` conf
[remote "aiern"]
    url = git@github.com:shadowrylander/aiern
    fetch = +refs/heads/*:refs/remotes/aiern/*
[remote "doom-aiern-modeline"]
    url = git@github.com:shadowrylander/doom-aiern-modeline
    fetch = +refs/heads/*:refs/remotes/doom-aiern-modeline/*
[remote "alloy"]
    url = git@github.com:shadowrylander/alloy.git
    fetch = +refs/heads/*:refs/remotes/alloy/*
[remote "alamode"]
    url = git@github.com:shadowrylander/alamode
    fetch = +refs/heads/*:refs/remotes/alamode/*
[remote "use-package-extras"]
    url = git@github.com:shadowrylander/use-package-extras
    fetch = +refs/heads/*:refs/remotes/use-package-extras/*
[remote "deino"]
    url = git@github.com:shadowrylander/deino
    fetch = +refs/heads/*:refs/remotes/deino/*
[remote "use-package-deino"]
    url = git@github.com:shadowrylander/use-package-deino
    fetch = +refs/heads/*:refs/remotes/use-package-deino/*
[remote "sorrow"]
    url = git@github.com:shadowrylander/sorrow
    fetch = +refs/heads/*:refs/remotes/sorrow/*
[remote "lode"]
    url = git@github.com:shadowrylander/lode
    fetch = +refs/heads/*:refs/remotes/lode/*
[remote "meq"]
    url = git@github.com:shadowrylander/meq.git
    fetch = +refs/heads/*:refs/remotes/meq/*
[remote "aiern-god-state"]
    url = git@github.com:shadowrylander/aiern-god-state
    fetch = +refs/heads/*:refs/remotes/aiern-god-state/*
[remote "janus"]
    url = git@github.com:shadowrylander/janus
    fetch = +refs/heads/*:refs/remotes/janus/*
[remote "titan"]
    url = git@github.com:shadowrylander/titan
    fetch = +refs/heads/*:refs/remotes/titan/*
[remote "fell"]
    url = git@github.com:shadowrylander/fell
    fetch = +refs/heads/*:refs/remotes/fell/*
[remote "doc"]
    url = git@github.com:shadowrylander/doc
    fetch = +refs/heads/*:refs/remotes/doc/*
[remote "cosmoem"]
    url = git@gitlab.com:shadowrylander/cosmoem
    fetch = +refs/heads/*:refs/remotes/cosmoem/*
[remote "cosmog"]
    url = git@github.com:shadowrylander/cosmog
    fetch = +refs/heads/*:refs/remotes/cosmog/*
[remote "prime"]
    url = git@github.com:shadowrylander/prime
    fetch = +refs/heads/*:refs/remotes/prime/*
[remote "uru"]
    url = git@github.com:shadowrylander/uru
    fetch = +refs/heads/*:refs/remotes/uru/*
[remote "meta"]
    url = git@github.com:shadowrylander/meta
    fetch = +refs/heads/*:refs/remotes/meta/*
[remote "riot"]
    url = git@github.com:shadowrylander/riot
    fetch = +refs/heads/*:refs/remotes/riot/*
[remote "damascus"]
    url = git@github.com:shadowrylander/damascus
    fetch = +refs/heads/*:refs/remotes/damascus/*
[submodule "profiles/doom"]
        url = https://github.com/hlissner/.emacs.d
        active = true
[submodule "profiles/spacemacs"]
        url = https://github.com/spacemacs/spacemacs
        active = true
[submodule "profiles/nano/lisp"]
        url = https://github.com/rougier/nano
        active = true
[submodule "profiles/graphene/lisp"]
    url = https://github.com/rdallasgray/graphene
    active = true
[submodule "profiles/centaur"]
    url = https://github.com/seagle0128/.emacs.d
    active = true
[submodule "profiles/prelude"]
    url = https://github.com/bbatsov/prelude
    active = true
[submodule "lib/dash.el"]
        url = https://github.com/magnars/dash.el
        active = true
[submodule "lib/s.el"]
        url = https://github.com/magnars/s.el
        active = true
[submodule "lib/a.el"]
        url = https://github.com/plexus/a.el
        active = true
[submodule "lib/f.el"]
        url = https://github.com/rejeep/f.el
        active = true
```

# makefile

``` makefile
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
|yes yes | fd . $(mkfileDir)/profiles/graphene \
    -HIe org \
    -E straight \
    -x $(mkfileDir)/backup-tangle.sh
|yes yes | fd . $(mkfileDir)/profiles/nano \
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

test-doom:
|emacs -nw --doom

test-graphene:
|emacs -nw --graphene

test-nano:
|emacs -nw --nano

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

update-test:
|emacs -nw --update

no-config-test:
|emacs -nw -Q

emacs: tangle test
remacs: delete tangle test-update-and-kill test
doom-remacs: delete-doom tangle test-update-doom-and-kill test-doom
graphene-remacs: delete-graphene tangle test-update-graphene-and-kill test-graphene
nano-remacs: delete-nano tangle test-update-nano-and-kill test-nano
super-push: tangle push
super-push-only: tangle push-only
```

# lib

## damascus.el

``` commonlisp
;;; damascus.el --- a simple package                     -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Jeet Ray

;; Author: Jeet Ray <aiern@protonmail.com>
;; Keywords: lisp
;; Version: 0.0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Put a description of the package here

;;; Code:

;; code goes here

(provide 'damascus)
;;; damascus.el ends here
```

# early-init.el

Adapted From:
<https://github.com/hlissner/doom-emacs/blob/develop/early-init.el>

# init.el

``` commonlisp
;;; $EMACSDIR/init.el -*- lexical-binding: t; -*-
(when (version< emacs-version "27") (load (meq/ued1 "early-init.el")))
(when meq/var/udei (meq/cl "lisp" (concat meq/var/profile-name ".el")))
(meq/cl "init.el")
```

This is used to hold themes for `emacs`.

# themes

``` commonlisp
  `(default ((,class (:background ,shade0 :foreground ,shade7))))
  `(font-lock-builtin-face ((,class (:foreground ,accent5))))
  `(font-lock-comment-face ((,class (:foreground ,shade3))))
  `(font-lock-negation-char-face ((,class (:foreground ,accent4))))
  `(font-lock-reference-face ((,class (:foreground ,accent4))))
  `(font-lock-constant-face ((,class (:foreground ,accent4))))
  `(font-lock-doc-face ((,class (:foreground ,shade3))))
  `(font-lock-function-name-face ((,class (:foreground ,accent6 :bold t))))
  `(font-lock-keyword-face ((,class (:bold ,class :foreground ,accent1))))
  `(font-lock-string-face ((,class (:foreground ,accent3))))
  `(font-lock-type-face ((,class (:foreground ,accent5 ))))
  `(font-lock-variable-name-face ((,class (:foreground ,accent6))))
  `(font-lock-warning-face ((,class (:foreground ,accent0 :background ,shade1))))
  `(region ((,class (:background ,shade7 :foreground ,shade0))))
  `(highlight ((,class (:foreground ,shade5 :background ,shade2))))
  `(hl-line ((,class (:background  ,shade1))))
  `(fringe ((,class (:background ,shade1 :foreground ,shade4))))
  `(cursor ((,class (:background ,shade2))))
  `(show-paren-match-face ((,class (:background ,accent0))))
  `(isearch ((,class (:bold t :foreground ,accent0 :background ,shade2))))
  `(mode-line ((,class (:box (:line-width 1 :color nil) :bold t :foreground ,shade4 :background ,shade0))))
  `(mode-line-inactive ((,class (:box (:line-width 1 :color nil :style pressed-button) :foreground ,accent7 :background ,shade1 :weight normal))))
  `(mode-line-buffer-id ((,class (:bold t :foreground ,accent6 :background nil))))
  `(mode-line-highlight ((,class (:foreground ,accent1 :box nil :weight bold))))
  `(mode-line-emphasis ((,class (:foreground ,shade7))))
  `(vertical-border ((,class (:foreground ,shade5))))
  `(minibuffer-prompt ((,class (:bold t :foreground ,accent1))))
  `(default-italic ((,class (:italic t))))
  `(link ((,class (:foreground ,accent4 :underline t))))
  `(org-code ((,class (:foreground ,shade6))))
  `(org-hide ((,class (:foreground ,shade4))))
  `(org-level-1 ((,class (:bold t :foreground ,accent4 :height 1.1))))
  `(org-level-2 ((,class (:bold nil :foreground ,accent5))))
  `(org-level-3 ((,class (:bold t :foreground ,accent6))))
  `(org-level-4 ((,class (:bold nil :foreground ,accent7))))
  `(org-date ((,class (:underline t :foreground ,accent2) )))
  `(org-footnote  ((,class (:underline t :foreground ,shade4))))
  `(org-link ((,class (:underline t :foreground ,accent5 ))))
  `(org-special-keyword ((,class (:foreground ,accent1))))
  `(org-block ((,class (:foreground ,shade5))))
  `(org-quote ((,class (:inherit org-block :slant italic))))
  `(org-verse ((,class (:inherit org-block :slant italic))))
  `(org-todo ((,class (:box (:line-width 1 :color ,shade1) :foreground ,accent1 :bold t))))
  `(org-done ((,class (:box (:line-width 1 :color ,shade1) :foreground ,shade5 :bold t))))
  `(org-warning ((,class (:underline t :foreground ,accent2))))
  `(org-agenda-structure ((,class (:weight bold :foreground ,shade5 :box (:color ,shade4) :background ,shade2))))
  `(org-agenda-date ((,class (:foreground ,accent6 :height 1.1 ))))
  `(org-agenda-date-weekend ((,class (:weight normal :foreground ,shade4))))
  `(org-agenda-date-today ((,class (:weight bold :foreground ,accent1 :height 1.4))))
  `(org-agenda-done ((,class (:foreground ,shade3))))
  `(org-scheduled ((,class (:foreground ,accent5))))
  `(org-scheduled-today ((,class (:foreground ,accent6 :weight bold :height 1.2))))
  `(org-ellipsis ((,class (:foreground ,accent5))))
  `(org-verbatim ((,class (:foreground ,shade4))))
  `(org-document-info-keyword ((,class (:foreground ,accent6))))
  `(font-latex-bold-face ((,class (:foreground ,accent5))))
  `(font-latex-italic-face ((,class (:foreground ,accent7 :italic t))))
  `(font-latex-string-face ((,class (:foreground ,accent3))))
  `(font-latex-match-reference-keywords ((,class (:foreground ,accent4))))
  `(font-latex-match-variable-keywords ((,class (:foreground ,accent6))))
  `(ido-only-match ((,class (:foreground ,accent0))))
  `(org-sexp-date ((,class (:foreground ,shade4))))
  `(ido-first-match ((,class (:foreground ,accent1 :bold t))))
  `(gnus-header-content ((,class (:foreground ,accent1))))
  `(gnus-header-from ((,class (:foreground ,accent6))))
  `(gnus-header-name ((,class (:foreground ,accent5))))
  `(gnus-header-subject ((,class (:foreground ,accent6 :bold t))))
  `(mu4e-view-url-number-face ((,class (:foreground ,accent5))))
  `(mu4e-cited-1-face ((,class (:foreground ,shade6))))
  `(mu4e-cited-7-face ((,class (:foreground ,shade5))))
  `(mu4e-header-marks-face ((,class (:foreground ,accent5))))
  `(ffap ((,class (:foreground ,shade4))))
  `(js2-private-function-call ((,class (:foreground ,accent4))))
  `(js2-jsdoc-html-tag-delimiter ((,class (:foreground ,accent3))))
  `(js2-jsdoc-html-tag-name ((,class (:foreground ,accent2))))
  `(js2-external-variable ((,class (:foreground ,accent5  ))))
  `(js2-function-param ((,class (:foreground ,accent4))))
  `(js2-jsdoc-value ((,class (:foreground ,accent3))))
  `(js2-private-member ((,class (:foreground ,shade5))))
  `(js3-warning-face ((,class (:underline ,accent1))))
  `(js3-error-face ((,class (:underline ,accent0))))
  `(js3-external-variable-face ((,class (:foreground ,accent6))))
  `(js3-function-param-face ((,class (:foreground ,accent7))))
  `(js3-jsdoc-tag-face ((,class (:foreground ,accent1))))
  `(js3-instance-member-face ((,class (:foreground ,accent4))))
  `(warning ((,class (:foreground ,accent0))))
  `(ac-completion-face ((,class (:underline t :foreground ,accent1))))
  `(info-quoted-name ((,class (:foreground ,accent5))))
  `(info-string ((,class (:foreground ,accent3))))
  `(icompletep-determined ((,class :foreground ,accent5)))
  `(undo-tree-visualizer-current-face ((,class :foreground ,accent5)))
  `(undo-tree-visualizer-default-face ((,class :foreground ,shade6)))
  `(undo-tree-visualizer-unmodified-face ((,class :foreground ,accent6)))
  `(undo-tree-visualizer-register-face ((,class :foreground ,accent5)))
  `(slime-repl-inputed-output-face ((,class (:foreground ,accent5))))
  `(trailing-whitespace ((,class :foreground nil :background ,accent0)))
  `(rainbow-delimiters-depth-1-face ((,class :foreground ,shade7)))
  `(rainbow-delimiters-depth-2-face ((,class :foreground ,accent5)))
  `(rainbow-delimiters-depth-3-face ((,class :foreground ,accent6)))
  `(rainbow-delimiters-depth-4-face ((,class :foreground ,accent4)))
  `(rainbow-delimiters-depth-5-face ((,class :foreground ,accent1)))
  `(rainbow-delimiters-depth-6-face ((,class :foreground ,shade5)))
  `(rainbow-delimiters-depth-7-face ((,class :foreground ,accent5)))
  `(rainbow-delimiters-depth-8-face ((,class :foreground ,accent3)))
  `(magit-item-highlight ((,class :background ,shade2)))
  `(magit-section-heading        ((,class (:foreground ,accent1 :weight bold))))
  `(magit-hunk-heading           ((,class (:background ,shade2))))
  `(magit-section-highlight      ((,class (:background ,shade1))))
  `(magit-hunk-heading-highlight ((,class (:background ,shade2))))
  `(magit-diff-context-highlight ((,class (:background ,shade2 :foreground ,shade5))))
  `(magit-diffstat-added   ((,class (:foreground ,accent5))))
  `(magit-diffstat-removed ((,class (:foreground ,accent6))))
  `(magit-process-ok ((,class (:foreground ,accent6 :weight bold))))
  `(magit-process-ng ((,class (:foreground ,accent0 :weight bold))))
  `(magit-branch ((,class (:foreground ,accent4 :weight bold))))
  `(magit-log-author ((,class (:foreground ,shade5))))
  `(magit-hash ((,class (:foreground ,shade6))))
  `(magit-diff-file-header ((,class (:foreground ,shade6 :background ,shade2))))
  `(lazy-highlight ((,class (:foreground ,shade6 :background ,shade2))))
  `(term ((,class (:foreground ,shade6 :background ,shade0))))
  `(term-color-black ((,class (:foreground ,shade6 :background ,shade0))))
  `(term-color-blue ((,class (:foreground ,accent5 :background ,shade0))))
  `(term-color-red ((,class (:foreground ,accent0 :background ,shade0))))
  `(term-color-green ((,class (:foreground ,accent3 :background ,shade0))))
  `(term-color-yellow ((,class (:foreground ,accent2 :background ,shade0))))
  `(term-color-magenta ((,class (:foreground ,accent7 :background ,shade0))))
  `(term-color-cyan ((,class (:foreground ,accent4 :background ,shade0))))
  `(term-color-white ((,class (:foreground ,shade2 :background ,shade0))))
  `(rainbow-delimiters-unmatched-face ((,class :foreground ,accent0)))
  `(helm-header ((,class (:foreground ,shade6 :background ,shade0 :underline nil :box nil))))
  `(helm-source-header ((,class (:foreground ,accent1 :background ,shade0 :underline nil :weight bold))))
  `(helm-selection ((,class (:background ,shade1 :underline nil))))
  `(helm-selection-line ((,class (:background ,shade1))))
  `(helm-visible-mark ((,class (:foreground ,shade0 :background ,shade2))))
  `(helm-candidate-number ((,class (:foreground ,shade0 :background ,shade7))))
  `(helm-separator ((,class (:foreground ,accent5 :background ,shade0))))
  `(helm-time-zone-current ((,class (:foreground ,accent5 :background ,shade0))))
  `(helm-time-zone-home ((,class (:foreground ,accent5 :background ,shade0))))
  `(helm-buffer-not-saved ((,class (:foreground ,accent5 :background ,shade0))))
  `(helm-buffer-process ((,class (:foreground ,accent5 :background ,shade0))))
  `(helm-buffer-saved-out ((,class (:foreground ,shade7 :background ,shade0))))
  `(helm-buffer-size ((,class (:foreground ,shade7 :background ,shade0))))
  `(helm-ff-directory ((,class (:foreground ,accent6 :background ,shade0 :weight bold))))
  `(helm-ff-file ((,class (:foreground ,shade7 :background ,shade0 :weight normal))))
  `(helm-ff-executable ((,class (:foreground ,accent2 :background ,shade0 :weight normal))))
  `(helm-ff-invalid-symlink ((,class (:foreground ,accent7 :background ,shade0 :weight bold))))
  `(helm-ff-symlink ((,class (:foreground ,accent1 :background ,shade0 :weight bold))))
  `(helm-ff-prefix ((,class (:foreground ,shade0 :background ,accent1 :weight normal))))
  `(helm-grep-cmd-line ((,class (:foreground ,shade7 :background ,shade0))))
  `(helm-grep-file ((,class (:foreground ,shade7 :background ,shade0))))
  `(helm-grep-finish ((,class (:foreground ,shade6 :background ,shade0))))
  `(helm-grep-lineno ((,class (:foreground ,shade7 :background ,shade0))))
  `(helm-grep-match ((,class (:foreground nil :background nil :inherit helm-match))))
  `(helm-grep-running ((,class (:foreground ,accent6 :background ,shade0))))
  `(helm-moccur-buffer ((,class (:foreground ,accent6 :background ,shade0))))
  `(helm-source-go-package-godoc-description ((,class (:foreground ,accent3))))
  `(helm-bookmark-w3m ((,class (:foreground ,accent5))))
  `(company-echo-common ((,class (:foreground ,shade0 :background ,shade7))))
  `(company-preview ((,class (:background ,shade0 :foreground ,accent2))))
  `(company-preview-common ((,class (:foreground ,shade1 :foreground ,shade5))))
  `(company-preview-search ((,class (:foreground ,accent5 :background ,shade0))))
  `(company-scrollbar-bg ((,class (:background ,shade2))))
  `(company-scrollbar-fg ((,class (:foreground ,accent1))))
  `(company-tooltip ((,class (:foreground ,shade6 :background ,shade0 :bold t))))
  `(company-tooltop-annotation ((,class (:foreground ,accent4))))
  `(company-tooltip-common ((,class ( :foreground ,shade5))))
  `(company-tooltip-common-selection ((,class (:foreground ,accent3))))
  `(company-tooltip-mouse ((,class (:inherit highlight))))
  `(company-tooltip-selection ((,class (:background ,shade2 :foreground ,shade5))))
  `(company-template-field ((,class (:inherit region))))
  `(web-mode-builtin-face ((,class (:inherit ,font-lock-builtin-face))))
  `(web-mode-comment-face ((,class (:inherit ,font-lock-comment-face))))
  `(web-mode-constant-face ((,class (:inherit ,font-lock-constant-face))))
  `(web-mode-keyword-face ((,class (:foreground ,accent1))))
  `(web-mode-doctype-face ((,class (:inherit ,font-lock-comment-face))))
  `(web-mode-function-name-face ((,class (:inherit ,font-lock-function-name-face))))
  `(web-mode-string-face ((,class (:foreground ,accent3))))
  `(web-mode-type-face ((,class (:inherit ,font-lock-type-face))))
  `(web-mode-html-attr-name-face ((,class (:foreground ,accent6))))
  `(web-mode-html-attr-value-face ((,class (:foreground ,accent1))))
  `(web-mode-warning-face ((,class (:inherit ,font-lock-warning-face))))
  `(web-mode-html-tag-face ((,class (:foreground ,accent5))))
  `(jde-java-font-lock-package-face ((t (:foreground ,accent6))))
  `(jde-java-font-lock-public-face ((t (:foreground ,accent1))))
  `(jde-java-font-lock-private-face ((t (:foreground ,accent1))))
  `(jde-java-font-lock-constant-face ((t (:foreground ,accent4))))
  `(jde-java-font-lock-modifier-face ((t (:foreground ,accent7))))
  `(jde-jave-font-lock-protected-face ((t (:foreground ,accent1))))
  `(jde-java-font-lock-number-face ((t (:foreground ,accent6))))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
    (file-name-as-directory (file-name-directory load-file-name))))
```

## dracula-purple-light-theme.el

![](https://github.com/shadowrylander/.emacs.d/blob/main/themes/screenshots/dracula-purple-dark.png)
[Get the theme
here!](https://themer.dev/?colors.dark.shade0=%23222222&colors.dark.shade7=%23ab5dee&colors.light.shade0=%23ab5dee&colors.light.shade7=%23222222&activeColorSet=dark)

``` commonlisp
;;; dracula-purple-light-theme.el

;; Version: 1.0.1
;; Package-Requires: ((emacs "24"))

;;; Commentary:

;; Generated by the emacs theme template for themer.

;;; Code:

(deftheme dracula-purple-light)
(let ((class '((class color) (min-colors 89)))
  (shade0 "#ab5dee")
  (shade1 "#9755d1")
  (shade2 "#844cb4")
  (shade3 "#704497")
  (shade4 "#5d3b79")
  (shade5 "#49335c")
  (shade6 "#362a3f")
  (shade7 "#222222")
  (accent0 "#222222")
  (accent1 "#222222")
  (accent2 "#222222")
  (accent3 "#222222")
  (accent4 "#222222")
  (accent5 "#222222")
  (accent6 "#222222")
  (accent7 "#222222"))
(custom-theme-set-faces
  'dracula-purple-light
  `(default ((,class (:background ,shade0 :foreground ,shade7))))
  `(font-lock-builtin-face ((,class (:foreground ,accent5))))
  `(font-lock-comment-face ((,class (:foreground ,shade3))))
  `(font-lock-negation-char-face ((,class (:foreground ,accent4))))
  `(font-lock-reference-face ((,class (:foreground ,accent4))))
  `(font-lock-constant-face ((,class (:foreground ,accent4))))
  `(font-lock-doc-face ((,class (:foreground ,shade3))))
  `(font-lock-function-name-face ((,class (:foreground ,accent6 :bold t))))
  `(font-lock-keyword-face ((,class (:bold ,class :foreground ,accent1))))
  `(font-lock-string-face ((,class (:foreground ,accent3))))
  `(font-lock-type-face ((,class (:foreground ,accent5 ))))
  `(font-lock-variable-name-face ((,class (:foreground ,accent6))))
  `(font-lock-warning-face ((,class (:foreground ,accent0 :background ,shade1))))
  `(region ((,class (:background ,shade7 :foreground ,shade0))))
  `(highlight ((,class (:foreground ,shade5 :background ,shade2))))
  `(hl-line ((,class (:background  ,shade1))))
  `(fringe ((,class (:background ,shade1 :foreground ,shade4))))
  `(cursor ((,class (:background ,shade2))))
  `(show-paren-match-face ((,class (:background ,accent0))))
  `(isearch ((,class (:bold t :foreground ,accent0 :background ,shade2))))
  `(mode-line ((,class (:box (:line-width 1 :color nil) :bold t :foreground ,shade4 :background ,shade0))))
  `(mode-line-inactive ((,class (:box (:line-width 1 :color nil :style pressed-button) :foreground ,accent7 :background ,shade1 :weight normal))))
  `(mode-line-buffer-id ((,class (:bold t :foreground ,accent6 :background nil))))
  `(mode-line-highlight ((,class (:foreground ,accent1 :box nil :weight bold))))
  `(mode-line-emphasis ((,class (:foreground ,shade7))))
  `(vertical-border ((,class (:foreground ,shade5))))
  `(minibuffer-prompt ((,class (:bold t :foreground ,accent1))))
  `(default-italic ((,class (:italic t))))
  `(link ((,class (:foreground ,accent4 :underline t))))
  `(org-code ((,class (:foreground ,shade6))))
  `(org-hide ((,class (:foreground ,shade4))))
  `(org-level-1 ((,class (:bold t :foreground ,accent4 :height 1.1))))
  `(org-level-2 ((,class (:bold nil :foreground ,accent5))))
  `(org-level-3 ((,class (:bold t :foreground ,accent6))))
  `(org-level-4 ((,class (:bold nil :foreground ,accent7))))
  `(org-date ((,class (:underline t :foreground ,accent2) )))
  `(org-footnote  ((,class (:underline t :foreground ,shade4))))
  `(org-link ((,class (:underline t :foreground ,accent5 ))))
  `(org-special-keyword ((,class (:foreground ,accent1))))
  `(org-block ((,class (:foreground ,shade5))))
  `(org-quote ((,class (:inherit org-block :slant italic))))
  `(org-verse ((,class (:inherit org-block :slant italic))))
  `(org-todo ((,class (:box (:line-width 1 :color ,shade1) :foreground ,accent1 :bold t))))
  `(org-done ((,class (:box (:line-width 1 :color ,shade1) :foreground ,shade5 :bold t))))
  `(org-warning ((,class (:underline t :foreground ,accent2))))
  `(org-agenda-structure ((,class (:weight bold :foreground ,shade5 :box (:color ,shade4) :background ,shade2))))
  `(org-agenda-date ((,class (:foreground ,accent6 :height 1.1 ))))
  `(org-agenda-date-weekend ((,class (:weight normal :foreground ,shade4))))
  `(org-agenda-date-today ((,class (:weight bold :foreground ,accent1 :height 1.4))))
  `(org-agenda-done ((,class (:foreground ,shade3))))
  `(org-scheduled ((,class (:foreground ,accent5))))
  `(org-scheduled-today ((,class (:foreground ,accent6 :weight bold :height 1.2))))
  `(org-ellipsis ((,class (:foreground ,accent5))))
  `(org-verbatim ((,class (:foreground ,shade4))))
  `(org-document-info-keyword ((,class (:foreground ,accent6))))
  `(font-latex-bold-face ((,class (:foreground ,accent5))))
  `(font-latex-italic-face ((,class (:foreground ,accent7 :italic t))))
  `(font-latex-string-face ((,class (:foreground ,accent3))))
  `(font-latex-match-reference-keywords ((,class (:foreground ,accent4))))
  `(font-latex-match-variable-keywords ((,class (:foreground ,accent6))))
  `(ido-only-match ((,class (:foreground ,accent0))))
  `(org-sexp-date ((,class (:foreground ,shade4))))
  `(ido-first-match ((,class (:foreground ,accent1 :bold t))))
  `(gnus-header-content ((,class (:foreground ,accent1))))
  `(gnus-header-from ((,class (:foreground ,accent6))))
  `(gnus-header-name ((,class (:foreground ,accent5))))
  `(gnus-header-subject ((,class (:foreground ,accent6 :bold t))))
  `(mu4e-view-url-number-face ((,class (:foreground ,accent5))))
  `(mu4e-cited-1-face ((,class (:foreground ,shade6))))
  `(mu4e-cited-7-face ((,class (:foreground ,shade5))))
  `(mu4e-header-marks-face ((,class (:foreground ,accent5))))
  `(ffap ((,class (:foreground ,shade4))))
  `(js2-private-function-call ((,class (:foreground ,accent4))))
  `(js2-jsdoc-html-tag-delimiter ((,class (:foreground ,accent3))))
  `(js2-jsdoc-html-tag-name ((,class (:foreground ,accent2))))
  `(js2-external-variable ((,class (:foreground ,accent5  ))))
  `(js2-function-param ((,class (:foreground ,accent4))))
  `(js2-jsdoc-value ((,class (:foreground ,accent3))))
  `(js2-private-member ((,class (:foreground ,shade5))))
  `(js3-warning-face ((,class (:underline ,accent1))))
  `(js3-error-face ((,class (:underline ,accent0))))
  `(js3-external-variable-face ((,class (:foreground ,accent6))))
  `(js3-function-param-face ((,class (:foreground ,accent7))))
  `(js3-jsdoc-tag-face ((,class (:foreground ,accent1))))
  `(js3-instance-member-face ((,class (:foreground ,accent4))))
  `(warning ((,class (:foreground ,accent0))))
  `(ac-completion-face ((,class (:underline t :foreground ,accent1))))
  `(info-quoted-name ((,class (:foreground ,accent5))))
  `(info-string ((,class (:foreground ,accent3))))
  `(icompletep-determined ((,class :foreground ,accent5)))
  `(undo-tree-visualizer-current-face ((,class :foreground ,accent5)))
  `(undo-tree-visualizer-default-face ((,class :foreground ,shade6)))
  `(undo-tree-visualizer-unmodified-face ((,class :foreground ,accent6)))
  `(undo-tree-visualizer-register-face ((,class :foreground ,accent5)))
  `(slime-repl-inputed-output-face ((,class (:foreground ,accent5))))
  `(trailing-whitespace ((,class :foreground nil :background ,accent0)))
  `(rainbow-delimiters-depth-1-face ((,class :foreground ,shade7)))
  `(rainbow-delimiters-depth-2-face ((,class :foreground ,accent5)))
  `(rainbow-delimiters-depth-3-face ((,class :foreground ,accent6)))
  `(rainbow-delimiters-depth-4-face ((,class :foreground ,accent4)))
  `(rainbow-delimiters-depth-5-face ((,class :foreground ,accent1)))
  `(rainbow-delimiters-depth-6-face ((,class :foreground ,shade5)))
  `(rainbow-delimiters-depth-7-face ((,class :foreground ,accent5)))
  `(rainbow-delimiters-depth-8-face ((,class :foreground ,accent3)))
  `(magit-item-highlight ((,class :background ,shade2)))
  `(magit-section-heading        ((,class (:foreground ,accent1 :weight bold))))
  `(magit-hunk-heading           ((,class (:background ,shade2))))
  `(magit-section-highlight      ((,class (:background ,shade1))))
  `(magit-hunk-heading-highlight ((,class (:background ,shade2))))
  `(magit-diff-context-highlight ((,class (:background ,shade2 :foreground ,shade5))))
  `(magit-diffstat-added   ((,class (:foreground ,accent5))))
  `(magit-diffstat-removed ((,class (:foreground ,accent6))))
  `(magit-process-ok ((,class (:foreground ,accent6 :weight bold))))
  `(magit-process-ng ((,class (:foreground ,accent0 :weight bold))))
  `(magit-branch ((,class (:foreground ,accent4 :weight bold))))
  `(magit-log-author ((,class (:foreground ,shade5))))
  `(magit-hash ((,class (:foreground ,shade6))))
  `(magit-diff-file-header ((,class (:foreground ,shade6 :background ,shade2))))
  `(lazy-highlight ((,class (:foreground ,shade6 :background ,shade2))))
  `(term ((,class (:foreground ,shade6 :background ,shade0))))
  `(term-color-black ((,class (:foreground ,shade6 :background ,shade0))))
  `(term-color-blue ((,class (:foreground ,accent5 :background ,shade0))))
  `(term-color-red ((,class (:foreground ,accent0 :background ,shade0))))
  `(term-color-green ((,class (:foreground ,accent3 :background ,shade0))))
  `(term-color-yellow ((,class (:foreground ,accent2 :background ,shade0))))
  `(term-color-magenta ((,class (:foreground ,accent7 :background ,shade0))))
  `(term-color-cyan ((,class (:foreground ,accent4 :background ,shade0))))
  `(term-color-white ((,class (:foreground ,shade2 :background ,shade0))))
  `(rainbow-delimiters-unmatched-face ((,class :foreground ,accent0)))
  `(helm-header ((,class (:foreground ,shade6 :background ,shade0 :underline nil :box nil))))
  `(helm-source-header ((,class (:foreground ,accent1 :background ,shade0 :underline nil :weight bold))))
  `(helm-selection ((,class (:background ,shade1 :underline nil))))
  `(helm-selection-line ((,class (:background ,shade1))))
  `(helm-visible-mark ((,class (:foreground ,shade0 :background ,shade2))))
  `(helm-candidate-number ((,class (:foreground ,shade0 :background ,shade7))))
  `(helm-separator ((,class (:foreground ,accent5 :background ,shade0))))
  `(helm-time-zone-current ((,class (:foreground ,accent5 :background ,shade0))))
  `(helm-time-zone-home ((,class (:foreground ,accent5 :background ,shade0))))
  `(helm-buffer-not-saved ((,class (:foreground ,accent5 :background ,shade0))))
  `(helm-buffer-process ((,class (:foreground ,accent5 :background ,shade0))))
  `(helm-buffer-saved-out ((,class (:foreground ,shade7 :background ,shade0))))
  `(helm-buffer-size ((,class (:foreground ,shade7 :background ,shade0))))
  `(helm-ff-directory ((,class (:foreground ,accent6 :background ,shade0 :weight bold))))
  `(helm-ff-file ((,class (:foreground ,shade7 :background ,shade0 :weight normal))))
  `(helm-ff-executable ((,class (:foreground ,accent2 :background ,shade0 :weight normal))))
  `(helm-ff-invalid-symlink ((,class (:foreground ,accent7 :background ,shade0 :weight bold))))
  `(helm-ff-symlink ((,class (:foreground ,accent1 :background ,shade0 :weight bold))))
  `(helm-ff-prefix ((,class (:foreground ,shade0 :background ,accent1 :weight normal))))
  `(helm-grep-cmd-line ((,class (:foreground ,shade7 :background ,shade0))))
  `(helm-grep-file ((,class (:foreground ,shade7 :background ,shade0))))
  `(helm-grep-finish ((,class (:foreground ,shade6 :background ,shade0))))
  `(helm-grep-lineno ((,class (:foreground ,shade7 :background ,shade0))))
  `(helm-grep-match ((,class (:foreground nil :background nil :inherit helm-match))))
  `(helm-grep-running ((,class (:foreground ,accent6 :background ,shade0))))
  `(helm-moccur-buffer ((,class (:foreground ,accent6 :background ,shade0))))
  `(helm-source-go-package-godoc-description ((,class (:foreground ,accent3))))
  `(helm-bookmark-w3m ((,class (:foreground ,accent5))))
  `(company-echo-common ((,class (:foreground ,shade0 :background ,shade7))))
  `(company-preview ((,class (:background ,shade0 :foreground ,accent2))))
  `(company-preview-common ((,class (:foreground ,shade1 :foreground ,shade5))))
  `(company-preview-search ((,class (:foreground ,accent5 :background ,shade0))))
  `(company-scrollbar-bg ((,class (:background ,shade2))))
  `(company-scrollbar-fg ((,class (:foreground ,accent1))))
  `(company-tooltip ((,class (:foreground ,shade6 :background ,shade0 :bold t))))
  `(company-tooltop-annotation ((,class (:foreground ,accent4))))
  `(company-tooltip-common ((,class ( :foreground ,shade5))))
  `(company-tooltip-common-selection ((,class (:foreground ,accent3))))
  `(company-tooltip-mouse ((,class (:inherit highlight))))
  `(company-tooltip-selection ((,class (:background ,shade2 :foreground ,shade5))))
  `(company-template-field ((,class (:inherit region))))
  `(web-mode-builtin-face ((,class (:inherit ,font-lock-builtin-face))))
  `(web-mode-comment-face ((,class (:inherit ,font-lock-comment-face))))
  `(web-mode-constant-face ((,class (:inherit ,font-lock-constant-face))))
  `(web-mode-keyword-face ((,class (:foreground ,accent1))))
  `(web-mode-doctype-face ((,class (:inherit ,font-lock-comment-face))))
  `(web-mode-function-name-face ((,class (:inherit ,font-lock-function-name-face))))
  `(web-mode-string-face ((,class (:foreground ,accent3))))
  `(web-mode-type-face ((,class (:inherit ,font-lock-type-face))))
  `(web-mode-html-attr-name-face ((,class (:foreground ,accent6))))
  `(web-mode-html-attr-value-face ((,class (:foreground ,accent1))))
  `(web-mode-warning-face ((,class (:inherit ,font-lock-warning-face))))
  `(web-mode-html-tag-face ((,class (:foreground ,accent5))))
  `(jde-java-font-lock-package-face ((t (:foreground ,accent6))))
  `(jde-java-font-lock-public-face ((t (:foreground ,accent1))))
  `(jde-java-font-lock-private-face ((t (:foreground ,accent1))))
  `(jde-java-font-lock-constant-face ((t (:foreground ,accent4))))
  `(jde-java-font-lock-modifier-face ((t (:foreground ,accent7))))
  `(jde-jave-font-lock-protected-face ((t (:foreground ,accent1))))
  `(jde-java-font-lock-number-face ((t (:foreground ,accent6))))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
    (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'dracula-purple-light)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; dracula-purple-light-theme.el ends here
```

## dracula-purple-dark-theme.el

[Get the theme
here!](https://themer.dev/?colors.dark.shade0=%23222222&colors.dark.shade7=%23ab5dee&colors.light.shade0=%23ab5dee&colors.light.shade7=%23222222&activeColorSet=dark)

``` commonlisp
;;; dracula-purple-dark-theme.el

;; Version: 1.0.1
;; Package-Requires: ((emacs "24"))

;;; Commentary:

;; Generated by the emacs theme template for themer.

;;; Code:

(deftheme dracula-purple-dark)
(let ((class '((class color) (min-colors 89)))
  (shade0 "#222222")
  (shade1 "#362a3f")
  (shade2 "#49335c")
  (shade3 "#5d3b79")
  (shade4 "#704497")
  (shade5 "#844cb4")
  (shade6 "#9755d1")
  (shade7 "#ab5dee")
  (accent0 "#ab5dee")
  (accent1 "#ab5dee")
  (accent2 "#ab5dee")
  (accent3 "#ab5dee")
  (accent4 "#ab5dee")
  (accent5 "#ab5dee")
  (accent6 "#ab5dee")
  (accent7 "#ab5dee"))
(custom-theme-set-faces
  'dracula-purple-dark
  `(default ((,class (:background ,shade0 :foreground ,shade7))))
  `(font-lock-builtin-face ((,class (:foreground ,accent5))))
  `(font-lock-comment-face ((,class (:foreground ,shade3))))
  `(font-lock-negation-char-face ((,class (:foreground ,accent4))))
  `(font-lock-reference-face ((,class (:foreground ,accent4))))
  `(font-lock-constant-face ((,class (:foreground ,accent4))))
  `(font-lock-doc-face ((,class (:foreground ,shade3))))
  `(font-lock-function-name-face ((,class (:foreground ,accent6 :bold t))))
  `(font-lock-keyword-face ((,class (:bold ,class :foreground ,accent1))))
  `(font-lock-string-face ((,class (:foreground ,accent3))))
  `(font-lock-type-face ((,class (:foreground ,accent5 ))))
  `(font-lock-variable-name-face ((,class (:foreground ,accent6))))
  `(font-lock-warning-face ((,class (:foreground ,accent0 :background ,shade1))))
  `(region ((,class (:background ,shade7 :foreground ,shade0))))
  `(highlight ((,class (:foreground ,shade5 :background ,shade2))))
  `(hl-line ((,class (:background  ,shade1))))
  `(fringe ((,class (:background ,shade1 :foreground ,shade4))))
  `(cursor ((,class (:background ,shade2))))
  `(show-paren-match-face ((,class (:background ,accent0))))
  `(isearch ((,class (:bold t :foreground ,accent0 :background ,shade2))))
  `(mode-line ((,class (:box (:line-width 1 :color nil) :bold t :foreground ,shade4 :background ,shade0))))
  `(mode-line-inactive ((,class (:box (:line-width 1 :color nil :style pressed-button) :foreground ,accent7 :background ,shade1 :weight normal))))
  `(mode-line-buffer-id ((,class (:bold t :foreground ,accent6 :background nil))))
  `(mode-line-highlight ((,class (:foreground ,accent1 :box nil :weight bold))))
  `(mode-line-emphasis ((,class (:foreground ,shade7))))
  `(vertical-border ((,class (:foreground ,shade5))))
  `(minibuffer-prompt ((,class (:bold t :foreground ,accent1))))
  `(default-italic ((,class (:italic t))))
  `(link ((,class (:foreground ,accent4 :underline t))))
  `(org-code ((,class (:foreground ,shade6))))
  `(org-hide ((,class (:foreground ,shade4))))
  `(org-level-1 ((,class (:bold t :foreground ,accent4 :height 1.1))))
  `(org-level-2 ((,class (:bold nil :foreground ,accent5))))
  `(org-level-3 ((,class (:bold t :foreground ,accent6))))
  `(org-level-4 ((,class (:bold nil :foreground ,accent7))))
  `(org-date ((,class (:underline t :foreground ,accent2) )))
  `(org-footnote  ((,class (:underline t :foreground ,shade4))))
  `(org-link ((,class (:underline t :foreground ,accent5 ))))
  `(org-special-keyword ((,class (:foreground ,accent1))))
  `(org-block ((,class (:foreground ,shade5))))
  `(org-quote ((,class (:inherit org-block :slant italic))))
  `(org-verse ((,class (:inherit org-block :slant italic))))
  `(org-todo ((,class (:box (:line-width 1 :color ,shade1) :foreground ,accent1 :bold t))))
  `(org-done ((,class (:box (:line-width 1 :color ,shade1) :foreground ,shade5 :bold t))))
  `(org-warning ((,class (:underline t :foreground ,accent2))))
  `(org-agenda-structure ((,class (:weight bold :foreground ,shade5 :box (:color ,shade4) :background ,shade2))))
  `(org-agenda-date ((,class (:foreground ,accent6 :height 1.1 ))))
  `(org-agenda-date-weekend ((,class (:weight normal :foreground ,shade4))))
  `(org-agenda-date-today ((,class (:weight bold :foreground ,accent1 :height 1.4))))
  `(org-agenda-done ((,class (:foreground ,shade3))))
  `(org-scheduled ((,class (:foreground ,accent5))))
  `(org-scheduled-today ((,class (:foreground ,accent6 :weight bold :height 1.2))))
  `(org-ellipsis ((,class (:foreground ,accent5))))
  `(org-verbatim ((,class (:foreground ,shade4))))
  `(org-document-info-keyword ((,class (:foreground ,accent6))))
  `(font-latex-bold-face ((,class (:foreground ,accent5))))
  `(font-latex-italic-face ((,class (:foreground ,accent7 :italic t))))
  `(font-latex-string-face ((,class (:foreground ,accent3))))
  `(font-latex-match-reference-keywords ((,class (:foreground ,accent4))))
  `(font-latex-match-variable-keywords ((,class (:foreground ,accent6))))
  `(ido-only-match ((,class (:foreground ,accent0))))
  `(org-sexp-date ((,class (:foreground ,shade4))))
  `(ido-first-match ((,class (:foreground ,accent1 :bold t))))
  `(gnus-header-content ((,class (:foreground ,accent1))))
  `(gnus-header-from ((,class (:foreground ,accent6))))
  `(gnus-header-name ((,class (:foreground ,accent5))))
  `(gnus-header-subject ((,class (:foreground ,accent6 :bold t))))
  `(mu4e-view-url-number-face ((,class (:foreground ,accent5))))
  `(mu4e-cited-1-face ((,class (:foreground ,shade6))))
  `(mu4e-cited-7-face ((,class (:foreground ,shade5))))
  `(mu4e-header-marks-face ((,class (:foreground ,accent5))))
  `(ffap ((,class (:foreground ,shade4))))
  `(js2-private-function-call ((,class (:foreground ,accent4))))
  `(js2-jsdoc-html-tag-delimiter ((,class (:foreground ,accent3))))
  `(js2-jsdoc-html-tag-name ((,class (:foreground ,accent2))))
  `(js2-external-variable ((,class (:foreground ,accent5  ))))
  `(js2-function-param ((,class (:foreground ,accent4))))
  `(js2-jsdoc-value ((,class (:foreground ,accent3))))
  `(js2-private-member ((,class (:foreground ,shade5))))
  `(js3-warning-face ((,class (:underline ,accent1))))
  `(js3-error-face ((,class (:underline ,accent0))))
  `(js3-external-variable-face ((,class (:foreground ,accent6))))
  `(js3-function-param-face ((,class (:foreground ,accent7))))
  `(js3-jsdoc-tag-face ((,class (:foreground ,accent1))))
  `(js3-instance-member-face ((,class (:foreground ,accent4))))
  `(warning ((,class (:foreground ,accent0))))
  `(ac-completion-face ((,class (:underline t :foreground ,accent1))))
  `(info-quoted-name ((,class (:foreground ,accent5))))
  `(info-string ((,class (:foreground ,accent3))))
  `(icompletep-determined ((,class :foreground ,accent5)))
  `(undo-tree-visualizer-current-face ((,class :foreground ,accent5)))
  `(undo-tree-visualizer-default-face ((,class :foreground ,shade6)))
  `(undo-tree-visualizer-unmodified-face ((,class :foreground ,accent6)))
  `(undo-tree-visualizer-register-face ((,class :foreground ,accent5)))
  `(slime-repl-inputed-output-face ((,class (:foreground ,accent5))))
  `(trailing-whitespace ((,class :foreground nil :background ,accent0)))
  `(rainbow-delimiters-depth-1-face ((,class :foreground ,shade7)))
  `(rainbow-delimiters-depth-2-face ((,class :foreground ,accent5)))
  `(rainbow-delimiters-depth-3-face ((,class :foreground ,accent6)))
  `(rainbow-delimiters-depth-4-face ((,class :foreground ,accent4)))
  `(rainbow-delimiters-depth-5-face ((,class :foreground ,accent1)))
  `(rainbow-delimiters-depth-6-face ((,class :foreground ,shade5)))
  `(rainbow-delimiters-depth-7-face ((,class :foreground ,accent5)))
  `(rainbow-delimiters-depth-8-face ((,class :foreground ,accent3)))
  `(magit-item-highlight ((,class :background ,shade2)))
  `(magit-section-heading        ((,class (:foreground ,accent1 :weight bold))))
  `(magit-hunk-heading           ((,class (:background ,shade2))))
  `(magit-section-highlight      ((,class (:background ,shade1))))
  `(magit-hunk-heading-highlight ((,class (:background ,shade2))))
  `(magit-diff-context-highlight ((,class (:background ,shade2 :foreground ,shade5))))
  `(magit-diffstat-added   ((,class (:foreground ,accent5))))
  `(magit-diffstat-removed ((,class (:foreground ,accent6))))
  `(magit-process-ok ((,class (:foreground ,accent6 :weight bold))))
  `(magit-process-ng ((,class (:foreground ,accent0 :weight bold))))
  `(magit-branch ((,class (:foreground ,accent4 :weight bold))))
  `(magit-log-author ((,class (:foreground ,shade5))))
  `(magit-hash ((,class (:foreground ,shade6))))
  `(magit-diff-file-header ((,class (:foreground ,shade6 :background ,shade2))))
  `(lazy-highlight ((,class (:foreground ,shade6 :background ,shade2))))
  `(term ((,class (:foreground ,shade6 :background ,shade0))))
  `(term-color-black ((,class (:foreground ,shade6 :background ,shade0))))
  `(term-color-blue ((,class (:foreground ,accent5 :background ,shade0))))
  `(term-color-red ((,class (:foreground ,accent0 :background ,shade0))))
  `(term-color-green ((,class (:foreground ,accent3 :background ,shade0))))
  `(term-color-yellow ((,class (:foreground ,accent2 :background ,shade0))))
  `(term-color-magenta ((,class (:foreground ,accent7 :background ,shade0))))
  `(term-color-cyan ((,class (:foreground ,accent4 :background ,shade0))))
  `(term-color-white ((,class (:foreground ,shade2 :background ,shade0))))
  `(rainbow-delimiters-unmatched-face ((,class :foreground ,accent0)))
  `(helm-header ((,class (:foreground ,shade6 :background ,shade0 :underline nil :box nil))))
  `(helm-source-header ((,class (:foreground ,accent1 :background ,shade0 :underline nil :weight bold))))
  `(helm-selection ((,class (:background ,shade1 :underline nil))))
  `(helm-selection-line ((,class (:background ,shade1))))
  `(helm-visible-mark ((,class (:foreground ,shade0 :background ,shade2))))
  `(helm-candidate-number ((,class (:foreground ,shade0 :background ,shade7))))
  `(helm-separator ((,class (:foreground ,accent5 :background ,shade0))))
  `(helm-time-zone-current ((,class (:foreground ,accent5 :background ,shade0))))
  `(helm-time-zone-home ((,class (:foreground ,accent5 :background ,shade0))))
  `(helm-buffer-not-saved ((,class (:foreground ,accent5 :background ,shade0))))
  `(helm-buffer-process ((,class (:foreground ,accent5 :background ,shade0))))
  `(helm-buffer-saved-out ((,class (:foreground ,shade7 :background ,shade0))))
  `(helm-buffer-size ((,class (:foreground ,shade7 :background ,shade0))))
  `(helm-ff-directory ((,class (:foreground ,accent6 :background ,shade0 :weight bold))))
  `(helm-ff-file ((,class (:foreground ,shade7 :background ,shade0 :weight normal))))
  `(helm-ff-executable ((,class (:foreground ,accent2 :background ,shade0 :weight normal))))
  `(helm-ff-invalid-symlink ((,class (:foreground ,accent7 :background ,shade0 :weight bold))))
  `(helm-ff-symlink ((,class (:foreground ,accent1 :background ,shade0 :weight bold))))
  `(helm-ff-prefix ((,class (:foreground ,shade0 :background ,accent1 :weight normal))))
  `(helm-grep-cmd-line ((,class (:foreground ,shade7 :background ,shade0))))
  `(helm-grep-file ((,class (:foreground ,shade7 :background ,shade0))))
  `(helm-grep-finish ((,class (:foreground ,shade6 :background ,shade0))))
  `(helm-grep-lineno ((,class (:foreground ,shade7 :background ,shade0))))
  `(helm-grep-match ((,class (:foreground nil :background nil :inherit helm-match))))
  `(helm-grep-running ((,class (:foreground ,accent6 :background ,shade0))))
  `(helm-moccur-buffer ((,class (:foreground ,accent6 :background ,shade0))))
  `(helm-source-go-package-godoc-description ((,class (:foreground ,accent3))))
  `(helm-bookmark-w3m ((,class (:foreground ,accent5))))
  `(company-echo-common ((,class (:foreground ,shade0 :background ,shade7))))
  `(company-preview ((,class (:background ,shade0 :foreground ,accent2))))
  `(company-preview-common ((,class (:foreground ,shade1 :foreground ,shade5))))
  `(company-preview-search ((,class (:foreground ,accent5 :background ,shade0))))
  `(company-scrollbar-bg ((,class (:background ,shade2))))
  `(company-scrollbar-fg ((,class (:foreground ,accent1))))
  `(company-tooltip ((,class (:foreground ,shade6 :background ,shade0 :bold t))))
  `(company-tooltop-annotation ((,class (:foreground ,accent4))))
  `(company-tooltip-common ((,class ( :foreground ,shade5))))
  `(company-tooltip-common-selection ((,class (:foreground ,accent3))))
  `(company-tooltip-mouse ((,class (:inherit highlight))))
  `(company-tooltip-selection ((,class (:background ,shade2 :foreground ,shade5))))
  `(company-template-field ((,class (:inherit region))))
  `(web-mode-builtin-face ((,class (:inherit ,font-lock-builtin-face))))
  `(web-mode-comment-face ((,class (:inherit ,font-lock-comment-face))))
  `(web-mode-constant-face ((,class (:inherit ,font-lock-constant-face))))
  `(web-mode-keyword-face ((,class (:foreground ,accent1))))
  `(web-mode-doctype-face ((,class (:inherit ,font-lock-comment-face))))
  `(web-mode-function-name-face ((,class (:inherit ,font-lock-function-name-face))))
  `(web-mode-string-face ((,class (:foreground ,accent3))))
  `(web-mode-type-face ((,class (:inherit ,font-lock-type-face))))
  `(web-mode-html-attr-name-face ((,class (:foreground ,accent6))))
  `(web-mode-html-attr-value-face ((,class (:foreground ,accent1))))
  `(web-mode-warning-face ((,class (:inherit ,font-lock-warning-face))))
  `(web-mode-html-tag-face ((,class (:foreground ,accent5))))
  `(jde-java-font-lock-package-face ((t (:foreground ,accent6))))
  `(jde-java-font-lock-public-face ((t (:foreground ,accent1))))
  `(jde-java-font-lock-private-face ((t (:foreground ,accent1))))
  `(jde-java-font-lock-constant-face ((t (:foreground ,accent4))))
  `(jde-java-font-lock-modifier-face ((t (:foreground ,accent7))))
  `(jde-jave-font-lock-protected-face ((t (:foreground ,accent1))))
  `(jde-java-font-lock-number-face ((t (:foreground ,accent6))))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
    (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'dracula-purple-dark)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; dracula-purple-dark-theme.el ends here
```

## dracula-orange-light-theme.el

[Get the theme
here!](https://themer.dev/?colors.dark.shade0=%23222222&colors.dark.shade7=%23ffb86c&colors.light.shade0=%23ffb86c&colors.light.shade7=%23222222&activeColorSet=dark)

``` commonlisp
;;; dracula-orange-light-theme.el

;; Version: 1.0.1
;; Package-Requires: ((emacs "24"))

;;; Commentary:

;; Generated by the emacs theme template for themer.

;;; Code:

(deftheme dracula-orange-light)
(let ((class '((class color) (min-colors 89)))
  (shade0 "#ffb86c")
  (shade1 "#dfa361")
  (shade2 "#c08d57")
  (shade3 "#a0784c")
  (shade4 "#816242")
  (shade5 "#614d37")
  (shade6 "#42372d")
  (shade7 "#222222")
  (accent0 "#222222")
  (accent1 "#222222")
  (accent2 "#222222")
  (accent3 "#222222")
  (accent4 "#222222")
  (accent5 "#222222")
  (accent6 "#222222")
  (accent7 "#222222"))
(custom-theme-set-faces
  'dracula-orange-light
  `(default ((,class (:background ,shade0 :foreground ,shade7))))
  `(font-lock-builtin-face ((,class (:foreground ,accent5))))
  `(font-lock-comment-face ((,class (:foreground ,shade3))))
  `(font-lock-negation-char-face ((,class (:foreground ,accent4))))
  `(font-lock-reference-face ((,class (:foreground ,accent4))))
  `(font-lock-constant-face ((,class (:foreground ,accent4))))
  `(font-lock-doc-face ((,class (:foreground ,shade3))))
  `(font-lock-function-name-face ((,class (:foreground ,accent6 :bold t))))
  `(font-lock-keyword-face ((,class (:bold ,class :foreground ,accent1))))
  `(font-lock-string-face ((,class (:foreground ,accent3))))
  `(font-lock-type-face ((,class (:foreground ,accent5 ))))
  `(font-lock-variable-name-face ((,class (:foreground ,accent6))))
  `(font-lock-warning-face ((,class (:foreground ,accent0 :background ,shade1))))
  `(region ((,class (:background ,shade7 :foreground ,shade0))))
  `(highlight ((,class (:foreground ,shade5 :background ,shade2))))
  `(hl-line ((,class (:background  ,shade1))))
  `(fringe ((,class (:background ,shade1 :foreground ,shade4))))
  `(cursor ((,class (:background ,shade2))))
  `(show-paren-match-face ((,class (:background ,accent0))))
  `(isearch ((,class (:bold t :foreground ,accent0 :background ,shade2))))
  `(mode-line ((,class (:box (:line-width 1 :color nil) :bold t :foreground ,shade4 :background ,shade0))))
  `(mode-line-inactive ((,class (:box (:line-width 1 :color nil :style pressed-button) :foreground ,accent7 :background ,shade1 :weight normal))))
  `(mode-line-buffer-id ((,class (:bold t :foreground ,accent6 :background nil))))
  `(mode-line-highlight ((,class (:foreground ,accent1 :box nil :weight bold))))
  `(mode-line-emphasis ((,class (:foreground ,shade7))))
  `(vertical-border ((,class (:foreground ,shade5))))
  `(minibuffer-prompt ((,class (:bold t :foreground ,accent1))))
  `(default-italic ((,class (:italic t))))
  `(link ((,class (:foreground ,accent4 :underline t))))
  `(org-code ((,class (:foreground ,shade6))))
  `(org-hide ((,class (:foreground ,shade4))))
  `(org-level-1 ((,class (:bold t :foreground ,accent4 :height 1.1))))
  `(org-level-2 ((,class (:bold nil :foreground ,accent5))))
  `(org-level-3 ((,class (:bold t :foreground ,accent6))))
  `(org-level-4 ((,class (:bold nil :foreground ,accent7))))
  `(org-date ((,class (:underline t :foreground ,accent2) )))
  `(org-footnote  ((,class (:underline t :foreground ,shade4))))
  `(org-link ((,class (:underline t :foreground ,accent5 ))))
  `(org-special-keyword ((,class (:foreground ,accent1))))
  `(org-block ((,class (:foreground ,shade5))))
  `(org-quote ((,class (:inherit org-block :slant italic))))
  `(org-verse ((,class (:inherit org-block :slant italic))))
  `(org-todo ((,class (:box (:line-width 1 :color ,shade1) :foreground ,accent1 :bold t))))
  `(org-done ((,class (:box (:line-width 1 :color ,shade1) :foreground ,shade5 :bold t))))
  `(org-warning ((,class (:underline t :foreground ,accent2))))
  `(org-agenda-structure ((,class (:weight bold :foreground ,shade5 :box (:color ,shade4) :background ,shade2))))
  `(org-agenda-date ((,class (:foreground ,accent6 :height 1.1 ))))
  `(org-agenda-date-weekend ((,class (:weight normal :foreground ,shade4))))
  `(org-agenda-date-today ((,class (:weight bold :foreground ,accent1 :height 1.4))))
  `(org-agenda-done ((,class (:foreground ,shade3))))
  `(org-scheduled ((,class (:foreground ,accent5))))
  `(org-scheduled-today ((,class (:foreground ,accent6 :weight bold :height 1.2))))
  `(org-ellipsis ((,class (:foreground ,accent5))))
  `(org-verbatim ((,class (:foreground ,shade4))))
  `(org-document-info-keyword ((,class (:foreground ,accent6))))
  `(font-latex-bold-face ((,class (:foreground ,accent5))))
  `(font-latex-italic-face ((,class (:foreground ,accent7 :italic t))))
  `(font-latex-string-face ((,class (:foreground ,accent3))))
  `(font-latex-match-reference-keywords ((,class (:foreground ,accent4))))
  `(font-latex-match-variable-keywords ((,class (:foreground ,accent6))))
  `(ido-only-match ((,class (:foreground ,accent0))))
  `(org-sexp-date ((,class (:foreground ,shade4))))
  `(ido-first-match ((,class (:foreground ,accent1 :bold t))))
  `(gnus-header-content ((,class (:foreground ,accent1))))
  `(gnus-header-from ((,class (:foreground ,accent6))))
  `(gnus-header-name ((,class (:foreground ,accent5))))
  `(gnus-header-subject ((,class (:foreground ,accent6 :bold t))))
  `(mu4e-view-url-number-face ((,class (:foreground ,accent5))))
  `(mu4e-cited-1-face ((,class (:foreground ,shade6))))
  `(mu4e-cited-7-face ((,class (:foreground ,shade5))))
  `(mu4e-header-marks-face ((,class (:foreground ,accent5))))
  `(ffap ((,class (:foreground ,shade4))))
  `(js2-private-function-call ((,class (:foreground ,accent4))))
  `(js2-jsdoc-html-tag-delimiter ((,class (:foreground ,accent3))))
  `(js2-jsdoc-html-tag-name ((,class (:foreground ,accent2))))
  `(js2-external-variable ((,class (:foreground ,accent5  ))))
  `(js2-function-param ((,class (:foreground ,accent4))))
  `(js2-jsdoc-value ((,class (:foreground ,accent3))))
  `(js2-private-member ((,class (:foreground ,shade5))))
  `(js3-warning-face ((,class (:underline ,accent1))))
  `(js3-error-face ((,class (:underline ,accent0))))
  `(js3-external-variable-face ((,class (:foreground ,accent6))))
  `(js3-function-param-face ((,class (:foreground ,accent7))))
  `(js3-jsdoc-tag-face ((,class (:foreground ,accent1))))
  `(js3-instance-member-face ((,class (:foreground ,accent4))))
  `(warning ((,class (:foreground ,accent0))))
  `(ac-completion-face ((,class (:underline t :foreground ,accent1))))
  `(info-quoted-name ((,class (:foreground ,accent5))))
  `(info-string ((,class (:foreground ,accent3))))
  `(icompletep-determined ((,class :foreground ,accent5)))
  `(undo-tree-visualizer-current-face ((,class :foreground ,accent5)))
  `(undo-tree-visualizer-default-face ((,class :foreground ,shade6)))
  `(undo-tree-visualizer-unmodified-face ((,class :foreground ,accent6)))
  `(undo-tree-visualizer-register-face ((,class :foreground ,accent5)))
  `(slime-repl-inputed-output-face ((,class (:foreground ,accent5))))
  `(trailing-whitespace ((,class :foreground nil :background ,accent0)))
  `(rainbow-delimiters-depth-1-face ((,class :foreground ,shade7)))
  `(rainbow-delimiters-depth-2-face ((,class :foreground ,accent5)))
  `(rainbow-delimiters-depth-3-face ((,class :foreground ,accent6)))
  `(rainbow-delimiters-depth-4-face ((,class :foreground ,accent4)))
  `(rainbow-delimiters-depth-5-face ((,class :foreground ,accent1)))
  `(rainbow-delimiters-depth-6-face ((,class :foreground ,shade5)))
  `(rainbow-delimiters-depth-7-face ((,class :foreground ,accent5)))
  `(rainbow-delimiters-depth-8-face ((,class :foreground ,accent3)))
  `(magit-item-highlight ((,class :background ,shade2)))
  `(magit-section-heading        ((,class (:foreground ,accent1 :weight bold))))
  `(magit-hunk-heading           ((,class (:background ,shade2))))
  `(magit-section-highlight      ((,class (:background ,shade1))))
  `(magit-hunk-heading-highlight ((,class (:background ,shade2))))
  `(magit-diff-context-highlight ((,class (:background ,shade2 :foreground ,shade5))))
  `(magit-diffstat-added   ((,class (:foreground ,accent5))))
  `(magit-diffstat-removed ((,class (:foreground ,accent6))))
  `(magit-process-ok ((,class (:foreground ,accent6 :weight bold))))
  `(magit-process-ng ((,class (:foreground ,accent0 :weight bold))))
  `(magit-branch ((,class (:foreground ,accent4 :weight bold))))
  `(magit-log-author ((,class (:foreground ,shade5))))
  `(magit-hash ((,class (:foreground ,shade6))))
  `(magit-diff-file-header ((,class (:foreground ,shade6 :background ,shade2))))
  `(lazy-highlight ((,class (:foreground ,shade6 :background ,shade2))))
  `(term ((,class (:foreground ,shade6 :background ,shade0))))
  `(term-color-black ((,class (:foreground ,shade6 :background ,shade0))))
  `(term-color-blue ((,class (:foreground ,accent5 :background ,shade0))))
  `(term-color-red ((,class (:foreground ,accent0 :background ,shade0))))
  `(term-color-green ((,class (:foreground ,accent3 :background ,shade0))))
  `(term-color-yellow ((,class (:foreground ,accent2 :background ,shade0))))
  `(term-color-magenta ((,class (:foreground ,accent7 :background ,shade0))))
  `(term-color-cyan ((,class (:foreground ,accent4 :background ,shade0))))
  `(term-color-white ((,class (:foreground ,shade2 :background ,shade0))))
  `(rainbow-delimiters-unmatched-face ((,class :foreground ,accent0)))
  `(helm-header ((,class (:foreground ,shade6 :background ,shade0 :underline nil :box nil))))
  `(helm-source-header ((,class (:foreground ,accent1 :background ,shade0 :underline nil :weight bold))))
  `(helm-selection ((,class (:background ,shade1 :underline nil))))
  `(helm-selection-line ((,class (:background ,shade1))))
  `(helm-visible-mark ((,class (:foreground ,shade0 :background ,shade2))))
  `(helm-candidate-number ((,class (:foreground ,shade0 :background ,shade7))))
  `(helm-separator ((,class (:foreground ,accent5 :background ,shade0))))
  `(helm-time-zone-current ((,class (:foreground ,accent5 :background ,shade0))))
  `(helm-time-zone-home ((,class (:foreground ,accent5 :background ,shade0))))
  `(helm-buffer-not-saved ((,class (:foreground ,accent5 :background ,shade0))))
  `(helm-buffer-process ((,class (:foreground ,accent5 :background ,shade0))))
  `(helm-buffer-saved-out ((,class (:foreground ,shade7 :background ,shade0))))
  `(helm-buffer-size ((,class (:foreground ,shade7 :background ,shade0))))
  `(helm-ff-directory ((,class (:foreground ,accent6 :background ,shade0 :weight bold))))
  `(helm-ff-file ((,class (:foreground ,shade7 :background ,shade0 :weight normal))))
  `(helm-ff-executable ((,class (:foreground ,accent2 :background ,shade0 :weight normal))))
  `(helm-ff-invalid-symlink ((,class (:foreground ,accent7 :background ,shade0 :weight bold))))
  `(helm-ff-symlink ((,class (:foreground ,accent1 :background ,shade0 :weight bold))))
  `(helm-ff-prefix ((,class (:foreground ,shade0 :background ,accent1 :weight normal))))
  `(helm-grep-cmd-line ((,class (:foreground ,shade7 :background ,shade0))))
  `(helm-grep-file ((,class (:foreground ,shade7 :background ,shade0))))
  `(helm-grep-finish ((,class (:foreground ,shade6 :background ,shade0))))
  `(helm-grep-lineno ((,class (:foreground ,shade7 :background ,shade0))))
  `(helm-grep-match ((,class (:foreground nil :background nil :inherit helm-match))))
  `(helm-grep-running ((,class (:foreground ,accent6 :background ,shade0))))
  `(helm-moccur-buffer ((,class (:foreground ,accent6 :background ,shade0))))
  `(helm-source-go-package-godoc-description ((,class (:foreground ,accent3))))
  `(helm-bookmark-w3m ((,class (:foreground ,accent5))))
  `(company-echo-common ((,class (:foreground ,shade0 :background ,shade7))))
  `(company-preview ((,class (:background ,shade0 :foreground ,accent2))))
  `(company-preview-common ((,class (:foreground ,shade1 :foreground ,shade5))))
  `(company-preview-search ((,class (:foreground ,accent5 :background ,shade0))))
  `(company-scrollbar-bg ((,class (:background ,shade2))))
  `(company-scrollbar-fg ((,class (:foreground ,accent1))))
  `(company-tooltip ((,class (:foreground ,shade6 :background ,shade0 :bold t))))
  `(company-tooltop-annotation ((,class (:foreground ,accent4))))
  `(company-tooltip-common ((,class ( :foreground ,shade5))))
  `(company-tooltip-common-selection ((,class (:foreground ,accent3))))
  `(company-tooltip-mouse ((,class (:inherit highlight))))
  `(company-tooltip-selection ((,class (:background ,shade2 :foreground ,shade5))))
  `(company-template-field ((,class (:inherit region))))
  `(web-mode-builtin-face ((,class (:inherit ,font-lock-builtin-face))))
  `(web-mode-comment-face ((,class (:inherit ,font-lock-comment-face))))
  `(web-mode-constant-face ((,class (:inherit ,font-lock-constant-face))))
  `(web-mode-keyword-face ((,class (:foreground ,accent1))))
  `(web-mode-doctype-face ((,class (:inherit ,font-lock-comment-face))))
  `(web-mode-function-name-face ((,class (:inherit ,font-lock-function-name-face))))
  `(web-mode-string-face ((,class (:foreground ,accent3))))
  `(web-mode-type-face ((,class (:inherit ,font-lock-type-face))))
  `(web-mode-html-attr-name-face ((,class (:foreground ,accent6))))
  `(web-mode-html-attr-value-face ((,class (:foreground ,accent1))))
  `(web-mode-warning-face ((,class (:inherit ,font-lock-warning-face))))
  `(web-mode-html-tag-face ((,class (:foreground ,accent5))))
  `(jde-java-font-lock-package-face ((t (:foreground ,accent6))))
  `(jde-java-font-lock-public-face ((t (:foreground ,accent1))))
  `(jde-java-font-lock-private-face ((t (:foreground ,accent1))))
  `(jde-java-font-lock-constant-face ((t (:foreground ,accent4))))
  `(jde-java-font-lock-modifier-face ((t (:foreground ,accent7))))
  `(jde-jave-font-lock-protected-face ((t (:foreground ,accent1))))
  `(jde-java-font-lock-number-face ((t (:foreground ,accent6))))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
    (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'dracula-orange-light)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; dracula-orange-light-theme.el ends here
```

## dracula-orange-dark-theme.el

[Get the theme
here!](https://themer.dev/?colors.dark.shade0=%23222222&colors.dark.shade7=%23ffb86c&colors.light.shade0=%23ffb86c&colors.light.shade7=%23222222&activeColorSet=dark)

``` commonlisp
;;; dracula-orange-dark-theme.el

;; Version: 1.0.1
;; Package-Requires: ((emacs "24"))

;;; Commentary:

;; Generated by the emacs theme template for themer.

;;; Code:

(deftheme dracula-orange-dark)
(let ((class '((class color) (min-colors 89)))
  (shade0 "#222222")
  (shade1 "#42372d")
  (shade2 "#614d37")
  (shade3 "#816242")
  (shade4 "#a0784c")
  (shade5 "#c08d57")
  (shade6 "#dfa361")
  (shade7 "#ffb86c")
  (accent0 "#ffb86c")
  (accent1 "#ffb86c")
  (accent2 "#ffb86c")
  (accent3 "#ffb86c")
  (accent4 "#ffb86c")
  (accent5 "#ffb86c")
  (accent6 "#ffb86c")
  (accent7 "#ffb86c"))
(custom-theme-set-faces
  'dracula-orange-dark
  `(default ((,class (:background ,shade0 :foreground ,shade7))))
  `(font-lock-builtin-face ((,class (:foreground ,accent5))))
  `(font-lock-comment-face ((,class (:foreground ,shade3))))
  `(font-lock-negation-char-face ((,class (:foreground ,accent4))))
  `(font-lock-reference-face ((,class (:foreground ,accent4))))
  `(font-lock-constant-face ((,class (:foreground ,accent4))))
  `(font-lock-doc-face ((,class (:foreground ,shade3))))
  `(font-lock-function-name-face ((,class (:foreground ,accent6 :bold t))))
  `(font-lock-keyword-face ((,class (:bold ,class :foreground ,accent1))))
  `(font-lock-string-face ((,class (:foreground ,accent3))))
  `(font-lock-type-face ((,class (:foreground ,accent5 ))))
  `(font-lock-variable-name-face ((,class (:foreground ,accent6))))
  `(font-lock-warning-face ((,class (:foreground ,accent0 :background ,shade1))))
  `(region ((,class (:background ,shade7 :foreground ,shade0))))
  `(highlight ((,class (:foreground ,shade5 :background ,shade2))))
  `(hl-line ((,class (:background  ,shade1))))
  `(fringe ((,class (:background ,shade1 :foreground ,shade4))))
  `(cursor ((,class (:background ,shade2))))
  `(show-paren-match-face ((,class (:background ,accent0))))
  `(isearch ((,class (:bold t :foreground ,accent0 :background ,shade2))))
  `(mode-line ((,class (:box (:line-width 1 :color nil) :bold t :foreground ,shade4 :background ,shade0))))
  `(mode-line-inactive ((,class (:box (:line-width 1 :color nil :style pressed-button) :foreground ,accent7 :background ,shade1 :weight normal))))
  `(mode-line-buffer-id ((,class (:bold t :foreground ,accent6 :background nil))))
  `(mode-line-highlight ((,class (:foreground ,accent1 :box nil :weight bold))))
  `(mode-line-emphasis ((,class (:foreground ,shade7))))
  `(vertical-border ((,class (:foreground ,shade5))))
  `(minibuffer-prompt ((,class (:bold t :foreground ,accent1))))
  `(default-italic ((,class (:italic t))))
  `(link ((,class (:foreground ,accent4 :underline t))))
  `(org-code ((,class (:foreground ,shade6))))
  `(org-hide ((,class (:foreground ,shade4))))
  `(org-level-1 ((,class (:bold t :foreground ,accent4 :height 1.1))))
  `(org-level-2 ((,class (:bold nil :foreground ,accent5))))
  `(org-level-3 ((,class (:bold t :foreground ,accent6))))
  `(org-level-4 ((,class (:bold nil :foreground ,accent7))))
  `(org-date ((,class (:underline t :foreground ,accent2) )))
  `(org-footnote  ((,class (:underline t :foreground ,shade4))))
  `(org-link ((,class (:underline t :foreground ,accent5 ))))
  `(org-special-keyword ((,class (:foreground ,accent1))))
  `(org-block ((,class (:foreground ,shade5))))
  `(org-quote ((,class (:inherit org-block :slant italic))))
  `(org-verse ((,class (:inherit org-block :slant italic))))
  `(org-todo ((,class (:box (:line-width 1 :color ,shade1) :foreground ,accent1 :bold t))))
  `(org-done ((,class (:box (:line-width 1 :color ,shade1) :foreground ,shade5 :bold t))))
  `(org-warning ((,class (:underline t :foreground ,accent2))))
  `(org-agenda-structure ((,class (:weight bold :foreground ,shade5 :box (:color ,shade4) :background ,shade2))))
  `(org-agenda-date ((,class (:foreground ,accent6 :height 1.1 ))))
  `(org-agenda-date-weekend ((,class (:weight normal :foreground ,shade4))))
  `(org-agenda-date-today ((,class (:weight bold :foreground ,accent1 :height 1.4))))
  `(org-agenda-done ((,class (:foreground ,shade3))))
  `(org-scheduled ((,class (:foreground ,accent5))))
  `(org-scheduled-today ((,class (:foreground ,accent6 :weight bold :height 1.2))))
  `(org-ellipsis ((,class (:foreground ,accent5))))
  `(org-verbatim ((,class (:foreground ,shade4))))
  `(org-document-info-keyword ((,class (:foreground ,accent6))))
  `(font-latex-bold-face ((,class (:foreground ,accent5))))
  `(font-latex-italic-face ((,class (:foreground ,accent7 :italic t))))
  `(font-latex-string-face ((,class (:foreground ,accent3))))
  `(font-latex-match-reference-keywords ((,class (:foreground ,accent4))))
  `(font-latex-match-variable-keywords ((,class (:foreground ,accent6))))
  `(ido-only-match ((,class (:foreground ,accent0))))
  `(org-sexp-date ((,class (:foreground ,shade4))))
  `(ido-first-match ((,class (:foreground ,accent1 :bold t))))
  `(gnus-header-content ((,class (:foreground ,accent1))))
  `(gnus-header-from ((,class (:foreground ,accent6))))
  `(gnus-header-name ((,class (:foreground ,accent5))))
  `(gnus-header-subject ((,class (:foreground ,accent6 :bold t))))
  `(mu4e-view-url-number-face ((,class (:foreground ,accent5))))
  `(mu4e-cited-1-face ((,class (:foreground ,shade6))))
  `(mu4e-cited-7-face ((,class (:foreground ,shade5))))
  `(mu4e-header-marks-face ((,class (:foreground ,accent5))))
  `(ffap ((,class (:foreground ,shade4))))
  `(js2-private-function-call ((,class (:foreground ,accent4))))
  `(js2-jsdoc-html-tag-delimiter ((,class (:foreground ,accent3))))
  `(js2-jsdoc-html-tag-name ((,class (:foreground ,accent2))))
  `(js2-external-variable ((,class (:foreground ,accent5  ))))
  `(js2-function-param ((,class (:foreground ,accent4))))
  `(js2-jsdoc-value ((,class (:foreground ,accent3))))
  `(js2-private-member ((,class (:foreground ,shade5))))
  `(js3-warning-face ((,class (:underline ,accent1))))
  `(js3-error-face ((,class (:underline ,accent0))))
  `(js3-external-variable-face ((,class (:foreground ,accent6))))
  `(js3-function-param-face ((,class (:foreground ,accent7))))
  `(js3-jsdoc-tag-face ((,class (:foreground ,accent1))))
  `(js3-instance-member-face ((,class (:foreground ,accent4))))
  `(warning ((,class (:foreground ,accent0))))
  `(ac-completion-face ((,class (:underline t :foreground ,accent1))))
  `(info-quoted-name ((,class (:foreground ,accent5))))
  `(info-string ((,class (:foreground ,accent3))))
  `(icompletep-determined ((,class :foreground ,accent5)))
  `(undo-tree-visualizer-current-face ((,class :foreground ,accent5)))
  `(undo-tree-visualizer-default-face ((,class :foreground ,shade6)))
  `(undo-tree-visualizer-unmodified-face ((,class :foreground ,accent6)))
  `(undo-tree-visualizer-register-face ((,class :foreground ,accent5)))
  `(slime-repl-inputed-output-face ((,class (:foreground ,accent5))))
  `(trailing-whitespace ((,class :foreground nil :background ,accent0)))
  `(rainbow-delimiters-depth-1-face ((,class :foreground ,shade7)))
  `(rainbow-delimiters-depth-2-face ((,class :foreground ,accent5)))
  `(rainbow-delimiters-depth-3-face ((,class :foreground ,accent6)))
  `(rainbow-delimiters-depth-4-face ((,class :foreground ,accent4)))
  `(rainbow-delimiters-depth-5-face ((,class :foreground ,accent1)))
  `(rainbow-delimiters-depth-6-face ((,class :foreground ,shade5)))
  `(rainbow-delimiters-depth-7-face ((,class :foreground ,accent5)))
  `(rainbow-delimiters-depth-8-face ((,class :foreground ,accent3)))
  `(magit-item-highlight ((,class :background ,shade2)))
  `(magit-section-heading        ((,class (:foreground ,accent1 :weight bold))))
  `(magit-hunk-heading           ((,class (:background ,shade2))))
  `(magit-section-highlight      ((,class (:background ,shade1))))
  `(magit-hunk-heading-highlight ((,class (:background ,shade2))))
  `(magit-diff-context-highlight ((,class (:background ,shade2 :foreground ,shade5))))
  `(magit-diffstat-added   ((,class (:foreground ,accent5))))
  `(magit-diffstat-removed ((,class (:foreground ,accent6))))
  `(magit-process-ok ((,class (:foreground ,accent6 :weight bold))))
  `(magit-process-ng ((,class (:foreground ,accent0 :weight bold))))
  `(magit-branch ((,class (:foreground ,accent4 :weight bold))))
  `(magit-log-author ((,class (:foreground ,shade5))))
  `(magit-hash ((,class (:foreground ,shade6))))
  `(magit-diff-file-header ((,class (:foreground ,shade6 :background ,shade2))))
  `(lazy-highlight ((,class (:foreground ,shade6 :background ,shade2))))
  `(term ((,class (:foreground ,shade6 :background ,shade0))))
  `(term-color-black ((,class (:foreground ,shade6 :background ,shade0))))
  `(term-color-blue ((,class (:foreground ,accent5 :background ,shade0))))
  `(term-color-red ((,class (:foreground ,accent0 :background ,shade0))))
  `(term-color-green ((,class (:foreground ,accent3 :background ,shade0))))
  `(term-color-yellow ((,class (:foreground ,accent2 :background ,shade0))))
  `(term-color-magenta ((,class (:foreground ,accent7 :background ,shade0))))
  `(term-color-cyan ((,class (:foreground ,accent4 :background ,shade0))))
  `(term-color-white ((,class (:foreground ,shade2 :background ,shade0))))
  `(rainbow-delimiters-unmatched-face ((,class :foreground ,accent0)))
  `(helm-header ((,class (:foreground ,shade6 :background ,shade0 :underline nil :box nil))))
  `(helm-source-header ((,class (:foreground ,accent1 :background ,shade0 :underline nil :weight bold))))
  `(helm-selection ((,class (:background ,shade1 :underline nil))))
  `(helm-selection-line ((,class (:background ,shade1))))
  `(helm-visible-mark ((,class (:foreground ,shade0 :background ,shade2))))
  `(helm-candidate-number ((,class (:foreground ,shade0 :background ,shade7))))
  `(helm-separator ((,class (:foreground ,accent5 :background ,shade0))))
  `(helm-time-zone-current ((,class (:foreground ,accent5 :background ,shade0))))
  `(helm-time-zone-home ((,class (:foreground ,accent5 :background ,shade0))))
  `(helm-buffer-not-saved ((,class (:foreground ,accent5 :background ,shade0))))
  `(helm-buffer-process ((,class (:foreground ,accent5 :background ,shade0))))
  `(helm-buffer-saved-out ((,class (:foreground ,shade7 :background ,shade0))))
  `(helm-buffer-size ((,class (:foreground ,shade7 :background ,shade0))))
  `(helm-ff-directory ((,class (:foreground ,accent6 :background ,shade0 :weight bold))))
  `(helm-ff-file ((,class (:foreground ,shade7 :background ,shade0 :weight normal))))
  `(helm-ff-executable ((,class (:foreground ,accent2 :background ,shade0 :weight normal))))
  `(helm-ff-invalid-symlink ((,class (:foreground ,accent7 :background ,shade0 :weight bold))))
  `(helm-ff-symlink ((,class (:foreground ,accent1 :background ,shade0 :weight bold))))
  `(helm-ff-prefix ((,class (:foreground ,shade0 :background ,accent1 :weight normal))))
  `(helm-grep-cmd-line ((,class (:foreground ,shade7 :background ,shade0))))
  `(helm-grep-file ((,class (:foreground ,shade7 :background ,shade0))))
  `(helm-grep-finish ((,class (:foreground ,shade6 :background ,shade0))))
  `(helm-grep-lineno ((,class (:foreground ,shade7 :background ,shade0))))
  `(helm-grep-match ((,class (:foreground nil :background nil :inherit helm-match))))
  `(helm-grep-running ((,class (:foreground ,accent6 :background ,shade0))))
  `(helm-moccur-buffer ((,class (:foreground ,accent6 :background ,shade0))))
  `(helm-source-go-package-godoc-description ((,class (:foreground ,accent3))))
  `(helm-bookmark-w3m ((,class (:foreground ,accent5))))
  `(company-echo-common ((,class (:foreground ,shade0 :background ,shade7))))
  `(company-preview ((,class (:background ,shade0 :foreground ,accent2))))
  `(company-preview-common ((,class (:foreground ,shade1 :foreground ,shade5))))
  `(company-preview-search ((,class (:foreground ,accent5 :background ,shade0))))
  `(company-scrollbar-bg ((,class (:background ,shade2))))
  `(company-scrollbar-fg ((,class (:foreground ,accent1))))
  `(company-tooltip ((,class (:foreground ,shade6 :background ,shade0 :bold t))))
  `(company-tooltop-annotation ((,class (:foreground ,accent4))))
  `(company-tooltip-common ((,class ( :foreground ,shade5))))
  `(company-tooltip-common-selection ((,class (:foreground ,accent3))))
  `(company-tooltip-mouse ((,class (:inherit highlight))))
  `(company-tooltip-selection ((,class (:background ,shade2 :foreground ,shade5))))
  `(company-template-field ((,class (:inherit region))))
  `(web-mode-builtin-face ((,class (:inherit ,font-lock-builtin-face))))
  `(web-mode-comment-face ((,class (:inherit ,font-lock-comment-face))))
  `(web-mode-constant-face ((,class (:inherit ,font-lock-constant-face))))
  `(web-mode-keyword-face ((,class (:foreground ,accent1))))
  `(web-mode-doctype-face ((,class (:inherit ,font-lock-comment-face))))
  `(web-mode-function-name-face ((,class (:inherit ,font-lock-function-name-face))))
  `(web-mode-string-face ((,class (:foreground ,accent3))))
  `(web-mode-type-face ((,class (:inherit ,font-lock-type-face))))
  `(web-mode-html-attr-name-face ((,class (:foreground ,accent6))))
  `(web-mode-html-attr-value-face ((,class (:foreground ,accent1))))
  `(web-mode-warning-face ((,class (:inherit ,font-lock-warning-face))))
  `(web-mode-html-tag-face ((,class (:foreground ,accent5))))
  `(jde-java-font-lock-package-face ((t (:foreground ,accent6))))
  `(jde-java-font-lock-public-face ((t (:foreground ,accent1))))
  `(jde-java-font-lock-private-face ((t (:foreground ,accent1))))
  `(jde-java-font-lock-constant-face ((t (:foreground ,accent4))))
  `(jde-java-font-lock-modifier-face ((t (:foreground ,accent7))))
  `(jde-jave-font-lock-protected-face ((t (:foreground ,accent1))))
  `(jde-java-font-lock-number-face ((t (:foreground ,accent6))))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
    (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'dracula-orange-dark)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; dracula-orange-dark-theme.el ends here
```

## herschel-flamingo-pink-dark-theme.el

[Get the theme
here!](https://themer.dev/?colors.dark.shade0=%23222222&colors.dark.shade7=%23fca78e&colors.light.shade0=%23fca78e&colors.light.shade7=%23222222&activeColorSet=dark)

``` commonlisp
;;; herschel-flamingo-pink-dark-theme.el

;; Version: 1.0.1
;; Package-Requires: ((emacs "24"))

;;; Commentary:

;; Generated by the emacs theme template for themer.

;;; Code:

(deftheme herschel-flamingo-pink-dark)
(let ((class '((class color) (min-colors 89)))
  (shade0 "#222222")
  (shade1 "#413531")
  (shade2 "#604841")
  (shade3 "#7f5b50")
  (shade4 "#9f6e60")
  (shade5 "#be816f")
  (shade6 "#dd947f")
  (shade7 "#fca78e")
  (accent0 "#fca78e")
  (accent1 "#fca78e")
  (accent2 "#fca78e")
  (accent3 "#fca78e")
  (accent4 "#fca78e")
  (accent5 "#fca78e")
  (accent6 "#fca78e")
  (accent7 "#fca78e"))
(custom-theme-set-faces
  'herschel-flamingo-pink-dark
  `(default ((,class (:background ,shade0 :foreground ,shade7))))
  `(font-lock-builtin-face ((,class (:foreground ,accent5))))
  `(font-lock-comment-face ((,class (:foreground ,shade3))))
  `(font-lock-negation-char-face ((,class (:foreground ,accent4))))
  `(font-lock-reference-face ((,class (:foreground ,accent4))))
  `(font-lock-constant-face ((,class (:foreground ,accent4))))
  `(font-lock-doc-face ((,class (:foreground ,shade3))))
  `(font-lock-function-name-face ((,class (:foreground ,accent6 :bold t))))
  `(font-lock-keyword-face ((,class (:bold ,class :foreground ,accent1))))
  `(font-lock-string-face ((,class (:foreground ,accent3))))
  `(font-lock-type-face ((,class (:foreground ,accent5 ))))
  `(font-lock-variable-name-face ((,class (:foreground ,accent6))))
  `(font-lock-warning-face ((,class (:foreground ,accent0 :background ,shade1))))
  `(region ((,class (:background ,shade7 :foreground ,shade0))))
  `(highlight ((,class (:foreground ,shade5 :background ,shade2))))
  `(hl-line ((,class (:background  ,shade1))))
  `(fringe ((,class (:background ,shade1 :foreground ,shade4))))
  `(cursor ((,class (:background ,shade2))))
  `(show-paren-match-face ((,class (:background ,accent0))))
  `(isearch ((,class (:bold t :foreground ,accent0 :background ,shade2))))
  `(mode-line ((,class (:box (:line-width 1 :color nil) :bold t :foreground ,shade4 :background ,shade0))))
  `(mode-line-inactive ((,class (:box (:line-width 1 :color nil :style pressed-button) :foreground ,accent7 :background ,shade1 :weight normal))))
  `(mode-line-buffer-id ((,class (:bold t :foreground ,accent6 :background nil))))
  `(mode-line-highlight ((,class (:foreground ,accent1 :box nil :weight bold))))
  `(mode-line-emphasis ((,class (:foreground ,shade7))))
  `(vertical-border ((,class (:foreground ,shade5))))
  `(minibuffer-prompt ((,class (:bold t :foreground ,accent1))))
  `(default-italic ((,class (:italic t))))
  `(link ((,class (:foreground ,accent4 :underline t))))
  `(org-code ((,class (:foreground ,shade6))))
  `(org-hide ((,class (:foreground ,shade4))))
  `(org-level-1 ((,class (:bold t :foreground ,accent4 :height 1.1))))
  `(org-level-2 ((,class (:bold nil :foreground ,accent5))))
  `(org-level-3 ((,class (:bold t :foreground ,accent6))))
  `(org-level-4 ((,class (:bold nil :foreground ,accent7))))
  `(org-date ((,class (:underline t :foreground ,accent2) )))
  `(org-footnote  ((,class (:underline t :foreground ,shade4))))
  `(org-link ((,class (:underline t :foreground ,accent5 ))))
  `(org-special-keyword ((,class (:foreground ,accent1))))
  `(org-block ((,class (:foreground ,shade5))))
  `(org-quote ((,class (:inherit org-block :slant italic))))
  `(org-verse ((,class (:inherit org-block :slant italic))))
  `(org-todo ((,class (:box (:line-width 1 :color ,shade1) :foreground ,accent1 :bold t))))
  `(org-done ((,class (:box (:line-width 1 :color ,shade1) :foreground ,shade5 :bold t))))
  `(org-warning ((,class (:underline t :foreground ,accent2))))
  `(org-agenda-structure ((,class (:weight bold :foreground ,shade5 :box (:color ,shade4) :background ,shade2))))
  `(org-agenda-date ((,class (:foreground ,accent6 :height 1.1 ))))
  `(org-agenda-date-weekend ((,class (:weight normal :foreground ,shade4))))
  `(org-agenda-date-today ((,class (:weight bold :foreground ,accent1 :height 1.4))))
  `(org-agenda-done ((,class (:foreground ,shade3))))
  `(org-scheduled ((,class (:foreground ,accent5))))
  `(org-scheduled-today ((,class (:foreground ,accent6 :weight bold :height 1.2))))
  `(org-ellipsis ((,class (:foreground ,accent5))))
  `(org-verbatim ((,class (:foreground ,shade4))))
  `(org-document-info-keyword ((,class (:foreground ,accent6))))
  `(font-latex-bold-face ((,class (:foreground ,accent5))))
  `(font-latex-italic-face ((,class (:foreground ,accent7 :italic t))))
  `(font-latex-string-face ((,class (:foreground ,accent3))))
  `(font-latex-match-reference-keywords ((,class (:foreground ,accent4))))
  `(font-latex-match-variable-keywords ((,class (:foreground ,accent6))))
  `(ido-only-match ((,class (:foreground ,accent0))))
  `(org-sexp-date ((,class (:foreground ,shade4))))
  `(ido-first-match ((,class (:foreground ,accent1 :bold t))))
  `(gnus-header-content ((,class (:foreground ,accent1))))
  `(gnus-header-from ((,class (:foreground ,accent6))))
  `(gnus-header-name ((,class (:foreground ,accent5))))
  `(gnus-header-subject ((,class (:foreground ,accent6 :bold t))))
  `(mu4e-view-url-number-face ((,class (:foreground ,accent5))))
  `(mu4e-cited-1-face ((,class (:foreground ,shade6))))
  `(mu4e-cited-7-face ((,class (:foreground ,shade5))))
  `(mu4e-header-marks-face ((,class (:foreground ,accent5))))
  `(ffap ((,class (:foreground ,shade4))))
  `(js2-private-function-call ((,class (:foreground ,accent4))))
  `(js2-jsdoc-html-tag-delimiter ((,class (:foreground ,accent3))))
  `(js2-jsdoc-html-tag-name ((,class (:foreground ,accent2))))
  `(js2-external-variable ((,class (:foreground ,accent5  ))))
  `(js2-function-param ((,class (:foreground ,accent4))))
  `(js2-jsdoc-value ((,class (:foreground ,accent3))))
  `(js2-private-member ((,class (:foreground ,shade5))))
  `(js3-warning-face ((,class (:underline ,accent1))))
  `(js3-error-face ((,class (:underline ,accent0))))
  `(js3-external-variable-face ((,class (:foreground ,accent6))))
  `(js3-function-param-face ((,class (:foreground ,accent7))))
  `(js3-jsdoc-tag-face ((,class (:foreground ,accent1))))
  `(js3-instance-member-face ((,class (:foreground ,accent4))))
  `(warning ((,class (:foreground ,accent0))))
  `(ac-completion-face ((,class (:underline t :foreground ,accent1))))
  `(info-quoted-name ((,class (:foreground ,accent5))))
  `(info-string ((,class (:foreground ,accent3))))
  `(icompletep-determined ((,class :foreground ,accent5)))
  `(undo-tree-visualizer-current-face ((,class :foreground ,accent5)))
  `(undo-tree-visualizer-default-face ((,class :foreground ,shade6)))
  `(undo-tree-visualizer-unmodified-face ((,class :foreground ,accent6)))
  `(undo-tree-visualizer-register-face ((,class :foreground ,accent5)))
  `(slime-repl-inputed-output-face ((,class (:foreground ,accent5))))
  `(trailing-whitespace ((,class :foreground nil :background ,accent0)))
  `(rainbow-delimiters-depth-1-face ((,class :foreground ,shade7)))
  `(rainbow-delimiters-depth-2-face ((,class :foreground ,accent5)))
  `(rainbow-delimiters-depth-3-face ((,class :foreground ,accent6)))
  `(rainbow-delimiters-depth-4-face ((,class :foreground ,accent4)))
  `(rainbow-delimiters-depth-5-face ((,class :foreground ,accent1)))
  `(rainbow-delimiters-depth-6-face ((,class :foreground ,shade5)))
  `(rainbow-delimiters-depth-7-face ((,class :foreground ,accent5)))
  `(rainbow-delimiters-depth-8-face ((,class :foreground ,accent3)))
  `(magit-item-highlight ((,class :background ,shade2)))
  `(magit-section-heading        ((,class (:foreground ,accent1 :weight bold))))
  `(magit-hunk-heading           ((,class (:background ,shade2))))
  `(magit-section-highlight      ((,class (:background ,shade1))))
  `(magit-hunk-heading-highlight ((,class (:background ,shade2))))
  `(magit-diff-context-highlight ((,class (:background ,shade2 :foreground ,shade5))))
  `(magit-diffstat-added   ((,class (:foreground ,accent5))))
  `(magit-diffstat-removed ((,class (:foreground ,accent6))))
  `(magit-process-ok ((,class (:foreground ,accent6 :weight bold))))
  `(magit-process-ng ((,class (:foreground ,accent0 :weight bold))))
  `(magit-branch ((,class (:foreground ,accent4 :weight bold))))
  `(magit-log-author ((,class (:foreground ,shade5))))
  `(magit-hash ((,class (:foreground ,shade6))))
  `(magit-diff-file-header ((,class (:foreground ,shade6 :background ,shade2))))
  `(lazy-highlight ((,class (:foreground ,shade6 :background ,shade2))))
  `(term ((,class (:foreground ,shade6 :background ,shade0))))
  `(term-color-black ((,class (:foreground ,shade6 :background ,shade0))))
  `(term-color-blue ((,class (:foreground ,accent5 :background ,shade0))))
  `(term-color-red ((,class (:foreground ,accent0 :background ,shade0))))
  `(term-color-green ((,class (:foreground ,accent3 :background ,shade0))))
  `(term-color-yellow ((,class (:foreground ,accent2 :background ,shade0))))
  `(term-color-magenta ((,class (:foreground ,accent7 :background ,shade0))))
  `(term-color-cyan ((,class (:foreground ,accent4 :background ,shade0))))
  `(term-color-white ((,class (:foreground ,shade2 :background ,shade0))))
  `(rainbow-delimiters-unmatched-face ((,class :foreground ,accent0)))
  `(helm-header ((,class (:foreground ,shade6 :background ,shade0 :underline nil :box nil))))
  `(helm-source-header ((,class (:foreground ,accent1 :background ,shade0 :underline nil :weight bold))))
  `(helm-selection ((,class (:background ,shade1 :underline nil))))
  `(helm-selection-line ((,class (:background ,shade1))))
  `(helm-visible-mark ((,class (:foreground ,shade0 :background ,shade2))))
  `(helm-candidate-number ((,class (:foreground ,shade0 :background ,shade7))))
  `(helm-separator ((,class (:foreground ,accent5 :background ,shade0))))
  `(helm-time-zone-current ((,class (:foreground ,accent5 :background ,shade0))))
  `(helm-time-zone-home ((,class (:foreground ,accent5 :background ,shade0))))
  `(helm-buffer-not-saved ((,class (:foreground ,accent5 :background ,shade0))))
  `(helm-buffer-process ((,class (:foreground ,accent5 :background ,shade0))))
  `(helm-buffer-saved-out ((,class (:foreground ,shade7 :background ,shade0))))
  `(helm-buffer-size ((,class (:foreground ,shade7 :background ,shade0))))
  `(helm-ff-directory ((,class (:foreground ,accent6 :background ,shade0 :weight bold))))
  `(helm-ff-file ((,class (:foreground ,shade7 :background ,shade0 :weight normal))))
  `(helm-ff-executable ((,class (:foreground ,accent2 :background ,shade0 :weight normal))))
  `(helm-ff-invalid-symlink ((,class (:foreground ,accent7 :background ,shade0 :weight bold))))
  `(helm-ff-symlink ((,class (:foreground ,accent1 :background ,shade0 :weight bold))))
  `(helm-ff-prefix ((,class (:foreground ,shade0 :background ,accent1 :weight normal))))
  `(helm-grep-cmd-line ((,class (:foreground ,shade7 :background ,shade0))))
  `(helm-grep-file ((,class (:foreground ,shade7 :background ,shade0))))
  `(helm-grep-finish ((,class (:foreground ,shade6 :background ,shade0))))
  `(helm-grep-lineno ((,class (:foreground ,shade7 :background ,shade0))))
  `(helm-grep-match ((,class (:foreground nil :background nil :inherit helm-match))))
  `(helm-grep-running ((,class (:foreground ,accent6 :background ,shade0))))
  `(helm-moccur-buffer ((,class (:foreground ,accent6 :background ,shade0))))
  `(helm-source-go-package-godoc-description ((,class (:foreground ,accent3))))
  `(helm-bookmark-w3m ((,class (:foreground ,accent5))))
  `(company-echo-common ((,class (:foreground ,shade0 :background ,shade7))))
  `(company-preview ((,class (:background ,shade0 :foreground ,accent2))))
  `(company-preview-common ((,class (:foreground ,shade1 :foreground ,shade5))))
  `(company-preview-search ((,class (:foreground ,accent5 :background ,shade0))))
  `(company-scrollbar-bg ((,class (:background ,shade2))))
  `(company-scrollbar-fg ((,class (:foreground ,accent1))))
  `(company-tooltip ((,class (:foreground ,shade6 :background ,shade0 :bold t))))
  `(company-tooltop-annotation ((,class (:foreground ,accent4))))
  `(company-tooltip-common ((,class ( :foreground ,shade5))))
  `(company-tooltip-common-selection ((,class (:foreground ,accent3))))
  `(company-tooltip-mouse ((,class (:inherit highlight))))
  `(company-tooltip-selection ((,class (:background ,shade2 :foreground ,shade5))))
  `(company-template-field ((,class (:inherit region))))
  `(web-mode-builtin-face ((,class (:inherit ,font-lock-builtin-face))))
  `(web-mode-comment-face ((,class (:inherit ,font-lock-comment-face))))
  `(web-mode-constant-face ((,class (:inherit ,font-lock-constant-face))))
  `(web-mode-keyword-face ((,class (:foreground ,accent1))))
  `(web-mode-doctype-face ((,class (:inherit ,font-lock-comment-face))))
  `(web-mode-function-name-face ((,class (:inherit ,font-lock-function-name-face))))
  `(web-mode-string-face ((,class (:foreground ,accent3))))
  `(web-mode-type-face ((,class (:inherit ,font-lock-type-face))))
  `(web-mode-html-attr-name-face ((,class (:foreground ,accent6))))
  `(web-mode-html-attr-value-face ((,class (:foreground ,accent1))))
  `(web-mode-warning-face ((,class (:inherit ,font-lock-warning-face))))
  `(web-mode-html-tag-face ((,class (:foreground ,accent5))))
  `(jde-java-font-lock-package-face ((t (:foreground ,accent6))))
  `(jde-java-font-lock-public-face ((t (:foreground ,accent1))))
  `(jde-java-font-lock-private-face ((t (:foreground ,accent1))))
  `(jde-java-font-lock-constant-face ((t (:foreground ,accent4))))
  `(jde-java-font-lock-modifier-face ((t (:foreground ,accent7))))
  `(jde-jave-font-lock-protected-face ((t (:foreground ,accent1))))
  `(jde-java-font-lock-number-face ((t (:foreground ,accent6))))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
    (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'herschel-flamingo-pink-dark)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; herschel-flamingo-pink-dark-theme.el ends here
```

## herschel-flamingo-pink-light-theme.el

[Get the theme
here!](https://themer.dev/?colors.dark.shade0=%23222222&colors.dark.shade7=%23fca78e&colors.light.shade0=%23fca78e&colors.light.shade7=%23222222&activeColorSet=dark)

``` commonlisp
;;; herschel-flamingo-pink-light-theme.el

;; Version: 1.0.1
;; Package-Requires: ((emacs "24"))

;;; Commentary:

;; Generated by the emacs theme template for themer.

;;; Code:

(deftheme herschel-flamingo-pink-light)
(let ((class '((class color) (min-colors 89)))
  (shade0 "#fca78e")
  (shade1 "#dd947f")
  (shade2 "#be816f")
  (shade3 "#9f6e60")
  (shade4 "#7f5b50")
  (shade5 "#604841")
  (shade6 "#413531")
  (shade7 "#222222")
  (accent0 "#222222")
  (accent1 "#222222")
  (accent2 "#222222")
  (accent3 "#222222")
  (accent4 "#222222")
  (accent5 "#222222")
  (accent6 "#222222")
  (accent7 "#222222"))
(custom-theme-set-faces
  'herschel-flamingo-pink-light
  `(default ((,class (:background ,shade0 :foreground ,shade7))))
  `(font-lock-builtin-face ((,class (:foreground ,accent5))))
  `(font-lock-comment-face ((,class (:foreground ,shade3))))
  `(font-lock-negation-char-face ((,class (:foreground ,accent4))))
  `(font-lock-reference-face ((,class (:foreground ,accent4))))
  `(font-lock-constant-face ((,class (:foreground ,accent4))))
  `(font-lock-doc-face ((,class (:foreground ,shade3))))
  `(font-lock-function-name-face ((,class (:foreground ,accent6 :bold t))))
  `(font-lock-keyword-face ((,class (:bold ,class :foreground ,accent1))))
  `(font-lock-string-face ((,class (:foreground ,accent3))))
  `(font-lock-type-face ((,class (:foreground ,accent5 ))))
  `(font-lock-variable-name-face ((,class (:foreground ,accent6))))
  `(font-lock-warning-face ((,class (:foreground ,accent0 :background ,shade1))))
  `(region ((,class (:background ,shade7 :foreground ,shade0))))
  `(highlight ((,class (:foreground ,shade5 :background ,shade2))))
  `(hl-line ((,class (:background  ,shade1))))
  `(fringe ((,class (:background ,shade1 :foreground ,shade4))))
  `(cursor ((,class (:background ,shade2))))
  `(show-paren-match-face ((,class (:background ,accent0))))
  `(isearch ((,class (:bold t :foreground ,accent0 :background ,shade2))))
  `(mode-line ((,class (:box (:line-width 1 :color nil) :bold t :foreground ,shade4 :background ,shade0))))
  `(mode-line-inactive ((,class (:box (:line-width 1 :color nil :style pressed-button) :foreground ,accent7 :background ,shade1 :weight normal))))
  `(mode-line-buffer-id ((,class (:bold t :foreground ,accent6 :background nil))))
  `(mode-line-highlight ((,class (:foreground ,accent1 :box nil :weight bold))))
  `(mode-line-emphasis ((,class (:foreground ,shade7))))
  `(vertical-border ((,class (:foreground ,shade5))))
  `(minibuffer-prompt ((,class (:bold t :foreground ,accent1))))
  `(default-italic ((,class (:italic t))))
  `(link ((,class (:foreground ,accent4 :underline t))))
  `(org-code ((,class (:foreground ,shade6))))
  `(org-hide ((,class (:foreground ,shade4))))
  `(org-level-1 ((,class (:bold t :foreground ,accent4 :height 1.1))))
  `(org-level-2 ((,class (:bold nil :foreground ,accent5))))
  `(org-level-3 ((,class (:bold t :foreground ,accent6))))
  `(org-level-4 ((,class (:bold nil :foreground ,accent7))))
  `(org-date ((,class (:underline t :foreground ,accent2) )))
  `(org-footnote  ((,class (:underline t :foreground ,shade4))))
  `(org-link ((,class (:underline t :foreground ,accent5 ))))
  `(org-special-keyword ((,class (:foreground ,accent1))))
  `(org-block ((,class (:foreground ,shade5))))
  `(org-quote ((,class (:inherit org-block :slant italic))))
  `(org-verse ((,class (:inherit org-block :slant italic))))
  `(org-todo ((,class (:box (:line-width 1 :color ,shade1) :foreground ,accent1 :bold t))))
  `(org-done ((,class (:box (:line-width 1 :color ,shade1) :foreground ,shade5 :bold t))))
  `(org-warning ((,class (:underline t :foreground ,accent2))))
  `(org-agenda-structure ((,class (:weight bold :foreground ,shade5 :box (:color ,shade4) :background ,shade2))))
  `(org-agenda-date ((,class (:foreground ,accent6 :height 1.1 ))))
  `(org-agenda-date-weekend ((,class (:weight normal :foreground ,shade4))))
  `(org-agenda-date-today ((,class (:weight bold :foreground ,accent1 :height 1.4))))
  `(org-agenda-done ((,class (:foreground ,shade3))))
  `(org-scheduled ((,class (:foreground ,accent5))))
  `(org-scheduled-today ((,class (:foreground ,accent6 :weight bold :height 1.2))))
  `(org-ellipsis ((,class (:foreground ,accent5))))
  `(org-verbatim ((,class (:foreground ,shade4))))
  `(org-document-info-keyword ((,class (:foreground ,accent6))))
  `(font-latex-bold-face ((,class (:foreground ,accent5))))
  `(font-latex-italic-face ((,class (:foreground ,accent7 :italic t))))
  `(font-latex-string-face ((,class (:foreground ,accent3))))
  `(font-latex-match-reference-keywords ((,class (:foreground ,accent4))))
  `(font-latex-match-variable-keywords ((,class (:foreground ,accent6))))
  `(ido-only-match ((,class (:foreground ,accent0))))
  `(org-sexp-date ((,class (:foreground ,shade4))))
  `(ido-first-match ((,class (:foreground ,accent1 :bold t))))
  `(gnus-header-content ((,class (:foreground ,accent1))))
  `(gnus-header-from ((,class (:foreground ,accent6))))
  `(gnus-header-name ((,class (:foreground ,accent5))))
  `(gnus-header-subject ((,class (:foreground ,accent6 :bold t))))
  `(mu4e-view-url-number-face ((,class (:foreground ,accent5))))
  `(mu4e-cited-1-face ((,class (:foreground ,shade6))))
  `(mu4e-cited-7-face ((,class (:foreground ,shade5))))
  `(mu4e-header-marks-face ((,class (:foreground ,accent5))))
  `(ffap ((,class (:foreground ,shade4))))
  `(js2-private-function-call ((,class (:foreground ,accent4))))
  `(js2-jsdoc-html-tag-delimiter ((,class (:foreground ,accent3))))
  `(js2-jsdoc-html-tag-name ((,class (:foreground ,accent2))))
  `(js2-external-variable ((,class (:foreground ,accent5  ))))
  `(js2-function-param ((,class (:foreground ,accent4))))
  `(js2-jsdoc-value ((,class (:foreground ,accent3))))
  `(js2-private-member ((,class (:foreground ,shade5))))
  `(js3-warning-face ((,class (:underline ,accent1))))
  `(js3-error-face ((,class (:underline ,accent0))))
  `(js3-external-variable-face ((,class (:foreground ,accent6))))
  `(js3-function-param-face ((,class (:foreground ,accent7))))
  `(js3-jsdoc-tag-face ((,class (:foreground ,accent1))))
  `(js3-instance-member-face ((,class (:foreground ,accent4))))
  `(warning ((,class (:foreground ,accent0))))
  `(ac-completion-face ((,class (:underline t :foreground ,accent1))))
  `(info-quoted-name ((,class (:foreground ,accent5))))
  `(info-string ((,class (:foreground ,accent3))))
  `(icompletep-determined ((,class :foreground ,accent5)))
  `(undo-tree-visualizer-current-face ((,class :foreground ,accent5)))
  `(undo-tree-visualizer-default-face ((,class :foreground ,shade6)))
  `(undo-tree-visualizer-unmodified-face ((,class :foreground ,accent6)))
  `(undo-tree-visualizer-register-face ((,class :foreground ,accent5)))
  `(slime-repl-inputed-output-face ((,class (:foreground ,accent5))))
  `(trailing-whitespace ((,class :foreground nil :background ,accent0)))
  `(rainbow-delimiters-depth-1-face ((,class :foreground ,shade7)))
  `(rainbow-delimiters-depth-2-face ((,class :foreground ,accent5)))
  `(rainbow-delimiters-depth-3-face ((,class :foreground ,accent6)))
  `(rainbow-delimiters-depth-4-face ((,class :foreground ,accent4)))
  `(rainbow-delimiters-depth-5-face ((,class :foreground ,accent1)))
  `(rainbow-delimiters-depth-6-face ((,class :foreground ,shade5)))
  `(rainbow-delimiters-depth-7-face ((,class :foreground ,accent5)))
  `(rainbow-delimiters-depth-8-face ((,class :foreground ,accent3)))
  `(magit-item-highlight ((,class :background ,shade2)))
  `(magit-section-heading        ((,class (:foreground ,accent1 :weight bold))))
  `(magit-hunk-heading           ((,class (:background ,shade2))))
  `(magit-section-highlight      ((,class (:background ,shade1))))
  `(magit-hunk-heading-highlight ((,class (:background ,shade2))))
  `(magit-diff-context-highlight ((,class (:background ,shade2 :foreground ,shade5))))
  `(magit-diffstat-added   ((,class (:foreground ,accent5))))
  `(magit-diffstat-removed ((,class (:foreground ,accent6))))
  `(magit-process-ok ((,class (:foreground ,accent6 :weight bold))))
  `(magit-process-ng ((,class (:foreground ,accent0 :weight bold))))
  `(magit-branch ((,class (:foreground ,accent4 :weight bold))))
  `(magit-log-author ((,class (:foreground ,shade5))))
  `(magit-hash ((,class (:foreground ,shade6))))
  `(magit-diff-file-header ((,class (:foreground ,shade6 :background ,shade2))))
  `(lazy-highlight ((,class (:foreground ,shade6 :background ,shade2))))
  `(term ((,class (:foreground ,shade6 :background ,shade0))))
  `(term-color-black ((,class (:foreground ,shade6 :background ,shade0))))
  `(term-color-blue ((,class (:foreground ,accent5 :background ,shade0))))
  `(term-color-red ((,class (:foreground ,accent0 :background ,shade0))))
  `(term-color-green ((,class (:foreground ,accent3 :background ,shade0))))
  `(term-color-yellow ((,class (:foreground ,accent2 :background ,shade0))))
  `(term-color-magenta ((,class (:foreground ,accent7 :background ,shade0))))
  `(term-color-cyan ((,class (:foreground ,accent4 :background ,shade0))))
  `(term-color-white ((,class (:foreground ,shade2 :background ,shade0))))
  `(rainbow-delimiters-unmatched-face ((,class :foreground ,accent0)))
  `(helm-header ((,class (:foreground ,shade6 :background ,shade0 :underline nil :box nil))))
  `(helm-source-header ((,class (:foreground ,accent1 :background ,shade0 :underline nil :weight bold))))
  `(helm-selection ((,class (:background ,shade1 :underline nil))))
  `(helm-selection-line ((,class (:background ,shade1))))
  `(helm-visible-mark ((,class (:foreground ,shade0 :background ,shade2))))
  `(helm-candidate-number ((,class (:foreground ,shade0 :background ,shade7))))
  `(helm-separator ((,class (:foreground ,accent5 :background ,shade0))))
  `(helm-time-zone-current ((,class (:foreground ,accent5 :background ,shade0))))
  `(helm-time-zone-home ((,class (:foreground ,accent5 :background ,shade0))))
  `(helm-buffer-not-saved ((,class (:foreground ,accent5 :background ,shade0))))
  `(helm-buffer-process ((,class (:foreground ,accent5 :background ,shade0))))
  `(helm-buffer-saved-out ((,class (:foreground ,shade7 :background ,shade0))))
  `(helm-buffer-size ((,class (:foreground ,shade7 :background ,shade0))))
  `(helm-ff-directory ((,class (:foreground ,accent6 :background ,shade0 :weight bold))))
  `(helm-ff-file ((,class (:foreground ,shade7 :background ,shade0 :weight normal))))
  `(helm-ff-executable ((,class (:foreground ,accent2 :background ,shade0 :weight normal))))
  `(helm-ff-invalid-symlink ((,class (:foreground ,accent7 :background ,shade0 :weight bold))))
  `(helm-ff-symlink ((,class (:foreground ,accent1 :background ,shade0 :weight bold))))
  `(helm-ff-prefix ((,class (:foreground ,shade0 :background ,accent1 :weight normal))))
  `(helm-grep-cmd-line ((,class (:foreground ,shade7 :background ,shade0))))
  `(helm-grep-file ((,class (:foreground ,shade7 :background ,shade0))))
  `(helm-grep-finish ((,class (:foreground ,shade6 :background ,shade0))))
  `(helm-grep-lineno ((,class (:foreground ,shade7 :background ,shade0))))
  `(helm-grep-match ((,class (:foreground nil :background nil :inherit helm-match))))
  `(helm-grep-running ((,class (:foreground ,accent6 :background ,shade0))))
  `(helm-moccur-buffer ((,class (:foreground ,accent6 :background ,shade0))))
  `(helm-source-go-package-godoc-description ((,class (:foreground ,accent3))))
  `(helm-bookmark-w3m ((,class (:foreground ,accent5))))
  `(company-echo-common ((,class (:foreground ,shade0 :background ,shade7))))
  `(company-preview ((,class (:background ,shade0 :foreground ,accent2))))
  `(company-preview-common ((,class (:foreground ,shade1 :foreground ,shade5))))
  `(company-preview-search ((,class (:foreground ,accent5 :background ,shade0))))
  `(company-scrollbar-bg ((,class (:background ,shade2))))
  `(company-scrollbar-fg ((,class (:foreground ,accent1))))
  `(company-tooltip ((,class (:foreground ,shade6 :background ,shade0 :bold t))))
  `(company-tooltop-annotation ((,class (:foreground ,accent4))))
  `(company-tooltip-common ((,class ( :foreground ,shade5))))
  `(company-tooltip-common-selection ((,class (:foreground ,accent3))))
  `(company-tooltip-mouse ((,class (:inherit highlight))))
  `(company-tooltip-selection ((,class (:background ,shade2 :foreground ,shade5))))
  `(company-template-field ((,class (:inherit region))))
  `(web-mode-builtin-face ((,class (:inherit ,font-lock-builtin-face))))
  `(web-mode-comment-face ((,class (:inherit ,font-lock-comment-face))))
  `(web-mode-constant-face ((,class (:inherit ,font-lock-constant-face))))
  `(web-mode-keyword-face ((,class (:foreground ,accent1))))
  `(web-mode-doctype-face ((,class (:inherit ,font-lock-comment-face))))
  `(web-mode-function-name-face ((,class (:inherit ,font-lock-function-name-face))))
  `(web-mode-string-face ((,class (:foreground ,accent3))))
  `(web-mode-type-face ((,class (:inherit ,font-lock-type-face))))
  `(web-mode-html-attr-name-face ((,class (:foreground ,accent6))))
  `(web-mode-html-attr-value-face ((,class (:foreground ,accent1))))
  `(web-mode-warning-face ((,class (:inherit ,font-lock-warning-face))))
  `(web-mode-html-tag-face ((,class (:foreground ,accent5))))
  `(jde-java-font-lock-package-face ((t (:foreground ,accent6))))
  `(jde-java-font-lock-public-face ((t (:foreground ,accent1))))
  `(jde-java-font-lock-private-face ((t (:foreground ,accent1))))
  `(jde-java-font-lock-constant-face ((t (:foreground ,accent4))))
  `(jde-java-font-lock-modifier-face ((t (:foreground ,accent7))))
  `(jde-jave-font-lock-protected-face ((t (:foreground ,accent1))))
  `(jde-java-font-lock-number-face ((t (:foreground ,accent6))))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
    (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'herschel-flamingo-pink-light)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; herschel-flamingo-pink-light-theme.el ends here
```

## exo-ui-red-dark-theme.el

[Get the theme
here!](https://themer.dev/?colors.dark.shade0=%23222222&colors.dark.shade7=%23ff5156&colors.light.shade0=%23ff5156&colors.light.shade7=%23222222&activeColorSet=dark)

``` commonlisp
;;; exo-ui-red-dark-theme.el

;; Version: 1.0.1
;; Package-Requires: ((emacs "24"))

;;; Commentary:

;; Generated by the emacs theme template for themer.

;;; Code:

(deftheme exo-ui-red-dark)
(let ((class '((class color) (min-colors 89)))
  (shade0 "#222222")
  (shade1 "#422929")
  (shade2 "#612f31")
  (shade3 "#813638")
  (shade4 "#a03d40")
  (shade5 "#c04447")
  (shade6 "#df4a4f")
  (shade7 "#ff5156")
  (accent0 "#ff5156")
  (accent1 "#ff5156")
  (accent2 "#ff5156")
  (accent3 "#ff5156")
  (accent4 "#ff5156")
  (accent5 "#ff5156")
  (accent6 "#ff5156")
  (accent7 "#ff5156"))
(custom-theme-set-faces
  'exo-ui-red-dark
  `(default ((,class (:background ,shade0 :foreground ,shade7))))
  `(font-lock-builtin-face ((,class (:foreground ,accent5))))
  `(font-lock-comment-face ((,class (:foreground ,shade3))))
  `(font-lock-negation-char-face ((,class (:foreground ,accent4))))
  `(font-lock-reference-face ((,class (:foreground ,accent4))))
  `(font-lock-constant-face ((,class (:foreground ,accent4))))
  `(font-lock-doc-face ((,class (:foreground ,shade3))))
  `(font-lock-function-name-face ((,class (:foreground ,accent6 :bold t))))
  `(font-lock-keyword-face ((,class (:bold ,class :foreground ,accent1))))
  `(font-lock-string-face ((,class (:foreground ,accent3))))
  `(font-lock-type-face ((,class (:foreground ,accent5 ))))
  `(font-lock-variable-name-face ((,class (:foreground ,accent6))))
  `(font-lock-warning-face ((,class (:foreground ,accent0 :background ,shade1))))
  `(region ((,class (:background ,shade7 :foreground ,shade0))))
  `(highlight ((,class (:foreground ,shade5 :background ,shade2))))
  `(hl-line ((,class (:background  ,shade1))))
  `(fringe ((,class (:background ,shade1 :foreground ,shade4))))
  `(cursor ((,class (:background ,shade2))))
  `(show-paren-match-face ((,class (:background ,accent0))))
  `(isearch ((,class (:bold t :foreground ,accent0 :background ,shade2))))
  `(mode-line ((,class (:box (:line-width 1 :color nil) :bold t :foreground ,shade4 :background ,shade0))))
  `(mode-line-inactive ((,class (:box (:line-width 1 :color nil :style pressed-button) :foreground ,accent7 :background ,shade1 :weight normal))))
  `(mode-line-buffer-id ((,class (:bold t :foreground ,accent6 :background nil))))
  `(mode-line-highlight ((,class (:foreground ,accent1 :box nil :weight bold))))
  `(mode-line-emphasis ((,class (:foreground ,shade7))))
  `(vertical-border ((,class (:foreground ,shade5))))
  `(minibuffer-prompt ((,class (:bold t :foreground ,accent1))))
  `(default-italic ((,class (:italic t))))
  `(link ((,class (:foreground ,accent4 :underline t))))
  `(org-code ((,class (:foreground ,shade6))))
  `(org-hide ((,class (:foreground ,shade4))))
  `(org-level-1 ((,class (:bold t :foreground ,accent4 :height 1.1))))
  `(org-level-2 ((,class (:bold nil :foreground ,accent5))))
  `(org-level-3 ((,class (:bold t :foreground ,accent6))))
  `(org-level-4 ((,class (:bold nil :foreground ,accent7))))
  `(org-date ((,class (:underline t :foreground ,accent2) )))
  `(org-footnote  ((,class (:underline t :foreground ,shade4))))
  `(org-link ((,class (:underline t :foreground ,accent5 ))))
  `(org-special-keyword ((,class (:foreground ,accent1))))
  `(org-block ((,class (:foreground ,shade5))))
  `(org-quote ((,class (:inherit org-block :slant italic))))
  `(org-verse ((,class (:inherit org-block :slant italic))))
  `(org-todo ((,class (:box (:line-width 1 :color ,shade1) :foreground ,accent1 :bold t))))
  `(org-done ((,class (:box (:line-width 1 :color ,shade1) :foreground ,shade5 :bold t))))
  `(org-warning ((,class (:underline t :foreground ,accent2))))
  `(org-agenda-structure ((,class (:weight bold :foreground ,shade5 :box (:color ,shade4) :background ,shade2))))
  `(org-agenda-date ((,class (:foreground ,accent6 :height 1.1 ))))
  `(org-agenda-date-weekend ((,class (:weight normal :foreground ,shade4))))
  `(org-agenda-date-today ((,class (:weight bold :foreground ,accent1 :height 1.4))))
  `(org-agenda-done ((,class (:foreground ,shade3))))
  `(org-scheduled ((,class (:foreground ,accent5))))
  `(org-scheduled-today ((,class (:foreground ,accent6 :weight bold :height 1.2))))
  `(org-ellipsis ((,class (:foreground ,accent5))))
  `(org-verbatim ((,class (:foreground ,shade4))))
  `(org-document-info-keyword ((,class (:foreground ,accent6))))
  `(font-latex-bold-face ((,class (:foreground ,accent5))))
  `(font-latex-italic-face ((,class (:foreground ,accent7 :italic t))))
  `(font-latex-string-face ((,class (:foreground ,accent3))))
  `(font-latex-match-reference-keywords ((,class (:foreground ,accent4))))
  `(font-latex-match-variable-keywords ((,class (:foreground ,accent6))))
  `(ido-only-match ((,class (:foreground ,accent0))))
  `(org-sexp-date ((,class (:foreground ,shade4))))
  `(ido-first-match ((,class (:foreground ,accent1 :bold t))))
  `(gnus-header-content ((,class (:foreground ,accent1))))
  `(gnus-header-from ((,class (:foreground ,accent6))))
  `(gnus-header-name ((,class (:foreground ,accent5))))
  `(gnus-header-subject ((,class (:foreground ,accent6 :bold t))))
  `(mu4e-view-url-number-face ((,class (:foreground ,accent5))))
  `(mu4e-cited-1-face ((,class (:foreground ,shade6))))
  `(mu4e-cited-7-face ((,class (:foreground ,shade5))))
  `(mu4e-header-marks-face ((,class (:foreground ,accent5))))
  `(ffap ((,class (:foreground ,shade4))))
  `(js2-private-function-call ((,class (:foreground ,accent4))))
  `(js2-jsdoc-html-tag-delimiter ((,class (:foreground ,accent3))))
  `(js2-jsdoc-html-tag-name ((,class (:foreground ,accent2))))
  `(js2-external-variable ((,class (:foreground ,accent5  ))))
  `(js2-function-param ((,class (:foreground ,accent4))))
  `(js2-jsdoc-value ((,class (:foreground ,accent3))))
  `(js2-private-member ((,class (:foreground ,shade5))))
  `(js3-warning-face ((,class (:underline ,accent1))))
  `(js3-error-face ((,class (:underline ,accent0))))
  `(js3-external-variable-face ((,class (:foreground ,accent6))))
  `(js3-function-param-face ((,class (:foreground ,accent7))))
  `(js3-jsdoc-tag-face ((,class (:foreground ,accent1))))
  `(js3-instance-member-face ((,class (:foreground ,accent4))))
  `(warning ((,class (:foreground ,accent0))))
  `(ac-completion-face ((,class (:underline t :foreground ,accent1))))
  `(info-quoted-name ((,class (:foreground ,accent5))))
  `(info-string ((,class (:foreground ,accent3))))
  `(icompletep-determined ((,class :foreground ,accent5)))
  `(undo-tree-visualizer-current-face ((,class :foreground ,accent5)))
  `(undo-tree-visualizer-default-face ((,class :foreground ,shade6)))
  `(undo-tree-visualizer-unmodified-face ((,class :foreground ,accent6)))
  `(undo-tree-visualizer-register-face ((,class :foreground ,accent5)))
  `(slime-repl-inputed-output-face ((,class (:foreground ,accent5))))
  `(trailing-whitespace ((,class :foreground nil :background ,accent0)))
  `(rainbow-delimiters-depth-1-face ((,class :foreground ,shade7)))
  `(rainbow-delimiters-depth-2-face ((,class :foreground ,accent5)))
  `(rainbow-delimiters-depth-3-face ((,class :foreground ,accent6)))
  `(rainbow-delimiters-depth-4-face ((,class :foreground ,accent4)))
  `(rainbow-delimiters-depth-5-face ((,class :foreground ,accent1)))
  `(rainbow-delimiters-depth-6-face ((,class :foreground ,shade5)))
  `(rainbow-delimiters-depth-7-face ((,class :foreground ,accent5)))
  `(rainbow-delimiters-depth-8-face ((,class :foreground ,accent3)))
  `(magit-item-highlight ((,class :background ,shade2)))
  `(magit-section-heading        ((,class (:foreground ,accent1 :weight bold))))
  `(magit-hunk-heading           ((,class (:background ,shade2))))
  `(magit-section-highlight      ((,class (:background ,shade1))))
  `(magit-hunk-heading-highlight ((,class (:background ,shade2))))
  `(magit-diff-context-highlight ((,class (:background ,shade2 :foreground ,shade5))))
  `(magit-diffstat-added   ((,class (:foreground ,accent5))))
  `(magit-diffstat-removed ((,class (:foreground ,accent6))))
  `(magit-process-ok ((,class (:foreground ,accent6 :weight bold))))
  `(magit-process-ng ((,class (:foreground ,accent0 :weight bold))))
  `(magit-branch ((,class (:foreground ,accent4 :weight bold))))
  `(magit-log-author ((,class (:foreground ,shade5))))
  `(magit-hash ((,class (:foreground ,shade6))))
  `(magit-diff-file-header ((,class (:foreground ,shade6 :background ,shade2))))
  `(lazy-highlight ((,class (:foreground ,shade6 :background ,shade2))))
  `(term ((,class (:foreground ,shade6 :background ,shade0))))
  `(term-color-black ((,class (:foreground ,shade6 :background ,shade0))))
  `(term-color-blue ((,class (:foreground ,accent5 :background ,shade0))))
  `(term-color-red ((,class (:foreground ,accent0 :background ,shade0))))
  `(term-color-green ((,class (:foreground ,accent3 :background ,shade0))))
  `(term-color-yellow ((,class (:foreground ,accent2 :background ,shade0))))
  `(term-color-magenta ((,class (:foreground ,accent7 :background ,shade0))))
  `(term-color-cyan ((,class (:foreground ,accent4 :background ,shade0))))
  `(term-color-white ((,class (:foreground ,shade2 :background ,shade0))))
  `(rainbow-delimiters-unmatched-face ((,class :foreground ,accent0)))
  `(helm-header ((,class (:foreground ,shade6 :background ,shade0 :underline nil :box nil))))
  `(helm-source-header ((,class (:foreground ,accent1 :background ,shade0 :underline nil :weight bold))))
  `(helm-selection ((,class (:background ,shade1 :underline nil))))
  `(helm-selection-line ((,class (:background ,shade1))))
  `(helm-visible-mark ((,class (:foreground ,shade0 :background ,shade2))))
  `(helm-candidate-number ((,class (:foreground ,shade0 :background ,shade7))))
  `(helm-separator ((,class (:foreground ,accent5 :background ,shade0))))
  `(helm-time-zone-current ((,class (:foreground ,accent5 :background ,shade0))))
  `(helm-time-zone-home ((,class (:foreground ,accent5 :background ,shade0))))
  `(helm-buffer-not-saved ((,class (:foreground ,accent5 :background ,shade0))))
  `(helm-buffer-process ((,class (:foreground ,accent5 :background ,shade0))))
  `(helm-buffer-saved-out ((,class (:foreground ,shade7 :background ,shade0))))
  `(helm-buffer-size ((,class (:foreground ,shade7 :background ,shade0))))
  `(helm-ff-directory ((,class (:foreground ,accent6 :background ,shade0 :weight bold))))
  `(helm-ff-file ((,class (:foreground ,shade7 :background ,shade0 :weight normal))))
  `(helm-ff-executable ((,class (:foreground ,accent2 :background ,shade0 :weight normal))))
  `(helm-ff-invalid-symlink ((,class (:foreground ,accent7 :background ,shade0 :weight bold))))
  `(helm-ff-symlink ((,class (:foreground ,accent1 :background ,shade0 :weight bold))))
  `(helm-ff-prefix ((,class (:foreground ,shade0 :background ,accent1 :weight normal))))
  `(helm-grep-cmd-line ((,class (:foreground ,shade7 :background ,shade0))))
  `(helm-grep-file ((,class (:foreground ,shade7 :background ,shade0))))
  `(helm-grep-finish ((,class (:foreground ,shade6 :background ,shade0))))
  `(helm-grep-lineno ((,class (:foreground ,shade7 :background ,shade0))))
  `(helm-grep-match ((,class (:foreground nil :background nil :inherit helm-match))))
  `(helm-grep-running ((,class (:foreground ,accent6 :background ,shade0))))
  `(helm-moccur-buffer ((,class (:foreground ,accent6 :background ,shade0))))
  `(helm-source-go-package-godoc-description ((,class (:foreground ,accent3))))
  `(helm-bookmark-w3m ((,class (:foreground ,accent5))))
  `(company-echo-common ((,class (:foreground ,shade0 :background ,shade7))))
  `(company-preview ((,class (:background ,shade0 :foreground ,accent2))))
  `(company-preview-common ((,class (:foreground ,shade1 :foreground ,shade5))))
  `(company-preview-search ((,class (:foreground ,accent5 :background ,shade0))))
  `(company-scrollbar-bg ((,class (:background ,shade2))))
  `(company-scrollbar-fg ((,class (:foreground ,accent1))))
  `(company-tooltip ((,class (:foreground ,shade6 :background ,shade0 :bold t))))
  `(company-tooltop-annotation ((,class (:foreground ,accent4))))
  `(company-tooltip-common ((,class ( :foreground ,shade5))))
  `(company-tooltip-common-selection ((,class (:foreground ,accent3))))
  `(company-tooltip-mouse ((,class (:inherit highlight))))
  `(company-tooltip-selection ((,class (:background ,shade2 :foreground ,shade5))))
  `(company-template-field ((,class (:inherit region))))
  `(web-mode-builtin-face ((,class (:inherit ,font-lock-builtin-face))))
  `(web-mode-comment-face ((,class (:inherit ,font-lock-comment-face))))
  `(web-mode-constant-face ((,class (:inherit ,font-lock-constant-face))))
  `(web-mode-keyword-face ((,class (:foreground ,accent1))))
  `(web-mode-doctype-face ((,class (:inherit ,font-lock-comment-face))))
  `(web-mode-function-name-face ((,class (:inherit ,font-lock-function-name-face))))
  `(web-mode-string-face ((,class (:foreground ,accent3))))
  `(web-mode-type-face ((,class (:inherit ,font-lock-type-face))))
  `(web-mode-html-attr-name-face ((,class (:foreground ,accent6))))
  `(web-mode-html-attr-value-face ((,class (:foreground ,accent1))))
  `(web-mode-warning-face ((,class (:inherit ,font-lock-warning-face))))
  `(web-mode-html-tag-face ((,class (:foreground ,accent5))))
  `(jde-java-font-lock-package-face ((t (:foreground ,accent6))))
  `(jde-java-font-lock-public-face ((t (:foreground ,accent1))))
  `(jde-java-font-lock-private-face ((t (:foreground ,accent1))))
  `(jde-java-font-lock-constant-face ((t (:foreground ,accent4))))
  `(jde-java-font-lock-modifier-face ((t (:foreground ,accent7))))
  `(jde-jave-font-lock-protected-face ((t (:foreground ,accent1))))
  `(jde-java-font-lock-number-face ((t (:foreground ,accent6))))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
    (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'exo-ui-red-dark)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; exo-ui-red-dark-theme.el ends here
```

## exo-ui-red-light-theme.el

[Get the theme
here!](https://themer.dev/?colors.dark.shade0=%23222222&colors.dark.shade7=%23ff5156&colors.light.shade0=%23ff5156&colors.light.shade7=%23222222&activeColorSet=dark)

``` commonlisp
;;; exo-ui-red-light-theme.el

;; Version: 1.0.1
;; Package-Requires: ((emacs "24"))

;;; Commentary:

;; Generated by the emacs theme template for themer.

;;; Code:

(deftheme exo-ui-red-light)
(let ((class '((class color) (min-colors 89)))
  (shade0 "#ff5156")
  (shade1 "#df4a4f")
  (shade2 "#c04447")
  (shade3 "#a03d40")
  (shade4 "#813638")
  (shade5 "#612f31")
  (shade6 "#422929")
  (shade7 "#222222")
  (accent0 "#222222")
  (accent1 "#222222")
  (accent2 "#222222")
  (accent3 "#222222")
  (accent4 "#222222")
  (accent5 "#222222")
  (accent6 "#222222")
  (accent7 "#222222"))
(custom-theme-set-faces
  'exo-ui-red-light
  `(default ((,class (:background ,shade0 :foreground ,shade7))))
  `(font-lock-builtin-face ((,class (:foreground ,accent5))))
  `(font-lock-comment-face ((,class (:foreground ,shade3))))
  `(font-lock-negation-char-face ((,class (:foreground ,accent4))))
  `(font-lock-reference-face ((,class (:foreground ,accent4))))
  `(font-lock-constant-face ((,class (:foreground ,accent4))))
  `(font-lock-doc-face ((,class (:foreground ,shade3))))
  `(font-lock-function-name-face ((,class (:foreground ,accent6 :bold t))))
  `(font-lock-keyword-face ((,class (:bold ,class :foreground ,accent1))))
  `(font-lock-string-face ((,class (:foreground ,accent3))))
  `(font-lock-type-face ((,class (:foreground ,accent5 ))))
  `(font-lock-variable-name-face ((,class (:foreground ,accent6))))
  `(font-lock-warning-face ((,class (:foreground ,accent0 :background ,shade1))))
  `(region ((,class (:background ,shade7 :foreground ,shade0))))
  `(highlight ((,class (:foreground ,shade5 :background ,shade2))))
  `(hl-line ((,class (:background  ,shade1))))
  `(fringe ((,class (:background ,shade1 :foreground ,shade4))))
  `(cursor ((,class (:background ,shade2))))
  `(show-paren-match-face ((,class (:background ,accent0))))
  `(isearch ((,class (:bold t :foreground ,accent0 :background ,shade2))))
  `(mode-line ((,class (:box (:line-width 1 :color nil) :bold t :foreground ,shade4 :background ,shade0))))
  `(mode-line-inactive ((,class (:box (:line-width 1 :color nil :style pressed-button) :foreground ,accent7 :background ,shade1 :weight normal))))
  `(mode-line-buffer-id ((,class (:bold t :foreground ,accent6 :background nil))))
  `(mode-line-highlight ((,class (:foreground ,accent1 :box nil :weight bold))))
  `(mode-line-emphasis ((,class (:foreground ,shade7))))
  `(vertical-border ((,class (:foreground ,shade5))))
  `(minibuffer-prompt ((,class (:bold t :foreground ,accent1))))
  `(default-italic ((,class (:italic t))))
  `(link ((,class (:foreground ,accent4 :underline t))))
  `(org-code ((,class (:foreground ,shade6))))
  `(org-hide ((,class (:foreground ,shade4))))
  `(org-level-1 ((,class (:bold t :foreground ,accent4 :height 1.1))))
  `(org-level-2 ((,class (:bold nil :foreground ,accent5))))
  `(org-level-3 ((,class (:bold t :foreground ,accent6))))
  `(org-level-4 ((,class (:bold nil :foreground ,accent7))))
  `(org-date ((,class (:underline t :foreground ,accent2) )))
  `(org-footnote  ((,class (:underline t :foreground ,shade4))))
  `(org-link ((,class (:underline t :foreground ,accent5 ))))
  `(org-special-keyword ((,class (:foreground ,accent1))))
  `(org-block ((,class (:foreground ,shade5))))
  `(org-quote ((,class (:inherit org-block :slant italic))))
  `(org-verse ((,class (:inherit org-block :slant italic))))
  `(org-todo ((,class (:box (:line-width 1 :color ,shade1) :foreground ,accent1 :bold t))))
  `(org-done ((,class (:box (:line-width 1 :color ,shade1) :foreground ,shade5 :bold t))))
  `(org-warning ((,class (:underline t :foreground ,accent2))))
  `(org-agenda-structure ((,class (:weight bold :foreground ,shade5 :box (:color ,shade4) :background ,shade2))))
  `(org-agenda-date ((,class (:foreground ,accent6 :height 1.1 ))))
  `(org-agenda-date-weekend ((,class (:weight normal :foreground ,shade4))))
  `(org-agenda-date-today ((,class (:weight bold :foreground ,accent1 :height 1.4))))
  `(org-agenda-done ((,class (:foreground ,shade3))))
  `(org-scheduled ((,class (:foreground ,accent5))))
  `(org-scheduled-today ((,class (:foreground ,accent6 :weight bold :height 1.2))))
  `(org-ellipsis ((,class (:foreground ,accent5))))
  `(org-verbatim ((,class (:foreground ,shade4))))
  `(org-document-info-keyword ((,class (:foreground ,accent6))))
  `(font-latex-bold-face ((,class (:foreground ,accent5))))
  `(font-latex-italic-face ((,class (:foreground ,accent7 :italic t))))
  `(font-latex-string-face ((,class (:foreground ,accent3))))
  `(font-latex-match-reference-keywords ((,class (:foreground ,accent4))))
  `(font-latex-match-variable-keywords ((,class (:foreground ,accent6))))
  `(ido-only-match ((,class (:foreground ,accent0))))
  `(org-sexp-date ((,class (:foreground ,shade4))))
  `(ido-first-match ((,class (:foreground ,accent1 :bold t))))
  `(gnus-header-content ((,class (:foreground ,accent1))))
  `(gnus-header-from ((,class (:foreground ,accent6))))
  `(gnus-header-name ((,class (:foreground ,accent5))))
  `(gnus-header-subject ((,class (:foreground ,accent6 :bold t))))
  `(mu4e-view-url-number-face ((,class (:foreground ,accent5))))
  `(mu4e-cited-1-face ((,class (:foreground ,shade6))))
  `(mu4e-cited-7-face ((,class (:foreground ,shade5))))
  `(mu4e-header-marks-face ((,class (:foreground ,accent5))))
  `(ffap ((,class (:foreground ,shade4))))
  `(js2-private-function-call ((,class (:foreground ,accent4))))
  `(js2-jsdoc-html-tag-delimiter ((,class (:foreground ,accent3))))
  `(js2-jsdoc-html-tag-name ((,class (:foreground ,accent2))))
  `(js2-external-variable ((,class (:foreground ,accent5  ))))
  `(js2-function-param ((,class (:foreground ,accent4))))
  `(js2-jsdoc-value ((,class (:foreground ,accent3))))
  `(js2-private-member ((,class (:foreground ,shade5))))
  `(js3-warning-face ((,class (:underline ,accent1))))
  `(js3-error-face ((,class (:underline ,accent0))))
  `(js3-external-variable-face ((,class (:foreground ,accent6))))
  `(js3-function-param-face ((,class (:foreground ,accent7))))
  `(js3-jsdoc-tag-face ((,class (:foreground ,accent1))))
  `(js3-instance-member-face ((,class (:foreground ,accent4))))
  `(warning ((,class (:foreground ,accent0))))
  `(ac-completion-face ((,class (:underline t :foreground ,accent1))))
  `(info-quoted-name ((,class (:foreground ,accent5))))
  `(info-string ((,class (:foreground ,accent3))))
  `(icompletep-determined ((,class :foreground ,accent5)))
  `(undo-tree-visualizer-current-face ((,class :foreground ,accent5)))
  `(undo-tree-visualizer-default-face ((,class :foreground ,shade6)))
  `(undo-tree-visualizer-unmodified-face ((,class :foreground ,accent6)))
  `(undo-tree-visualizer-register-face ((,class :foreground ,accent5)))
  `(slime-repl-inputed-output-face ((,class (:foreground ,accent5))))
  `(trailing-whitespace ((,class :foreground nil :background ,accent0)))
  `(rainbow-delimiters-depth-1-face ((,class :foreground ,shade7)))
  `(rainbow-delimiters-depth-2-face ((,class :foreground ,accent5)))
  `(rainbow-delimiters-depth-3-face ((,class :foreground ,accent6)))
  `(rainbow-delimiters-depth-4-face ((,class :foreground ,accent4)))
  `(rainbow-delimiters-depth-5-face ((,class :foreground ,accent1)))
  `(rainbow-delimiters-depth-6-face ((,class :foreground ,shade5)))
  `(rainbow-delimiters-depth-7-face ((,class :foreground ,accent5)))
  `(rainbow-delimiters-depth-8-face ((,class :foreground ,accent3)))
  `(magit-item-highlight ((,class :background ,shade2)))
  `(magit-section-heading        ((,class (:foreground ,accent1 :weight bold))))
  `(magit-hunk-heading           ((,class (:background ,shade2))))
  `(magit-section-highlight      ((,class (:background ,shade1))))
  `(magit-hunk-heading-highlight ((,class (:background ,shade2))))
  `(magit-diff-context-highlight ((,class (:background ,shade2 :foreground ,shade5))))
  `(magit-diffstat-added   ((,class (:foreground ,accent5))))
  `(magit-diffstat-removed ((,class (:foreground ,accent6))))
  `(magit-process-ok ((,class (:foreground ,accent6 :weight bold))))
  `(magit-process-ng ((,class (:foreground ,accent0 :weight bold))))
  `(magit-branch ((,class (:foreground ,accent4 :weight bold))))
  `(magit-log-author ((,class (:foreground ,shade5))))
  `(magit-hash ((,class (:foreground ,shade6))))
  `(magit-diff-file-header ((,class (:foreground ,shade6 :background ,shade2))))
  `(lazy-highlight ((,class (:foreground ,shade6 :background ,shade2))))
  `(term ((,class (:foreground ,shade6 :background ,shade0))))
  `(term-color-black ((,class (:foreground ,shade6 :background ,shade0))))
  `(term-color-blue ((,class (:foreground ,accent5 :background ,shade0))))
  `(term-color-red ((,class (:foreground ,accent0 :background ,shade0))))
  `(term-color-green ((,class (:foreground ,accent3 :background ,shade0))))
  `(term-color-yellow ((,class (:foreground ,accent2 :background ,shade0))))
  `(term-color-magenta ((,class (:foreground ,accent7 :background ,shade0))))
  `(term-color-cyan ((,class (:foreground ,accent4 :background ,shade0))))
  `(term-color-white ((,class (:foreground ,shade2 :background ,shade0))))
  `(rainbow-delimiters-unmatched-face ((,class :foreground ,accent0)))
  `(helm-header ((,class (:foreground ,shade6 :background ,shade0 :underline nil :box nil))))
  `(helm-source-header ((,class (:foreground ,accent1 :background ,shade0 :underline nil :weight bold))))
  `(helm-selection ((,class (:background ,shade1 :underline nil))))
  `(helm-selection-line ((,class (:background ,shade1))))
  `(helm-visible-mark ((,class (:foreground ,shade0 :background ,shade2))))
  `(helm-candidate-number ((,class (:foreground ,shade0 :background ,shade7))))
  `(helm-separator ((,class (:foreground ,accent5 :background ,shade0))))
  `(helm-time-zone-current ((,class (:foreground ,accent5 :background ,shade0))))
  `(helm-time-zone-home ((,class (:foreground ,accent5 :background ,shade0))))
  `(helm-buffer-not-saved ((,class (:foreground ,accent5 :background ,shade0))))
  `(helm-buffer-process ((,class (:foreground ,accent5 :background ,shade0))))
  `(helm-buffer-saved-out ((,class (:foreground ,shade7 :background ,shade0))))
  `(helm-buffer-size ((,class (:foreground ,shade7 :background ,shade0))))
  `(helm-ff-directory ((,class (:foreground ,accent6 :background ,shade0 :weight bold))))
  `(helm-ff-file ((,class (:foreground ,shade7 :background ,shade0 :weight normal))))
  `(helm-ff-executable ((,class (:foreground ,accent2 :background ,shade0 :weight normal))))
  `(helm-ff-invalid-symlink ((,class (:foreground ,accent7 :background ,shade0 :weight bold))))
  `(helm-ff-symlink ((,class (:foreground ,accent1 :background ,shade0 :weight bold))))
  `(helm-ff-prefix ((,class (:foreground ,shade0 :background ,accent1 :weight normal))))
  `(helm-grep-cmd-line ((,class (:foreground ,shade7 :background ,shade0))))
  `(helm-grep-file ((,class (:foreground ,shade7 :background ,shade0))))
  `(helm-grep-finish ((,class (:foreground ,shade6 :background ,shade0))))
  `(helm-grep-lineno ((,class (:foreground ,shade7 :background ,shade0))))
  `(helm-grep-match ((,class (:foreground nil :background nil :inherit helm-match))))
  `(helm-grep-running ((,class (:foreground ,accent6 :background ,shade0))))
  `(helm-moccur-buffer ((,class (:foreground ,accent6 :background ,shade0))))
  `(helm-source-go-package-godoc-description ((,class (:foreground ,accent3))))
  `(helm-bookmark-w3m ((,class (:foreground ,accent5))))
  `(company-echo-common ((,class (:foreground ,shade0 :background ,shade7))))
  `(company-preview ((,class (:background ,shade0 :foreground ,accent2))))
  `(company-preview-common ((,class (:foreground ,shade1 :foreground ,shade5))))
  `(company-preview-search ((,class (:foreground ,accent5 :background ,shade0))))
  `(company-scrollbar-bg ((,class (:background ,shade2))))
  `(company-scrollbar-fg ((,class (:foreground ,accent1))))
  `(company-tooltip ((,class (:foreground ,shade6 :background ,shade0 :bold t))))
  `(company-tooltop-annotation ((,class (:foreground ,accent4))))
  `(company-tooltip-common ((,class ( :foreground ,shade5))))
  `(company-tooltip-common-selection ((,class (:foreground ,accent3))))
  `(company-tooltip-mouse ((,class (:inherit highlight))))
  `(company-tooltip-selection ((,class (:background ,shade2 :foreground ,shade5))))
  `(company-template-field ((,class (:inherit region))))
  `(web-mode-builtin-face ((,class (:inherit ,font-lock-builtin-face))))
  `(web-mode-comment-face ((,class (:inherit ,font-lock-comment-face))))
  `(web-mode-constant-face ((,class (:inherit ,font-lock-constant-face))))
  `(web-mode-keyword-face ((,class (:foreground ,accent1))))
  `(web-mode-doctype-face ((,class (:inherit ,font-lock-comment-face))))
  `(web-mode-function-name-face ((,class (:inherit ,font-lock-function-name-face))))
  `(web-mode-string-face ((,class (:foreground ,accent3))))
  `(web-mode-type-face ((,class (:inherit ,font-lock-type-face))))
  `(web-mode-html-attr-name-face ((,class (:foreground ,accent6))))
  `(web-mode-html-attr-value-face ((,class (:foreground ,accent1))))
  `(web-mode-warning-face ((,class (:inherit ,font-lock-warning-face))))
  `(web-mode-html-tag-face ((,class (:foreground ,accent5))))
  `(jde-java-font-lock-package-face ((t (:foreground ,accent6))))
  `(jde-java-font-lock-public-face ((t (:foreground ,accent1))))
  `(jde-java-font-lock-private-face ((t (:foreground ,accent1))))
  `(jde-java-font-lock-constant-face ((t (:foreground ,accent4))))
  `(jde-java-font-lock-modifier-face ((t (:foreground ,accent7))))
  `(jde-jave-font-lock-protected-face ((t (:foreground ,accent1))))
  `(jde-java-font-lock-number-face ((t (:foreground ,accent6))))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
    (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'exo-ui-red-light)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; exo-ui-red-light-theme.el ends here
```

## st-giles-blue-dark-theme.el

[Get the theme
here!](https://themer.dev/?colors.dark.shade0=%23222222&colors.dark.shade7=%23599ec4&colors.light.shade0=%23599ec4&colors.light.shade7=%23222222&activeColorSet=dark)

``` commonlisp
;;; st-giles-blue-dark-theme.el

;; Version: 1.0.1
;; Package-Requires: ((emacs "24"))

;;; Commentary:

;; Generated by the emacs theme template for themer.

;;; Code:

(deftheme st-giles-blue-dark)
(let ((class '((class color) (min-colors 89)))
  (shade0 "#222222")
  (shade1 "#2a3439")
  (shade2 "#324550")
  (shade3 "#3a5767")
  (shade4 "#41697f")
  (shade5 "#497b96")
  (shade6 "#518cad")
  (shade7 "#599ec4")
  (accent0 "#599ec4")
  (accent1 "#599ec4")
  (accent2 "#599ec4")
  (accent3 "#599ec4")
  (accent4 "#599ec4")
  (accent5 "#599ec4")
  (accent6 "#599ec4")
  (accent7 "#599ec4"))
(custom-theme-set-faces
  'st-giles-blue-dark
  `(default ((,class (:background ,shade0 :foreground ,shade7))))
  `(font-lock-builtin-face ((,class (:foreground ,accent5))))
  `(font-lock-comment-face ((,class (:foreground ,shade3))))
  `(font-lock-negation-char-face ((,class (:foreground ,accent4))))
  `(font-lock-reference-face ((,class (:foreground ,accent4))))
  `(font-lock-constant-face ((,class (:foreground ,accent4))))
  `(font-lock-doc-face ((,class (:foreground ,shade3))))
  `(font-lock-function-name-face ((,class (:foreground ,accent6 :bold t))))
  `(font-lock-keyword-face ((,class (:bold ,class :foreground ,accent1))))
  `(font-lock-string-face ((,class (:foreground ,accent3))))
  `(font-lock-type-face ((,class (:foreground ,accent5 ))))
  `(font-lock-variable-name-face ((,class (:foreground ,accent6))))
  `(font-lock-warning-face ((,class (:foreground ,accent0 :background ,shade1))))
  `(region ((,class (:background ,shade7 :foreground ,shade0))))
  `(highlight ((,class (:foreground ,shade5 :background ,shade2))))
  `(hl-line ((,class (:background  ,shade1))))
  `(fringe ((,class (:background ,shade1 :foreground ,shade4))))
  `(cursor ((,class (:background ,shade2))))
  `(show-paren-match-face ((,class (:background ,accent0))))
  `(isearch ((,class (:bold t :foreground ,accent0 :background ,shade2))))
  `(mode-line ((,class (:box (:line-width 1 :color nil) :bold t :foreground ,shade4 :background ,shade0))))
  `(mode-line-inactive ((,class (:box (:line-width 1 :color nil :style pressed-button) :foreground ,accent7 :background ,shade1 :weight normal))))
  `(mode-line-buffer-id ((,class (:bold t :foreground ,accent6 :background nil))))
  `(mode-line-highlight ((,class (:foreground ,accent1 :box nil :weight bold))))
  `(mode-line-emphasis ((,class (:foreground ,shade7))))
  `(vertical-border ((,class (:foreground ,shade5))))
  `(minibuffer-prompt ((,class (:bold t :foreground ,accent1))))
  `(default-italic ((,class (:italic t))))
  `(link ((,class (:foreground ,accent4 :underline t))))
  `(org-code ((,class (:foreground ,shade6))))
  `(org-hide ((,class (:foreground ,shade4))))
  `(org-level-1 ((,class (:bold t :foreground ,accent4 :height 1.1))))
  `(org-level-2 ((,class (:bold nil :foreground ,accent5))))
  `(org-level-3 ((,class (:bold t :foreground ,accent6))))
  `(org-level-4 ((,class (:bold nil :foreground ,accent7))))
  `(org-date ((,class (:underline t :foreground ,accent2) )))
  `(org-footnote  ((,class (:underline t :foreground ,shade4))))
  `(org-link ((,class (:underline t :foreground ,accent5 ))))
  `(org-special-keyword ((,class (:foreground ,accent1))))
  `(org-block ((,class (:foreground ,shade5))))
  `(org-quote ((,class (:inherit org-block :slant italic))))
  `(org-verse ((,class (:inherit org-block :slant italic))))
  `(org-todo ((,class (:box (:line-width 1 :color ,shade1) :foreground ,accent1 :bold t))))
  `(org-done ((,class (:box (:line-width 1 :color ,shade1) :foreground ,shade5 :bold t))))
  `(org-warning ((,class (:underline t :foreground ,accent2))))
  `(org-agenda-structure ((,class (:weight bold :foreground ,shade5 :box (:color ,shade4) :background ,shade2))))
  `(org-agenda-date ((,class (:foreground ,accent6 :height 1.1 ))))
  `(org-agenda-date-weekend ((,class (:weight normal :foreground ,shade4))))
  `(org-agenda-date-today ((,class (:weight bold :foreground ,accent1 :height 1.4))))
  `(org-agenda-done ((,class (:foreground ,shade3))))
  `(org-scheduled ((,class (:foreground ,accent5))))
  `(org-scheduled-today ((,class (:foreground ,accent6 :weight bold :height 1.2))))
  `(org-ellipsis ((,class (:foreground ,accent5))))
  `(org-verbatim ((,class (:foreground ,shade4))))
  `(org-document-info-keyword ((,class (:foreground ,accent6))))
  `(font-latex-bold-face ((,class (:foreground ,accent5))))
  `(font-latex-italic-face ((,class (:foreground ,accent7 :italic t))))
  `(font-latex-string-face ((,class (:foreground ,accent3))))
  `(font-latex-match-reference-keywords ((,class (:foreground ,accent4))))
  `(font-latex-match-variable-keywords ((,class (:foreground ,accent6))))
  `(ido-only-match ((,class (:foreground ,accent0))))
  `(org-sexp-date ((,class (:foreground ,shade4))))
  `(ido-first-match ((,class (:foreground ,accent1 :bold t))))
  `(gnus-header-content ((,class (:foreground ,accent1))))
  `(gnus-header-from ((,class (:foreground ,accent6))))
  `(gnus-header-name ((,class (:foreground ,accent5))))
  `(gnus-header-subject ((,class (:foreground ,accent6 :bold t))))
  `(mu4e-view-url-number-face ((,class (:foreground ,accent5))))
  `(mu4e-cited-1-face ((,class (:foreground ,shade6))))
  `(mu4e-cited-7-face ((,class (:foreground ,shade5))))
  `(mu4e-header-marks-face ((,class (:foreground ,accent5))))
  `(ffap ((,class (:foreground ,shade4))))
  `(js2-private-function-call ((,class (:foreground ,accent4))))
  `(js2-jsdoc-html-tag-delimiter ((,class (:foreground ,accent3))))
  `(js2-jsdoc-html-tag-name ((,class (:foreground ,accent2))))
  `(js2-external-variable ((,class (:foreground ,accent5  ))))
  `(js2-function-param ((,class (:foreground ,accent4))))
  `(js2-jsdoc-value ((,class (:foreground ,accent3))))
  `(js2-private-member ((,class (:foreground ,shade5))))
  `(js3-warning-face ((,class (:underline ,accent1))))
  `(js3-error-face ((,class (:underline ,accent0))))
  `(js3-external-variable-face ((,class (:foreground ,accent6))))
  `(js3-function-param-face ((,class (:foreground ,accent7))))
  `(js3-jsdoc-tag-face ((,class (:foreground ,accent1))))
  `(js3-instance-member-face ((,class (:foreground ,accent4))))
  `(warning ((,class (:foreground ,accent0))))
  `(ac-completion-face ((,class (:underline t :foreground ,accent1))))
  `(info-quoted-name ((,class (:foreground ,accent5))))
  `(info-string ((,class (:foreground ,accent3))))
  `(icompletep-determined ((,class :foreground ,accent5)))
  `(undo-tree-visualizer-current-face ((,class :foreground ,accent5)))
  `(undo-tree-visualizer-default-face ((,class :foreground ,shade6)))
  `(undo-tree-visualizer-unmodified-face ((,class :foreground ,accent6)))
  `(undo-tree-visualizer-register-face ((,class :foreground ,accent5)))
  `(slime-repl-inputed-output-face ((,class (:foreground ,accent5))))
  `(trailing-whitespace ((,class :foreground nil :background ,accent0)))
  `(rainbow-delimiters-depth-1-face ((,class :foreground ,shade7)))
  `(rainbow-delimiters-depth-2-face ((,class :foreground ,accent5)))
  `(rainbow-delimiters-depth-3-face ((,class :foreground ,accent6)))
  `(rainbow-delimiters-depth-4-face ((,class :foreground ,accent4)))
  `(rainbow-delimiters-depth-5-face ((,class :foreground ,accent1)))
  `(rainbow-delimiters-depth-6-face ((,class :foreground ,shade5)))
  `(rainbow-delimiters-depth-7-face ((,class :foreground ,accent5)))
  `(rainbow-delimiters-depth-8-face ((,class :foreground ,accent3)))
  `(magit-item-highlight ((,class :background ,shade2)))
  `(magit-section-heading        ((,class (:foreground ,accent1 :weight bold))))
  `(magit-hunk-heading           ((,class (:background ,shade2))))
  `(magit-section-highlight      ((,class (:background ,shade1))))
  `(magit-hunk-heading-highlight ((,class (:background ,shade2))))
  `(magit-diff-context-highlight ((,class (:background ,shade2 :foreground ,shade5))))
  `(magit-diffstat-added   ((,class (:foreground ,accent5))))
  `(magit-diffstat-removed ((,class (:foreground ,accent6))))
  `(magit-process-ok ((,class (:foreground ,accent6 :weight bold))))
  `(magit-process-ng ((,class (:foreground ,accent0 :weight bold))))
  `(magit-branch ((,class (:foreground ,accent4 :weight bold))))
  `(magit-log-author ((,class (:foreground ,shade5))))
  `(magit-hash ((,class (:foreground ,shade6))))
  `(magit-diff-file-header ((,class (:foreground ,shade6 :background ,shade2))))
  `(lazy-highlight ((,class (:foreground ,shade6 :background ,shade2))))
  `(term ((,class (:foreground ,shade6 :background ,shade0))))
  `(term-color-black ((,class (:foreground ,shade6 :background ,shade0))))
  `(term-color-blue ((,class (:foreground ,accent5 :background ,shade0))))
  `(term-color-red ((,class (:foreground ,accent0 :background ,shade0))))
  `(term-color-green ((,class (:foreground ,accent3 :background ,shade0))))
  `(term-color-yellow ((,class (:foreground ,accent2 :background ,shade0))))
  `(term-color-magenta ((,class (:foreground ,accent7 :background ,shade0))))
  `(term-color-cyan ((,class (:foreground ,accent4 :background ,shade0))))
  `(term-color-white ((,class (:foreground ,shade2 :background ,shade0))))
  `(rainbow-delimiters-unmatched-face ((,class :foreground ,accent0)))
  `(helm-header ((,class (:foreground ,shade6 :background ,shade0 :underline nil :box nil))))
  `(helm-source-header ((,class (:foreground ,accent1 :background ,shade0 :underline nil :weight bold))))
  `(helm-selection ((,class (:background ,shade1 :underline nil))))
  `(helm-selection-line ((,class (:background ,shade1))))
  `(helm-visible-mark ((,class (:foreground ,shade0 :background ,shade2))))
  `(helm-candidate-number ((,class (:foreground ,shade0 :background ,shade7))))
  `(helm-separator ((,class (:foreground ,accent5 :background ,shade0))))
  `(helm-time-zone-current ((,class (:foreground ,accent5 :background ,shade0))))
  `(helm-time-zone-home ((,class (:foreground ,accent5 :background ,shade0))))
  `(helm-buffer-not-saved ((,class (:foreground ,accent5 :background ,shade0))))
  `(helm-buffer-process ((,class (:foreground ,accent5 :background ,shade0))))
  `(helm-buffer-saved-out ((,class (:foreground ,shade7 :background ,shade0))))
  `(helm-buffer-size ((,class (:foreground ,shade7 :background ,shade0))))
  `(helm-ff-directory ((,class (:foreground ,accent6 :background ,shade0 :weight bold))))
  `(helm-ff-file ((,class (:foreground ,shade7 :background ,shade0 :weight normal))))
  `(helm-ff-executable ((,class (:foreground ,accent2 :background ,shade0 :weight normal))))
  `(helm-ff-invalid-symlink ((,class (:foreground ,accent7 :background ,shade0 :weight bold))))
  `(helm-ff-symlink ((,class (:foreground ,accent1 :background ,shade0 :weight bold))))
  `(helm-ff-prefix ((,class (:foreground ,shade0 :background ,accent1 :weight normal))))
  `(helm-grep-cmd-line ((,class (:foreground ,shade7 :background ,shade0))))
  `(helm-grep-file ((,class (:foreground ,shade7 :background ,shade0))))
  `(helm-grep-finish ((,class (:foreground ,shade6 :background ,shade0))))
  `(helm-grep-lineno ((,class (:foreground ,shade7 :background ,shade0))))
  `(helm-grep-match ((,class (:foreground nil :background nil :inherit helm-match))))
  `(helm-grep-running ((,class (:foreground ,accent6 :background ,shade0))))
  `(helm-moccur-buffer ((,class (:foreground ,accent6 :background ,shade0))))
  `(helm-source-go-package-godoc-description ((,class (:foreground ,accent3))))
  `(helm-bookmark-w3m ((,class (:foreground ,accent5))))
  `(company-echo-common ((,class (:foreground ,shade0 :background ,shade7))))
  `(company-preview ((,class (:background ,shade0 :foreground ,accent2))))
  `(company-preview-common ((,class (:foreground ,shade1 :foreground ,shade5))))
  `(company-preview-search ((,class (:foreground ,accent5 :background ,shade0))))
  `(company-scrollbar-bg ((,class (:background ,shade2))))
  `(company-scrollbar-fg ((,class (:foreground ,accent1))))
  `(company-tooltip ((,class (:foreground ,shade6 :background ,shade0 :bold t))))
  `(company-tooltop-annotation ((,class (:foreground ,accent4))))
  `(company-tooltip-common ((,class ( :foreground ,shade5))))
  `(company-tooltip-common-selection ((,class (:foreground ,accent3))))
  `(company-tooltip-mouse ((,class (:inherit highlight))))
  `(company-tooltip-selection ((,class (:background ,shade2 :foreground ,shade5))))
  `(company-template-field ((,class (:inherit region))))
  `(web-mode-builtin-face ((,class (:inherit ,font-lock-builtin-face))))
  `(web-mode-comment-face ((,class (:inherit ,font-lock-comment-face))))
  `(web-mode-constant-face ((,class (:inherit ,font-lock-constant-face))))
  `(web-mode-keyword-face ((,class (:foreground ,accent1))))
  `(web-mode-doctype-face ((,class (:inherit ,font-lock-comment-face))))
  `(web-mode-function-name-face ((,class (:inherit ,font-lock-function-name-face))))
  `(web-mode-string-face ((,class (:foreground ,accent3))))
  `(web-mode-type-face ((,class (:inherit ,font-lock-type-face))))
  `(web-mode-html-attr-name-face ((,class (:foreground ,accent6))))
  `(web-mode-html-attr-value-face ((,class (:foreground ,accent1))))
  `(web-mode-warning-face ((,class (:inherit ,font-lock-warning-face))))
  `(web-mode-html-tag-face ((,class (:foreground ,accent5))))
  `(jde-java-font-lock-package-face ((t (:foreground ,accent6))))
  `(jde-java-font-lock-public-face ((t (:foreground ,accent1))))
  `(jde-java-font-lock-private-face ((t (:foreground ,accent1))))
  `(jde-java-font-lock-constant-face ((t (:foreground ,accent4))))
  `(jde-java-font-lock-modifier-face ((t (:foreground ,accent7))))
  `(jde-jave-font-lock-protected-face ((t (:foreground ,accent1))))
  `(jde-java-font-lock-number-face ((t (:foreground ,accent6))))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
    (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'st-giles-blue-dark)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; st-giles-blue-dark-theme.el ends here
```

## st-giles-blue-light-theme.el

[Get the theme
here!](https://themer.dev/?colors.dark.shade0=%23222222&colors.dark.shade7=%23599ec4&colors.light.shade0=%23599ec4&colors.light.shade7=%23222222&activeColorSet=dark)

``` commonlisp
;;; st-giles-blue-light-theme.el

;; Version: 1.0.1
;; Package-Requires: ((emacs "24"))

;;; Commentary:

;; Generated by the emacs theme template for themer.

;;; Code:

(deftheme st-giles-blue-light)
(let ((class '((class color) (min-colors 89)))
  (shade0 "#599ec4")
  (shade1 "#518cad")
  (shade2 "#497b96")
  (shade3 "#41697f")
  (shade4 "#3a5767")
  (shade5 "#324550")
  (shade6 "#2a3439")
  (shade7 "#222222")
  (accent0 "#222222")
  (accent1 "#222222")
  (accent2 "#222222")
  (accent3 "#222222")
  (accent4 "#222222")
  (accent5 "#222222")
  (accent6 "#222222")
  (accent7 "#222222"))
(custom-theme-set-faces
  'st-giles-blue-light
  `(default ((,class (:background ,shade0 :foreground ,shade7))))
  `(font-lock-builtin-face ((,class (:foreground ,accent5))))
  `(font-lock-comment-face ((,class (:foreground ,shade3))))
  `(font-lock-negation-char-face ((,class (:foreground ,accent4))))
  `(font-lock-reference-face ((,class (:foreground ,accent4))))
  `(font-lock-constant-face ((,class (:foreground ,accent4))))
  `(font-lock-doc-face ((,class (:foreground ,shade3))))
  `(font-lock-function-name-face ((,class (:foreground ,accent6 :bold t))))
  `(font-lock-keyword-face ((,class (:bold ,class :foreground ,accent1))))
  `(font-lock-string-face ((,class (:foreground ,accent3))))
  `(font-lock-type-face ((,class (:foreground ,accent5 ))))
  `(font-lock-variable-name-face ((,class (:foreground ,accent6))))
  `(font-lock-warning-face ((,class (:foreground ,accent0 :background ,shade1))))
  `(region ((,class (:background ,shade7 :foreground ,shade0))))
  `(highlight ((,class (:foreground ,shade5 :background ,shade2))))
  `(hl-line ((,class (:background  ,shade1))))
  `(fringe ((,class (:background ,shade1 :foreground ,shade4))))
  `(cursor ((,class (:background ,shade2))))
  `(show-paren-match-face ((,class (:background ,accent0))))
  `(isearch ((,class (:bold t :foreground ,accent0 :background ,shade2))))
  `(mode-line ((,class (:box (:line-width 1 :color nil) :bold t :foreground ,shade4 :background ,shade0))))
  `(mode-line-inactive ((,class (:box (:line-width 1 :color nil :style pressed-button) :foreground ,accent7 :background ,shade1 :weight normal))))
  `(mode-line-buffer-id ((,class (:bold t :foreground ,accent6 :background nil))))
  `(mode-line-highlight ((,class (:foreground ,accent1 :box nil :weight bold))))
  `(mode-line-emphasis ((,class (:foreground ,shade7))))
  `(vertical-border ((,class (:foreground ,shade5))))
  `(minibuffer-prompt ((,class (:bold t :foreground ,accent1))))
  `(default-italic ((,class (:italic t))))
  `(link ((,class (:foreground ,accent4 :underline t))))
  `(org-code ((,class (:foreground ,shade6))))
  `(org-hide ((,class (:foreground ,shade4))))
  `(org-level-1 ((,class (:bold t :foreground ,accent4 :height 1.1))))
  `(org-level-2 ((,class (:bold nil :foreground ,accent5))))
  `(org-level-3 ((,class (:bold t :foreground ,accent6))))
  `(org-level-4 ((,class (:bold nil :foreground ,accent7))))
  `(org-date ((,class (:underline t :foreground ,accent2) )))
  `(org-footnote  ((,class (:underline t :foreground ,shade4))))
  `(org-link ((,class (:underline t :foreground ,accent5 ))))
  `(org-special-keyword ((,class (:foreground ,accent1))))
  `(org-block ((,class (:foreground ,shade5))))
  `(org-quote ((,class (:inherit org-block :slant italic))))
  `(org-verse ((,class (:inherit org-block :slant italic))))
  `(org-todo ((,class (:box (:line-width 1 :color ,shade1) :foreground ,accent1 :bold t))))
  `(org-done ((,class (:box (:line-width 1 :color ,shade1) :foreground ,shade5 :bold t))))
  `(org-warning ((,class (:underline t :foreground ,accent2))))
  `(org-agenda-structure ((,class (:weight bold :foreground ,shade5 :box (:color ,shade4) :background ,shade2))))
  `(org-agenda-date ((,class (:foreground ,accent6 :height 1.1 ))))
  `(org-agenda-date-weekend ((,class (:weight normal :foreground ,shade4))))
  `(org-agenda-date-today ((,class (:weight bold :foreground ,accent1 :height 1.4))))
  `(org-agenda-done ((,class (:foreground ,shade3))))
  `(org-scheduled ((,class (:foreground ,accent5))))
  `(org-scheduled-today ((,class (:foreground ,accent6 :weight bold :height 1.2))))
  `(org-ellipsis ((,class (:foreground ,accent5))))
  `(org-verbatim ((,class (:foreground ,shade4))))
  `(org-document-info-keyword ((,class (:foreground ,accent6))))
  `(font-latex-bold-face ((,class (:foreground ,accent5))))
  `(font-latex-italic-face ((,class (:foreground ,accent7 :italic t))))
  `(font-latex-string-face ((,class (:foreground ,accent3))))
  `(font-latex-match-reference-keywords ((,class (:foreground ,accent4))))
  `(font-latex-match-variable-keywords ((,class (:foreground ,accent6))))
  `(ido-only-match ((,class (:foreground ,accent0))))
  `(org-sexp-date ((,class (:foreground ,shade4))))
  `(ido-first-match ((,class (:foreground ,accent1 :bold t))))
  `(gnus-header-content ((,class (:foreground ,accent1))))
  `(gnus-header-from ((,class (:foreground ,accent6))))
  `(gnus-header-name ((,class (:foreground ,accent5))))
  `(gnus-header-subject ((,class (:foreground ,accent6 :bold t))))
  `(mu4e-view-url-number-face ((,class (:foreground ,accent5))))
  `(mu4e-cited-1-face ((,class (:foreground ,shade6))))
  `(mu4e-cited-7-face ((,class (:foreground ,shade5))))
  `(mu4e-header-marks-face ((,class (:foreground ,accent5))))
  `(ffap ((,class (:foreground ,shade4))))
  `(js2-private-function-call ((,class (:foreground ,accent4))))
  `(js2-jsdoc-html-tag-delimiter ((,class (:foreground ,accent3))))
  `(js2-jsdoc-html-tag-name ((,class (:foreground ,accent2))))
  `(js2-external-variable ((,class (:foreground ,accent5  ))))
  `(js2-function-param ((,class (:foreground ,accent4))))
  `(js2-jsdoc-value ((,class (:foreground ,accent3))))
  `(js2-private-member ((,class (:foreground ,shade5))))
  `(js3-warning-face ((,class (:underline ,accent1))))
  `(js3-error-face ((,class (:underline ,accent0))))
  `(js3-external-variable-face ((,class (:foreground ,accent6))))
  `(js3-function-param-face ((,class (:foreground ,accent7))))
  `(js3-jsdoc-tag-face ((,class (:foreground ,accent1))))
  `(js3-instance-member-face ((,class (:foreground ,accent4))))
  `(warning ((,class (:foreground ,accent0))))
  `(ac-completion-face ((,class (:underline t :foreground ,accent1))))
  `(info-quoted-name ((,class (:foreground ,accent5))))
  `(info-string ((,class (:foreground ,accent3))))
  `(icompletep-determined ((,class :foreground ,accent5)))
  `(undo-tree-visualizer-current-face ((,class :foreground ,accent5)))
  `(undo-tree-visualizer-default-face ((,class :foreground ,shade6)))
  `(undo-tree-visualizer-unmodified-face ((,class :foreground ,accent6)))
  `(undo-tree-visualizer-register-face ((,class :foreground ,accent5)))
  `(slime-repl-inputed-output-face ((,class (:foreground ,accent5))))
  `(trailing-whitespace ((,class :foreground nil :background ,accent0)))
  `(rainbow-delimiters-depth-1-face ((,class :foreground ,shade7)))
  `(rainbow-delimiters-depth-2-face ((,class :foreground ,accent5)))
  `(rainbow-delimiters-depth-3-face ((,class :foreground ,accent6)))
  `(rainbow-delimiters-depth-4-face ((,class :foreground ,accent4)))
  `(rainbow-delimiters-depth-5-face ((,class :foreground ,accent1)))
  `(rainbow-delimiters-depth-6-face ((,class :foreground ,shade5)))
  `(rainbow-delimiters-depth-7-face ((,class :foreground ,accent5)))
  `(rainbow-delimiters-depth-8-face ((,class :foreground ,accent3)))
  `(magit-item-highlight ((,class :background ,shade2)))
  `(magit-section-heading        ((,class (:foreground ,accent1 :weight bold))))
  `(magit-hunk-heading           ((,class (:background ,shade2))))
  `(magit-section-highlight      ((,class (:background ,shade1))))
  `(magit-hunk-heading-highlight ((,class (:background ,shade2))))
  `(magit-diff-context-highlight ((,class (:background ,shade2 :foreground ,shade5))))
  `(magit-diffstat-added   ((,class (:foreground ,accent5))))
  `(magit-diffstat-removed ((,class (:foreground ,accent6))))
  `(magit-process-ok ((,class (:foreground ,accent6 :weight bold))))
  `(magit-process-ng ((,class (:foreground ,accent0 :weight bold))))
  `(magit-branch ((,class (:foreground ,accent4 :weight bold))))
  `(magit-log-author ((,class (:foreground ,shade5))))
  `(magit-hash ((,class (:foreground ,shade6))))
  `(magit-diff-file-header ((,class (:foreground ,shade6 :background ,shade2))))
  `(lazy-highlight ((,class (:foreground ,shade6 :background ,shade2))))
  `(term ((,class (:foreground ,shade6 :background ,shade0))))
  `(term-color-black ((,class (:foreground ,shade6 :background ,shade0))))
  `(term-color-blue ((,class (:foreground ,accent5 :background ,shade0))))
  `(term-color-red ((,class (:foreground ,accent0 :background ,shade0))))
  `(term-color-green ((,class (:foreground ,accent3 :background ,shade0))))
  `(term-color-yellow ((,class (:foreground ,accent2 :background ,shade0))))
  `(term-color-magenta ((,class (:foreground ,accent7 :background ,shade0))))
  `(term-color-cyan ((,class (:foreground ,accent4 :background ,shade0))))
  `(term-color-white ((,class (:foreground ,shade2 :background ,shade0))))
  `(rainbow-delimiters-unmatched-face ((,class :foreground ,accent0)))
  `(helm-header ((,class (:foreground ,shade6 :background ,shade0 :underline nil :box nil))))
  `(helm-source-header ((,class (:foreground ,accent1 :background ,shade0 :underline nil :weight bold))))
  `(helm-selection ((,class (:background ,shade1 :underline nil))))
  `(helm-selection-line ((,class (:background ,shade1))))
  `(helm-visible-mark ((,class (:foreground ,shade0 :background ,shade2))))
  `(helm-candidate-number ((,class (:foreground ,shade0 :background ,shade7))))
  `(helm-separator ((,class (:foreground ,accent5 :background ,shade0))))
  `(helm-time-zone-current ((,class (:foreground ,accent5 :background ,shade0))))
  `(helm-time-zone-home ((,class (:foreground ,accent5 :background ,shade0))))
  `(helm-buffer-not-saved ((,class (:foreground ,accent5 :background ,shade0))))
  `(helm-buffer-process ((,class (:foreground ,accent5 :background ,shade0))))
  `(helm-buffer-saved-out ((,class (:foreground ,shade7 :background ,shade0))))
  `(helm-buffer-size ((,class (:foreground ,shade7 :background ,shade0))))
  `(helm-ff-directory ((,class (:foreground ,accent6 :background ,shade0 :weight bold))))
  `(helm-ff-file ((,class (:foreground ,shade7 :background ,shade0 :weight normal))))
  `(helm-ff-executable ((,class (:foreground ,accent2 :background ,shade0 :weight normal))))
  `(helm-ff-invalid-symlink ((,class (:foreground ,accent7 :background ,shade0 :weight bold))))
  `(helm-ff-symlink ((,class (:foreground ,accent1 :background ,shade0 :weight bold))))
  `(helm-ff-prefix ((,class (:foreground ,shade0 :background ,accent1 :weight normal))))
  `(helm-grep-cmd-line ((,class (:foreground ,shade7 :background ,shade0))))
  `(helm-grep-file ((,class (:foreground ,shade7 :background ,shade0))))
  `(helm-grep-finish ((,class (:foreground ,shade6 :background ,shade0))))
  `(helm-grep-lineno ((,class (:foreground ,shade7 :background ,shade0))))
  `(helm-grep-match ((,class (:foreground nil :background nil :inherit helm-match))))
  `(helm-grep-running ((,class (:foreground ,accent6 :background ,shade0))))
  `(helm-moccur-buffer ((,class (:foreground ,accent6 :background ,shade0))))
  `(helm-source-go-package-godoc-description ((,class (:foreground ,accent3))))
  `(helm-bookmark-w3m ((,class (:foreground ,accent5))))
  `(company-echo-common ((,class (:foreground ,shade0 :background ,shade7))))
  `(company-preview ((,class (:background ,shade0 :foreground ,accent2))))
  `(company-preview-common ((,class (:foreground ,shade1 :foreground ,shade5))))
  `(company-preview-search ((,class (:foreground ,accent5 :background ,shade0))))
  `(company-scrollbar-bg ((,class (:background ,shade2))))
  `(company-scrollbar-fg ((,class (:foreground ,accent1))))
  `(company-tooltip ((,class (:foreground ,shade6 :background ,shade0 :bold t))))
  `(company-tooltop-annotation ((,class (:foreground ,accent4))))
  `(company-tooltip-common ((,class ( :foreground ,shade5))))
  `(company-tooltip-common-selection ((,class (:foreground ,accent3))))
  `(company-tooltip-mouse ((,class (:inherit highlight))))
  `(company-tooltip-selection ((,class (:background ,shade2 :foreground ,shade5))))
  `(company-template-field ((,class (:inherit region))))
  `(web-mode-builtin-face ((,class (:inherit ,font-lock-builtin-face))))
  `(web-mode-comment-face ((,class (:inherit ,font-lock-comment-face))))
  `(web-mode-constant-face ((,class (:inherit ,font-lock-constant-face))))
  `(web-mode-keyword-face ((,class (:foreground ,accent1))))
  `(web-mode-doctype-face ((,class (:inherit ,font-lock-comment-face))))
  `(web-mode-function-name-face ((,class (:inherit ,font-lock-function-name-face))))
  `(web-mode-string-face ((,class (:foreground ,accent3))))
  `(web-mode-type-face ((,class (:inherit ,font-lock-type-face))))
  `(web-mode-html-attr-name-face ((,class (:foreground ,accent6))))
  `(web-mode-html-attr-value-face ((,class (:foreground ,accent1))))
  `(web-mode-warning-face ((,class (:inherit ,font-lock-warning-face))))
  `(web-mode-html-tag-face ((,class (:foreground ,accent5))))
  `(jde-java-font-lock-package-face ((t (:foreground ,accent6))))
  `(jde-java-font-lock-public-face ((t (:foreground ,accent1))))
  `(jde-java-font-lock-private-face ((t (:foreground ,accent1))))
  `(jde-java-font-lock-constant-face ((t (:foreground ,accent4))))
  `(jde-java-font-lock-modifier-face ((t (:foreground ,accent7))))
  `(jde-jave-font-lock-protected-face ((t (:foreground ,accent1))))
  `(jde-java-font-lock-number-face ((t (:foreground ,accent6))))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
    (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'st-giles-blue-light)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; st-giles-blue-light-theme.el ends here
```

## ghostfreak-green-dark-theme.el

[Get the theme
here!](https://themer.dev/?colors.dark.shade0=%23222222&colors.dark.shade7=%23E3FFC5&colors.light.shade0=%23E3FFC5&colors.light.shade7=%23222222&activeColorSet=dark)

``` commonlisp
;;; ghostfreak-green-dark-theme.el

;; Version: 1.0.1
;; Package-Requires: ((emacs "24"))

;;; Commentary:

;; Generated by the emacs theme template for themer.

;;; Code:

(deftheme ghostfreak-green-dark)
(let ((class '((class color) (min-colors 89)))
  (shade0 "#222222")
  (shade1 "#3e4239")
  (shade2 "#596151")
  (shade3 "#758168")
  (shade4 "#90a07f")
  (shade5 "#acc096")
  (shade6 "#c7dfae")
  (shade7 "#e3ffc5")
  (accent0 "#e3ffc5")
  (accent1 "#e3ffc5")
  (accent2 "#e3ffc5")
  (accent3 "#e3ffc5")
  (accent4 "#e3ffc5")
  (accent5 "#e3ffc5")
  (accent6 "#e3ffc5")
  (accent7 "#e3ffc5"))
(custom-theme-set-faces
  'ghostfreak-green-dark
  `(default ((,class (:background ,shade0 :foreground ,shade7))))
  `(font-lock-builtin-face ((,class (:foreground ,accent5))))
  `(font-lock-comment-face ((,class (:foreground ,shade3))))
  `(font-lock-negation-char-face ((,class (:foreground ,accent4))))
  `(font-lock-reference-face ((,class (:foreground ,accent4))))
  `(font-lock-constant-face ((,class (:foreground ,accent4))))
  `(font-lock-doc-face ((,class (:foreground ,shade3))))
  `(font-lock-function-name-face ((,class (:foreground ,accent6 :bold t))))
  `(font-lock-keyword-face ((,class (:bold ,class :foreground ,accent1))))
  `(font-lock-string-face ((,class (:foreground ,accent3))))
  `(font-lock-type-face ((,class (:foreground ,accent5 ))))
  `(font-lock-variable-name-face ((,class (:foreground ,accent6))))
  `(font-lock-warning-face ((,class (:foreground ,accent0 :background ,shade1))))
  `(region ((,class (:background ,shade7 :foreground ,shade0))))
  `(highlight ((,class (:foreground ,shade5 :background ,shade2))))
  `(hl-line ((,class (:background  ,shade1))))
  `(fringe ((,class (:background ,shade1 :foreground ,shade4))))
  `(cursor ((,class (:background ,shade2))))
  `(show-paren-match-face ((,class (:background ,accent0))))
  `(isearch ((,class (:bold t :foreground ,accent0 :background ,shade2))))
  `(mode-line ((,class (:box (:line-width 1 :color nil) :bold t :foreground ,shade4 :background ,shade0))))
  `(mode-line-inactive ((,class (:box (:line-width 1 :color nil :style pressed-button) :foreground ,accent7 :background ,shade1 :weight normal))))
  `(mode-line-buffer-id ((,class (:bold t :foreground ,accent6 :background nil))))
  `(mode-line-highlight ((,class (:foreground ,accent1 :box nil :weight bold))))
  `(mode-line-emphasis ((,class (:foreground ,shade7))))
  `(vertical-border ((,class (:foreground ,shade5))))
  `(minibuffer-prompt ((,class (:bold t :foreground ,accent1))))
  `(default-italic ((,class (:italic t))))
  `(link ((,class (:foreground ,accent4 :underline t))))
  `(org-code ((,class (:foreground ,shade6))))
  `(org-hide ((,class (:foreground ,shade4))))
  `(org-level-1 ((,class (:bold t :foreground ,accent4 :height 1.1))))
  `(org-level-2 ((,class (:bold nil :foreground ,accent5))))
  `(org-level-3 ((,class (:bold t :foreground ,accent6))))
  `(org-level-4 ((,class (:bold nil :foreground ,accent7))))
  `(org-date ((,class (:underline t :foreground ,accent2) )))
  `(org-footnote  ((,class (:underline t :foreground ,shade4))))
  `(org-link ((,class (:underline t :foreground ,accent5 ))))
  `(org-special-keyword ((,class (:foreground ,accent1))))
  `(org-block ((,class (:foreground ,shade5))))
  `(org-quote ((,class (:inherit org-block :slant italic))))
  `(org-verse ((,class (:inherit org-block :slant italic))))
  `(org-todo ((,class (:box (:line-width 1 :color ,shade1) :foreground ,accent1 :bold t))))
  `(org-done ((,class (:box (:line-width 1 :color ,shade1) :foreground ,shade5 :bold t))))
  `(org-warning ((,class (:underline t :foreground ,accent2))))
  `(org-agenda-structure ((,class (:weight bold :foreground ,shade5 :box (:color ,shade4) :background ,shade2))))
  `(org-agenda-date ((,class (:foreground ,accent6 :height 1.1 ))))
  `(org-agenda-date-weekend ((,class (:weight normal :foreground ,shade4))))
  `(org-agenda-date-today ((,class (:weight bold :foreground ,accent1 :height 1.4))))
  `(org-agenda-done ((,class (:foreground ,shade3))))
  `(org-scheduled ((,class (:foreground ,accent5))))
  `(org-scheduled-today ((,class (:foreground ,accent6 :weight bold :height 1.2))))
  `(org-ellipsis ((,class (:foreground ,accent5))))
  `(org-verbatim ((,class (:foreground ,shade4))))
  `(org-document-info-keyword ((,class (:foreground ,accent6))))
  `(font-latex-bold-face ((,class (:foreground ,accent5))))
  `(font-latex-italic-face ((,class (:foreground ,accent7 :italic t))))
  `(font-latex-string-face ((,class (:foreground ,accent3))))
  `(font-latex-match-reference-keywords ((,class (:foreground ,accent4))))
  `(font-latex-match-variable-keywords ((,class (:foreground ,accent6))))
  `(ido-only-match ((,class (:foreground ,accent0))))
  `(org-sexp-date ((,class (:foreground ,shade4))))
  `(ido-first-match ((,class (:foreground ,accent1 :bold t))))
  `(gnus-header-content ((,class (:foreground ,accent1))))
  `(gnus-header-from ((,class (:foreground ,accent6))))
  `(gnus-header-name ((,class (:foreground ,accent5))))
  `(gnus-header-subject ((,class (:foreground ,accent6 :bold t))))
  `(mu4e-view-url-number-face ((,class (:foreground ,accent5))))
  `(mu4e-cited-1-face ((,class (:foreground ,shade6))))
  `(mu4e-cited-7-face ((,class (:foreground ,shade5))))
  `(mu4e-header-marks-face ((,class (:foreground ,accent5))))
  `(ffap ((,class (:foreground ,shade4))))
  `(js2-private-function-call ((,class (:foreground ,accent4))))
  `(js2-jsdoc-html-tag-delimiter ((,class (:foreground ,accent3))))
  `(js2-jsdoc-html-tag-name ((,class (:foreground ,accent2))))
  `(js2-external-variable ((,class (:foreground ,accent5  ))))
  `(js2-function-param ((,class (:foreground ,accent4))))
  `(js2-jsdoc-value ((,class (:foreground ,accent3))))
  `(js2-private-member ((,class (:foreground ,shade5))))
  `(js3-warning-face ((,class (:underline ,accent1))))
  `(js3-error-face ((,class (:underline ,accent0))))
  `(js3-external-variable-face ((,class (:foreground ,accent6))))
  `(js3-function-param-face ((,class (:foreground ,accent7))))
  `(js3-jsdoc-tag-face ((,class (:foreground ,accent1))))
  `(js3-instance-member-face ((,class (:foreground ,accent4))))
  `(warning ((,class (:foreground ,accent0))))
  `(ac-completion-face ((,class (:underline t :foreground ,accent1))))
  `(info-quoted-name ((,class (:foreground ,accent5))))
  `(info-string ((,class (:foreground ,accent3))))
  `(icompletep-determined ((,class :foreground ,accent5)))
  `(undo-tree-visualizer-current-face ((,class :foreground ,accent5)))
  `(undo-tree-visualizer-default-face ((,class :foreground ,shade6)))
  `(undo-tree-visualizer-unmodified-face ((,class :foreground ,accent6)))
  `(undo-tree-visualizer-register-face ((,class :foreground ,accent5)))
  `(slime-repl-inputed-output-face ((,class (:foreground ,accent5))))
  `(trailing-whitespace ((,class :foreground nil :background ,accent0)))
  `(rainbow-delimiters-depth-1-face ((,class :foreground ,shade7)))
  `(rainbow-delimiters-depth-2-face ((,class :foreground ,accent5)))
  `(rainbow-delimiters-depth-3-face ((,class :foreground ,accent6)))
  `(rainbow-delimiters-depth-4-face ((,class :foreground ,accent4)))
  `(rainbow-delimiters-depth-5-face ((,class :foreground ,accent1)))
  `(rainbow-delimiters-depth-6-face ((,class :foreground ,shade5)))
  `(rainbow-delimiters-depth-7-face ((,class :foreground ,accent5)))
  `(rainbow-delimiters-depth-8-face ((,class :foreground ,accent3)))
  `(magit-item-highlight ((,class :background ,shade2)))
  `(magit-section-heading        ((,class (:foreground ,accent1 :weight bold))))
  `(magit-hunk-heading           ((,class (:background ,shade2))))
  `(magit-section-highlight      ((,class (:background ,shade1))))
  `(magit-hunk-heading-highlight ((,class (:background ,shade2))))
  `(magit-diff-context-highlight ((,class (:background ,shade2 :foreground ,shade5))))
  `(magit-diffstat-added   ((,class (:foreground ,accent5))))
  `(magit-diffstat-removed ((,class (:foreground ,accent6))))
  `(magit-process-ok ((,class (:foreground ,accent6 :weight bold))))
  `(magit-process-ng ((,class (:foreground ,accent0 :weight bold))))
  `(magit-branch ((,class (:foreground ,accent4 :weight bold))))
  `(magit-log-author ((,class (:foreground ,shade5))))
  `(magit-hash ((,class (:foreground ,shade6))))
  `(magit-diff-file-header ((,class (:foreground ,shade6 :background ,shade2))))
  `(lazy-highlight ((,class (:foreground ,shade6 :background ,shade2))))
  `(term ((,class (:foreground ,shade6 :background ,shade0))))
  `(term-color-black ((,class (:foreground ,shade6 :background ,shade0))))
  `(term-color-blue ((,class (:foreground ,accent5 :background ,shade0))))
  `(term-color-red ((,class (:foreground ,accent0 :background ,shade0))))
  `(term-color-green ((,class (:foreground ,accent3 :background ,shade0))))
  `(term-color-yellow ((,class (:foreground ,accent2 :background ,shade0))))
  `(term-color-magenta ((,class (:foreground ,accent7 :background ,shade0))))
  `(term-color-cyan ((,class (:foreground ,accent4 :background ,shade0))))
  `(term-color-white ((,class (:foreground ,shade2 :background ,shade0))))
  `(rainbow-delimiters-unmatched-face ((,class :foreground ,accent0)))
  `(helm-header ((,class (:foreground ,shade6 :background ,shade0 :underline nil :box nil))))
  `(helm-source-header ((,class (:foreground ,accent1 :background ,shade0 :underline nil :weight bold))))
  `(helm-selection ((,class (:background ,shade1 :underline nil))))
  `(helm-selection-line ((,class (:background ,shade1))))
  `(helm-visible-mark ((,class (:foreground ,shade0 :background ,shade2))))
  `(helm-candidate-number ((,class (:foreground ,shade0 :background ,shade7))))
  `(helm-separator ((,class (:foreground ,accent5 :background ,shade0))))
  `(helm-time-zone-current ((,class (:foreground ,accent5 :background ,shade0))))
  `(helm-time-zone-home ((,class (:foreground ,accent5 :background ,shade0))))
  `(helm-buffer-not-saved ((,class (:foreground ,accent5 :background ,shade0))))
  `(helm-buffer-process ((,class (:foreground ,accent5 :background ,shade0))))
  `(helm-buffer-saved-out ((,class (:foreground ,shade7 :background ,shade0))))
  `(helm-buffer-size ((,class (:foreground ,shade7 :background ,shade0))))
  `(helm-ff-directory ((,class (:foreground ,accent6 :background ,shade0 :weight bold))))
  `(helm-ff-file ((,class (:foreground ,shade7 :background ,shade0 :weight normal))))
  `(helm-ff-executable ((,class (:foreground ,accent2 :background ,shade0 :weight normal))))
  `(helm-ff-invalid-symlink ((,class (:foreground ,accent7 :background ,shade0 :weight bold))))
  `(helm-ff-symlink ((,class (:foreground ,accent1 :background ,shade0 :weight bold))))
  `(helm-ff-prefix ((,class (:foreground ,shade0 :background ,accent1 :weight normal))))
  `(helm-grep-cmd-line ((,class (:foreground ,shade7 :background ,shade0))))
  `(helm-grep-file ((,class (:foreground ,shade7 :background ,shade0))))
  `(helm-grep-finish ((,class (:foreground ,shade6 :background ,shade0))))
  `(helm-grep-lineno ((,class (:foreground ,shade7 :background ,shade0))))
  `(helm-grep-match ((,class (:foreground nil :background nil :inherit helm-match))))
  `(helm-grep-running ((,class (:foreground ,accent6 :background ,shade0))))
  `(helm-moccur-buffer ((,class (:foreground ,accent6 :background ,shade0))))
  `(helm-source-go-package-godoc-description ((,class (:foreground ,accent3))))
  `(helm-bookmark-w3m ((,class (:foreground ,accent5))))
  `(company-echo-common ((,class (:foreground ,shade0 :background ,shade7))))
  `(company-preview ((,class (:background ,shade0 :foreground ,accent2))))
  `(company-preview-common ((,class (:foreground ,shade1 :foreground ,shade5))))
  `(company-preview-search ((,class (:foreground ,accent5 :background ,shade0))))
  `(company-scrollbar-bg ((,class (:background ,shade2))))
  `(company-scrollbar-fg ((,class (:foreground ,accent1))))
  `(company-tooltip ((,class (:foreground ,shade6 :background ,shade0 :bold t))))
  `(company-tooltop-annotation ((,class (:foreground ,accent4))))
  `(company-tooltip-common ((,class ( :foreground ,shade5))))
  `(company-tooltip-common-selection ((,class (:foreground ,accent3))))
  `(company-tooltip-mouse ((,class (:inherit highlight))))
  `(company-tooltip-selection ((,class (:background ,shade2 :foreground ,shade5))))
  `(company-template-field ((,class (:inherit region))))
  `(web-mode-builtin-face ((,class (:inherit ,font-lock-builtin-face))))
  `(web-mode-comment-face ((,class (:inherit ,font-lock-comment-face))))
  `(web-mode-constant-face ((,class (:inherit ,font-lock-constant-face))))
  `(web-mode-keyword-face ((,class (:foreground ,accent1))))
  `(web-mode-doctype-face ((,class (:inherit ,font-lock-comment-face))))
  `(web-mode-function-name-face ((,class (:inherit ,font-lock-function-name-face))))
  `(web-mode-string-face ((,class (:foreground ,accent3))))
  `(web-mode-type-face ((,class (:inherit ,font-lock-type-face))))
  `(web-mode-html-attr-name-face ((,class (:foreground ,accent6))))
  `(web-mode-html-attr-value-face ((,class (:foreground ,accent1))))
  `(web-mode-warning-face ((,class (:inherit ,font-lock-warning-face))))
  `(web-mode-html-tag-face ((,class (:foreground ,accent5))))
  `(jde-java-font-lock-package-face ((t (:foreground ,accent6))))
  `(jde-java-font-lock-public-face ((t (:foreground ,accent1))))
  `(jde-java-font-lock-private-face ((t (:foreground ,accent1))))
  `(jde-java-font-lock-constant-face ((t (:foreground ,accent4))))
  `(jde-java-font-lock-modifier-face ((t (:foreground ,accent7))))
  `(jde-jave-font-lock-protected-face ((t (:foreground ,accent1))))
  `(jde-java-font-lock-number-face ((t (:foreground ,accent6))))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
    (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'ghostfreak-green-dark)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; ghostfreak-green-dark-theme.el ends here
```

## ghostfreak-green-light-theme.el

[Get the theme
here!](https://themer.dev/?colors.dark.shade0=%23222222&colors.dark.shade7=%23E3FFC5&colors.light.shade0=%23E3FFC5&colors.light.shade7=%23222222&activeColorSet=dark)

``` commonlisp
;;; ghostfreak-green-light-theme.el

;; Version: 1.0.1
;; Package-Requires: ((emacs "24"))

;;; Commentary:

;; Generated by the emacs theme template for themer.

;;; Code:

(deftheme ghostfreak-green-light)
(let ((class '((class color) (min-colors 89)))
  (shade0 "#e3ffc5")
  (shade1 "#c7dfae")
  (shade2 "#acc096")
  (shade3 "#90a07f")
  (shade4 "#758168")
  (shade5 "#596151")
  (shade6 "#3e4239")
  (shade7 "#222222")
  (accent0 "#222222")
  (accent1 "#222222")
  (accent2 "#222222")
  (accent3 "#222222")
  (accent4 "#222222")
  (accent5 "#222222")
  (accent6 "#222222")
  (accent7 "#222222"))
(custom-theme-set-faces
  'ghostfreak-green-light
  `(default ((,class (:background ,shade0 :foreground ,shade7))))
  `(font-lock-builtin-face ((,class (:foreground ,accent5))))
  `(font-lock-comment-face ((,class (:foreground ,shade3))))
  `(font-lock-negation-char-face ((,class (:foreground ,accent4))))
  `(font-lock-reference-face ((,class (:foreground ,accent4))))
  `(font-lock-constant-face ((,class (:foreground ,accent4))))
  `(font-lock-doc-face ((,class (:foreground ,shade3))))
  `(font-lock-function-name-face ((,class (:foreground ,accent6 :bold t))))
  `(font-lock-keyword-face ((,class (:bold ,class :foreground ,accent1))))
  `(font-lock-string-face ((,class (:foreground ,accent3))))
  `(font-lock-type-face ((,class (:foreground ,accent5 ))))
  `(font-lock-variable-name-face ((,class (:foreground ,accent6))))
  `(font-lock-warning-face ((,class (:foreground ,accent0 :background ,shade1))))
  `(region ((,class (:background ,shade7 :foreground ,shade0))))
  `(highlight ((,class (:foreground ,shade5 :background ,shade2))))
  `(hl-line ((,class (:background  ,shade1))))
  `(fringe ((,class (:background ,shade1 :foreground ,shade4))))
  `(cursor ((,class (:background ,shade2))))
  `(show-paren-match-face ((,class (:background ,accent0))))
  `(isearch ((,class (:bold t :foreground ,accent0 :background ,shade2))))
  `(mode-line ((,class (:box (:line-width 1 :color nil) :bold t :foreground ,shade4 :background ,shade0))))
  `(mode-line-inactive ((,class (:box (:line-width 1 :color nil :style pressed-button) :foreground ,accent7 :background ,shade1 :weight normal))))
  `(mode-line-buffer-id ((,class (:bold t :foreground ,accent6 :background nil))))
  `(mode-line-highlight ((,class (:foreground ,accent1 :box nil :weight bold))))
  `(mode-line-emphasis ((,class (:foreground ,shade7))))
  `(vertical-border ((,class (:foreground ,shade5))))
  `(minibuffer-prompt ((,class (:bold t :foreground ,accent1))))
  `(default-italic ((,class (:italic t))))
  `(link ((,class (:foreground ,accent4 :underline t))))
  `(org-code ((,class (:foreground ,shade6))))
  `(org-hide ((,class (:foreground ,shade4))))
  `(org-level-1 ((,class (:bold t :foreground ,accent4 :height 1.1))))
  `(org-level-2 ((,class (:bold nil :foreground ,accent5))))
  `(org-level-3 ((,class (:bold t :foreground ,accent6))))
  `(org-level-4 ((,class (:bold nil :foreground ,accent7))))
  `(org-date ((,class (:underline t :foreground ,accent2) )))
  `(org-footnote  ((,class (:underline t :foreground ,shade4))))
  `(org-link ((,class (:underline t :foreground ,accent5 ))))
  `(org-special-keyword ((,class (:foreground ,accent1))))
  `(org-block ((,class (:foreground ,shade5))))
  `(org-quote ((,class (:inherit org-block :slant italic))))
  `(org-verse ((,class (:inherit org-block :slant italic))))
  `(org-todo ((,class (:box (:line-width 1 :color ,shade1) :foreground ,accent1 :bold t))))
  `(org-done ((,class (:box (:line-width 1 :color ,shade1) :foreground ,shade5 :bold t))))
  `(org-warning ((,class (:underline t :foreground ,accent2))))
  `(org-agenda-structure ((,class (:weight bold :foreground ,shade5 :box (:color ,shade4) :background ,shade2))))
  `(org-agenda-date ((,class (:foreground ,accent6 :height 1.1 ))))
  `(org-agenda-date-weekend ((,class (:weight normal :foreground ,shade4))))
  `(org-agenda-date-today ((,class (:weight bold :foreground ,accent1 :height 1.4))))
  `(org-agenda-done ((,class (:foreground ,shade3))))
  `(org-scheduled ((,class (:foreground ,accent5))))
  `(org-scheduled-today ((,class (:foreground ,accent6 :weight bold :height 1.2))))
  `(org-ellipsis ((,class (:foreground ,accent5))))
  `(org-verbatim ((,class (:foreground ,shade4))))
  `(org-document-info-keyword ((,class (:foreground ,accent6))))
  `(font-latex-bold-face ((,class (:foreground ,accent5))))
  `(font-latex-italic-face ((,class (:foreground ,accent7 :italic t))))
  `(font-latex-string-face ((,class (:foreground ,accent3))))
  `(font-latex-match-reference-keywords ((,class (:foreground ,accent4))))
  `(font-latex-match-variable-keywords ((,class (:foreground ,accent6))))
  `(ido-only-match ((,class (:foreground ,accent0))))
  `(org-sexp-date ((,class (:foreground ,shade4))))
  `(ido-first-match ((,class (:foreground ,accent1 :bold t))))
  `(gnus-header-content ((,class (:foreground ,accent1))))
  `(gnus-header-from ((,class (:foreground ,accent6))))
  `(gnus-header-name ((,class (:foreground ,accent5))))
  `(gnus-header-subject ((,class (:foreground ,accent6 :bold t))))
  `(mu4e-view-url-number-face ((,class (:foreground ,accent5))))
  `(mu4e-cited-1-face ((,class (:foreground ,shade6))))
  `(mu4e-cited-7-face ((,class (:foreground ,shade5))))
  `(mu4e-header-marks-face ((,class (:foreground ,accent5))))
  `(ffap ((,class (:foreground ,shade4))))
  `(js2-private-function-call ((,class (:foreground ,accent4))))
  `(js2-jsdoc-html-tag-delimiter ((,class (:foreground ,accent3))))
  `(js2-jsdoc-html-tag-name ((,class (:foreground ,accent2))))
  `(js2-external-variable ((,class (:foreground ,accent5  ))))
  `(js2-function-param ((,class (:foreground ,accent4))))
  `(js2-jsdoc-value ((,class (:foreground ,accent3))))
  `(js2-private-member ((,class (:foreground ,shade5))))
  `(js3-warning-face ((,class (:underline ,accent1))))
  `(js3-error-face ((,class (:underline ,accent0))))
  `(js3-external-variable-face ((,class (:foreground ,accent6))))
  `(js3-function-param-face ((,class (:foreground ,accent7))))
  `(js3-jsdoc-tag-face ((,class (:foreground ,accent1))))
  `(js3-instance-member-face ((,class (:foreground ,accent4))))
  `(warning ((,class (:foreground ,accent0))))
  `(ac-completion-face ((,class (:underline t :foreground ,accent1))))
  `(info-quoted-name ((,class (:foreground ,accent5))))
  `(info-string ((,class (:foreground ,accent3))))
  `(icompletep-determined ((,class :foreground ,accent5)))
  `(undo-tree-visualizer-current-face ((,class :foreground ,accent5)))
  `(undo-tree-visualizer-default-face ((,class :foreground ,shade6)))
  `(undo-tree-visualizer-unmodified-face ((,class :foreground ,accent6)))
  `(undo-tree-visualizer-register-face ((,class :foreground ,accent5)))
  `(slime-repl-inputed-output-face ((,class (:foreground ,accent5))))
  `(trailing-whitespace ((,class :foreground nil :background ,accent0)))
  `(rainbow-delimiters-depth-1-face ((,class :foreground ,shade7)))
  `(rainbow-delimiters-depth-2-face ((,class :foreground ,accent5)))
  `(rainbow-delimiters-depth-3-face ((,class :foreground ,accent6)))
  `(rainbow-delimiters-depth-4-face ((,class :foreground ,accent4)))
  `(rainbow-delimiters-depth-5-face ((,class :foreground ,accent1)))
  `(rainbow-delimiters-depth-6-face ((,class :foreground ,shade5)))
  `(rainbow-delimiters-depth-7-face ((,class :foreground ,accent5)))
  `(rainbow-delimiters-depth-8-face ((,class :foreground ,accent3)))
  `(magit-item-highlight ((,class :background ,shade2)))
  `(magit-section-heading        ((,class (:foreground ,accent1 :weight bold))))
  `(magit-hunk-heading           ((,class (:background ,shade2))))
  `(magit-section-highlight      ((,class (:background ,shade1))))
  `(magit-hunk-heading-highlight ((,class (:background ,shade2))))
  `(magit-diff-context-highlight ((,class (:background ,shade2 :foreground ,shade5))))
  `(magit-diffstat-added   ((,class (:foreground ,accent5))))
  `(magit-diffstat-removed ((,class (:foreground ,accent6))))
  `(magit-process-ok ((,class (:foreground ,accent6 :weight bold))))
  `(magit-process-ng ((,class (:foreground ,accent0 :weight bold))))
  `(magit-branch ((,class (:foreground ,accent4 :weight bold))))
  `(magit-log-author ((,class (:foreground ,shade5))))
  `(magit-hash ((,class (:foreground ,shade6))))
  `(magit-diff-file-header ((,class (:foreground ,shade6 :background ,shade2))))
  `(lazy-highlight ((,class (:foreground ,shade6 :background ,shade2))))
  `(term ((,class (:foreground ,shade6 :background ,shade0))))
  `(term-color-black ((,class (:foreground ,shade6 :background ,shade0))))
  `(term-color-blue ((,class (:foreground ,accent5 :background ,shade0))))
  `(term-color-red ((,class (:foreground ,accent0 :background ,shade0))))
  `(term-color-green ((,class (:foreground ,accent3 :background ,shade0))))
  `(term-color-yellow ((,class (:foreground ,accent2 :background ,shade0))))
  `(term-color-magenta ((,class (:foreground ,accent7 :background ,shade0))))
  `(term-color-cyan ((,class (:foreground ,accent4 :background ,shade0))))
  `(term-color-white ((,class (:foreground ,shade2 :background ,shade0))))
  `(rainbow-delimiters-unmatched-face ((,class :foreground ,accent0)))
  `(helm-header ((,class (:foreground ,shade6 :background ,shade0 :underline nil :box nil))))
  `(helm-source-header ((,class (:foreground ,accent1 :background ,shade0 :underline nil :weight bold))))
  `(helm-selection ((,class (:background ,shade1 :underline nil))))
  `(helm-selection-line ((,class (:background ,shade1))))
  `(helm-visible-mark ((,class (:foreground ,shade0 :background ,shade2))))
  `(helm-candidate-number ((,class (:foreground ,shade0 :background ,shade7))))
  `(helm-separator ((,class (:foreground ,accent5 :background ,shade0))))
  `(helm-time-zone-current ((,class (:foreground ,accent5 :background ,shade0))))
  `(helm-time-zone-home ((,class (:foreground ,accent5 :background ,shade0))))
  `(helm-buffer-not-saved ((,class (:foreground ,accent5 :background ,shade0))))
  `(helm-buffer-process ((,class (:foreground ,accent5 :background ,shade0))))
  `(helm-buffer-saved-out ((,class (:foreground ,shade7 :background ,shade0))))
  `(helm-buffer-size ((,class (:foreground ,shade7 :background ,shade0))))
  `(helm-ff-directory ((,class (:foreground ,accent6 :background ,shade0 :weight bold))))
  `(helm-ff-file ((,class (:foreground ,shade7 :background ,shade0 :weight normal))))
  `(helm-ff-executable ((,class (:foreground ,accent2 :background ,shade0 :weight normal))))
  `(helm-ff-invalid-symlink ((,class (:foreground ,accent7 :background ,shade0 :weight bold))))
  `(helm-ff-symlink ((,class (:foreground ,accent1 :background ,shade0 :weight bold))))
  `(helm-ff-prefix ((,class (:foreground ,shade0 :background ,accent1 :weight normal))))
  `(helm-grep-cmd-line ((,class (:foreground ,shade7 :background ,shade0))))
  `(helm-grep-file ((,class (:foreground ,shade7 :background ,shade0))))
  `(helm-grep-finish ((,class (:foreground ,shade6 :background ,shade0))))
  `(helm-grep-lineno ((,class (:foreground ,shade7 :background ,shade0))))
  `(helm-grep-match ((,class (:foreground nil :background nil :inherit helm-match))))
  `(helm-grep-running ((,class (:foreground ,accent6 :background ,shade0))))
  `(helm-moccur-buffer ((,class (:foreground ,accent6 :background ,shade0))))
  `(helm-source-go-package-godoc-description ((,class (:foreground ,accent3))))
  `(helm-bookmark-w3m ((,class (:foreground ,accent5))))
  `(company-echo-common ((,class (:foreground ,shade0 :background ,shade7))))
  `(company-preview ((,class (:background ,shade0 :foreground ,accent2))))
  `(company-preview-common ((,class (:foreground ,shade1 :foreground ,shade5))))
  `(company-preview-search ((,class (:foreground ,accent5 :background ,shade0))))
  `(company-scrollbar-bg ((,class (:background ,shade2))))
  `(company-scrollbar-fg ((,class (:foreground ,accent1))))
  `(company-tooltip ((,class (:foreground ,shade6 :background ,shade0 :bold t))))
  `(company-tooltop-annotation ((,class (:foreground ,accent4))))
  `(company-tooltip-common ((,class ( :foreground ,shade5))))
  `(company-tooltip-common-selection ((,class (:foreground ,accent3))))
  `(company-tooltip-mouse ((,class (:inherit highlight))))
  `(company-tooltip-selection ((,class (:background ,shade2 :foreground ,shade5))))
  `(company-template-field ((,class (:inherit region))))
  `(web-mode-builtin-face ((,class (:inherit ,font-lock-builtin-face))))
  `(web-mode-comment-face ((,class (:inherit ,font-lock-comment-face))))
  `(web-mode-constant-face ((,class (:inherit ,font-lock-constant-face))))
  `(web-mode-keyword-face ((,class (:foreground ,accent1))))
  `(web-mode-doctype-face ((,class (:inherit ,font-lock-comment-face))))
  `(web-mode-function-name-face ((,class (:inherit ,font-lock-function-name-face))))
  `(web-mode-string-face ((,class (:foreground ,accent3))))
  `(web-mode-type-face ((,class (:inherit ,font-lock-type-face))))
  `(web-mode-html-attr-name-face ((,class (:foreground ,accent6))))
  `(web-mode-html-attr-value-face ((,class (:foreground ,accent1))))
  `(web-mode-warning-face ((,class (:inherit ,font-lock-warning-face))))
  `(web-mode-html-tag-face ((,class (:foreground ,accent5))))
  `(jde-java-font-lock-package-face ((t (:foreground ,accent6))))
  `(jde-java-font-lock-public-face ((t (:foreground ,accent1))))
  `(jde-java-font-lock-private-face ((t (:foreground ,accent1))))
  `(jde-java-font-lock-constant-face ((t (:foreground ,accent4))))
  `(jde-java-font-lock-modifier-face ((t (:foreground ,accent7))))
  `(jde-jave-font-lock-protected-face ((t (:foreground ,accent1))))
  `(jde-java-font-lock-number-face ((t (:foreground ,accent6))))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
    (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'ghostfreak-green-light)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; ghostfreak-green-light-theme.el ends here
```

## lio-fotia-dark-theme.el

[Get the theme
here!](https://themer.dev/?colors.dark.shade0=%23A06088&colors.dark.shade7=%23C8E0A8&colors.light.shade0=%23C8E0A8&colors.light.shade7=%23A06088&activeColorSet=dark)

``` commonlisp
;;; lio-fotia-dark-theme.el

;; Version: 1.0.1
;; Package-Requires: ((emacs "24"))

;;; Commentary:

;; Generated by the emacs theme template for themer.

;;; Code:

(deftheme lio-fotia-dark)
(let ((class '((class color) (min-colors 89)))
  (shade0 "#a06088")
  (shade1 "#a6728d")
  (shade2 "#ab8591")
  (shade3 "#b19796")
  (shade4 "#b7a99a")
  (shade5 "#bdbb9f")
  (shade6 "#c2cea3")
  (shade7 "#c8e0a8")
  (accent0 "#c8e0a8")
  (accent1 "#c8e0a8")
  (accent2 "#c8e0a8")
  (accent3 "#c8e0a8")
  (accent4 "#c8e0a8")
  (accent5 "#c8e0a8")
  (accent6 "#c8e0a8")
  (accent7 "#c8e0a8"))
(custom-theme-set-faces
  'lio-fotia-dark
  `(default ((,class (:background ,shade0 :foreground ,shade7))))
  `(font-lock-builtin-face ((,class (:foreground ,accent5))))
  `(font-lock-comment-face ((,class (:foreground ,shade3))))
  `(font-lock-negation-char-face ((,class (:foreground ,accent4))))
  `(font-lock-reference-face ((,class (:foreground ,accent4))))
  `(font-lock-constant-face ((,class (:foreground ,accent4))))
  `(font-lock-doc-face ((,class (:foreground ,shade3))))
  `(font-lock-function-name-face ((,class (:foreground ,accent6 :bold t))))
  `(font-lock-keyword-face ((,class (:bold ,class :foreground ,accent1))))
  `(font-lock-string-face ((,class (:foreground ,accent3))))
  `(font-lock-type-face ((,class (:foreground ,accent5 ))))
  `(font-lock-variable-name-face ((,class (:foreground ,accent6))))
  `(font-lock-warning-face ((,class (:foreground ,accent0 :background ,shade1))))
  `(region ((,class (:background ,shade7 :foreground ,shade0))))
  `(highlight ((,class (:foreground ,shade5 :background ,shade2))))
  `(hl-line ((,class (:background  ,shade1))))
  `(fringe ((,class (:background ,shade1 :foreground ,shade4))))
  `(cursor ((,class (:background ,shade2))))
  `(show-paren-match-face ((,class (:background ,accent0))))
  `(isearch ((,class (:bold t :foreground ,accent0 :background ,shade2))))
  `(mode-line ((,class (:box (:line-width 1 :color nil) :bold t :foreground ,shade4 :background ,shade0))))
  `(mode-line-inactive ((,class (:box (:line-width 1 :color nil :style pressed-button) :foreground ,accent7 :background ,shade1 :weight normal))))
  `(mode-line-buffer-id ((,class (:bold t :foreground ,accent6 :background nil))))
  `(mode-line-highlight ((,class (:foreground ,accent1 :box nil :weight bold))))
  `(mode-line-emphasis ((,class (:foreground ,shade7))))
  `(vertical-border ((,class (:foreground ,shade5))))
  `(minibuffer-prompt ((,class (:bold t :foreground ,accent1))))
  `(default-italic ((,class (:italic t))))
  `(link ((,class (:foreground ,accent4 :underline t))))
  `(org-code ((,class (:foreground ,shade6))))
  `(org-hide ((,class (:foreground ,shade4))))
  `(org-level-1 ((,class (:bold t :foreground ,accent4 :height 1.1))))
  `(org-level-2 ((,class (:bold nil :foreground ,accent5))))
  `(org-level-3 ((,class (:bold t :foreground ,accent6))))
  `(org-level-4 ((,class (:bold nil :foreground ,accent7))))
  `(org-date ((,class (:underline t :foreground ,accent2) )))
  `(org-footnote  ((,class (:underline t :foreground ,shade4))))
  `(org-link ((,class (:underline t :foreground ,accent5 ))))
  `(org-special-keyword ((,class (:foreground ,accent1))))
  `(org-block ((,class (:foreground ,shade5))))
  `(org-quote ((,class (:inherit org-block :slant italic))))
  `(org-verse ((,class (:inherit org-block :slant italic))))
  `(org-todo ((,class (:box (:line-width 1 :color ,shade1) :foreground ,accent1 :bold t))))
  `(org-done ((,class (:box (:line-width 1 :color ,shade1) :foreground ,shade5 :bold t))))
  `(org-warning ((,class (:underline t :foreground ,accent2))))
  `(org-agenda-structure ((,class (:weight bold :foreground ,shade5 :box (:color ,shade4) :background ,shade2))))
  `(org-agenda-date ((,class (:foreground ,accent6 :height 1.1 ))))
  `(org-agenda-date-weekend ((,class (:weight normal :foreground ,shade4))))
  `(org-agenda-date-today ((,class (:weight bold :foreground ,accent1 :height 1.4))))
  `(org-agenda-done ((,class (:foreground ,shade3))))
  `(org-scheduled ((,class (:foreground ,accent5))))
  `(org-scheduled-today ((,class (:foreground ,accent6 :weight bold :height 1.2))))
  `(org-ellipsis ((,class (:foreground ,accent5))))
  `(org-verbatim ((,class (:foreground ,shade4))))
  `(org-document-info-keyword ((,class (:foreground ,accent6))))
  `(font-latex-bold-face ((,class (:foreground ,accent5))))
  `(font-latex-italic-face ((,class (:foreground ,accent7 :italic t))))
  `(font-latex-string-face ((,class (:foreground ,accent3))))
  `(font-latex-match-reference-keywords ((,class (:foreground ,accent4))))
  `(font-latex-match-variable-keywords ((,class (:foreground ,accent6))))
  `(ido-only-match ((,class (:foreground ,accent0))))
  `(org-sexp-date ((,class (:foreground ,shade4))))
  `(ido-first-match ((,class (:foreground ,accent1 :bold t))))
  `(gnus-header-content ((,class (:foreground ,accent1))))
  `(gnus-header-from ((,class (:foreground ,accent6))))
  `(gnus-header-name ((,class (:foreground ,accent5))))
  `(gnus-header-subject ((,class (:foreground ,accent6 :bold t))))
  `(mu4e-view-url-number-face ((,class (:foreground ,accent5))))
  `(mu4e-cited-1-face ((,class (:foreground ,shade6))))
  `(mu4e-cited-7-face ((,class (:foreground ,shade5))))
  `(mu4e-header-marks-face ((,class (:foreground ,accent5))))
  `(ffap ((,class (:foreground ,shade4))))
  `(js2-private-function-call ((,class (:foreground ,accent4))))
  `(js2-jsdoc-html-tag-delimiter ((,class (:foreground ,accent3))))
  `(js2-jsdoc-html-tag-name ((,class (:foreground ,accent2))))
  `(js2-external-variable ((,class (:foreground ,accent5  ))))
  `(js2-function-param ((,class (:foreground ,accent4))))
  `(js2-jsdoc-value ((,class (:foreground ,accent3))))
  `(js2-private-member ((,class (:foreground ,shade5))))
  `(js3-warning-face ((,class (:underline ,accent1))))
  `(js3-error-face ((,class (:underline ,accent0))))
  `(js3-external-variable-face ((,class (:foreground ,accent6))))
  `(js3-function-param-face ((,class (:foreground ,accent7))))
  `(js3-jsdoc-tag-face ((,class (:foreground ,accent1))))
  `(js3-instance-member-face ((,class (:foreground ,accent4))))
  `(warning ((,class (:foreground ,accent0))))
  `(ac-completion-face ((,class (:underline t :foreground ,accent1))))
  `(info-quoted-name ((,class (:foreground ,accent5))))
  `(info-string ((,class (:foreground ,accent3))))
  `(icompletep-determined ((,class :foreground ,accent5)))
  `(undo-tree-visualizer-current-face ((,class :foreground ,accent5)))
  `(undo-tree-visualizer-default-face ((,class :foreground ,shade6)))
  `(undo-tree-visualizer-unmodified-face ((,class :foreground ,accent6)))
  `(undo-tree-visualizer-register-face ((,class :foreground ,accent5)))
  `(slime-repl-inputed-output-face ((,class (:foreground ,accent5))))
  `(trailing-whitespace ((,class :foreground nil :background ,accent0)))
  `(rainbow-delimiters-depth-1-face ((,class :foreground ,shade7)))
  `(rainbow-delimiters-depth-2-face ((,class :foreground ,accent5)))
  `(rainbow-delimiters-depth-3-face ((,class :foreground ,accent6)))
  `(rainbow-delimiters-depth-4-face ((,class :foreground ,accent4)))
  `(rainbow-delimiters-depth-5-face ((,class :foreground ,accent1)))
  `(rainbow-delimiters-depth-6-face ((,class :foreground ,shade5)))
  `(rainbow-delimiters-depth-7-face ((,class :foreground ,accent5)))
  `(rainbow-delimiters-depth-8-face ((,class :foreground ,accent3)))
  `(magit-item-highlight ((,class :background ,shade2)))
  `(magit-section-heading        ((,class (:foreground ,accent1 :weight bold))))
  `(magit-hunk-heading           ((,class (:background ,shade2))))
  `(magit-section-highlight      ((,class (:background ,shade1))))
  `(magit-hunk-heading-highlight ((,class (:background ,shade2))))
  `(magit-diff-context-highlight ((,class (:background ,shade2 :foreground ,shade5))))
  `(magit-diffstat-added   ((,class (:foreground ,accent5))))
  `(magit-diffstat-removed ((,class (:foreground ,accent6))))
  `(magit-process-ok ((,class (:foreground ,accent6 :weight bold))))
  `(magit-process-ng ((,class (:foreground ,accent0 :weight bold))))
  `(magit-branch ((,class (:foreground ,accent4 :weight bold))))
  `(magit-log-author ((,class (:foreground ,shade5))))
  `(magit-hash ((,class (:foreground ,shade6))))
  `(magit-diff-file-header ((,class (:foreground ,shade6 :background ,shade2))))
  `(lazy-highlight ((,class (:foreground ,shade6 :background ,shade2))))
  `(term ((,class (:foreground ,shade6 :background ,shade0))))
  `(term-color-black ((,class (:foreground ,shade6 :background ,shade0))))
  `(term-color-blue ((,class (:foreground ,accent5 :background ,shade0))))
  `(term-color-red ((,class (:foreground ,accent0 :background ,shade0))))
  `(term-color-green ((,class (:foreground ,accent3 :background ,shade0))))
  `(term-color-yellow ((,class (:foreground ,accent2 :background ,shade0))))
  `(term-color-magenta ((,class (:foreground ,accent7 :background ,shade0))))
  `(term-color-cyan ((,class (:foreground ,accent4 :background ,shade0))))
  `(term-color-white ((,class (:foreground ,shade2 :background ,shade0))))
  `(rainbow-delimiters-unmatched-face ((,class :foreground ,accent0)))
  `(helm-header ((,class (:foreground ,shade6 :background ,shade0 :underline nil :box nil))))
  `(helm-source-header ((,class (:foreground ,accent1 :background ,shade0 :underline nil :weight bold))))
  `(helm-selection ((,class (:background ,shade1 :underline nil))))
  `(helm-selection-line ((,class (:background ,shade1))))
  `(helm-visible-mark ((,class (:foreground ,shade0 :background ,shade2))))
  `(helm-candidate-number ((,class (:foreground ,shade0 :background ,shade7))))
  `(helm-separator ((,class (:foreground ,accent5 :background ,shade0))))
  `(helm-time-zone-current ((,class (:foreground ,accent5 :background ,shade0))))
  `(helm-time-zone-home ((,class (:foreground ,accent5 :background ,shade0))))
  `(helm-buffer-not-saved ((,class (:foreground ,accent5 :background ,shade0))))
  `(helm-buffer-process ((,class (:foreground ,accent5 :background ,shade0))))
  `(helm-buffer-saved-out ((,class (:foreground ,shade7 :background ,shade0))))
  `(helm-buffer-size ((,class (:foreground ,shade7 :background ,shade0))))
  `(helm-ff-directory ((,class (:foreground ,accent6 :background ,shade0 :weight bold))))
  `(helm-ff-file ((,class (:foreground ,shade7 :background ,shade0 :weight normal))))
  `(helm-ff-executable ((,class (:foreground ,accent2 :background ,shade0 :weight normal))))
  `(helm-ff-invalid-symlink ((,class (:foreground ,accent7 :background ,shade0 :weight bold))))
  `(helm-ff-symlink ((,class (:foreground ,accent1 :background ,shade0 :weight bold))))
  `(helm-ff-prefix ((,class (:foreground ,shade0 :background ,accent1 :weight normal))))
  `(helm-grep-cmd-line ((,class (:foreground ,shade7 :background ,shade0))))
  `(helm-grep-file ((,class (:foreground ,shade7 :background ,shade0))))
  `(helm-grep-finish ((,class (:foreground ,shade6 :background ,shade0))))
  `(helm-grep-lineno ((,class (:foreground ,shade7 :background ,shade0))))
  `(helm-grep-match ((,class (:foreground nil :background nil :inherit helm-match))))
  `(helm-grep-running ((,class (:foreground ,accent6 :background ,shade0))))
  `(helm-moccur-buffer ((,class (:foreground ,accent6 :background ,shade0))))
  `(helm-source-go-package-godoc-description ((,class (:foreground ,accent3))))
  `(helm-bookmark-w3m ((,class (:foreground ,accent5))))
  `(company-echo-common ((,class (:foreground ,shade0 :background ,shade7))))
  `(company-preview ((,class (:background ,shade0 :foreground ,accent2))))
  `(company-preview-common ((,class (:foreground ,shade1 :foreground ,shade5))))
  `(company-preview-search ((,class (:foreground ,accent5 :background ,shade0))))
  `(company-scrollbar-bg ((,class (:background ,shade2))))
  `(company-scrollbar-fg ((,class (:foreground ,accent1))))
  `(company-tooltip ((,class (:foreground ,shade6 :background ,shade0 :bold t))))
  `(company-tooltop-annotation ((,class (:foreground ,accent4))))
  `(company-tooltip-common ((,class ( :foreground ,shade5))))
  `(company-tooltip-common-selection ((,class (:foreground ,accent3))))
  `(company-tooltip-mouse ((,class (:inherit highlight))))
  `(company-tooltip-selection ((,class (:background ,shade2 :foreground ,shade5))))
  `(company-template-field ((,class (:inherit region))))
  `(web-mode-builtin-face ((,class (:inherit ,font-lock-builtin-face))))
  `(web-mode-comment-face ((,class (:inherit ,font-lock-comment-face))))
  `(web-mode-constant-face ((,class (:inherit ,font-lock-constant-face))))
  `(web-mode-keyword-face ((,class (:foreground ,accent1))))
  `(web-mode-doctype-face ((,class (:inherit ,font-lock-comment-face))))
  `(web-mode-function-name-face ((,class (:inherit ,font-lock-function-name-face))))
  `(web-mode-string-face ((,class (:foreground ,accent3))))
  `(web-mode-type-face ((,class (:inherit ,font-lock-type-face))))
  `(web-mode-html-attr-name-face ((,class (:foreground ,accent6))))
  `(web-mode-html-attr-value-face ((,class (:foreground ,accent1))))
  `(web-mode-warning-face ((,class (:inherit ,font-lock-warning-face))))
  `(web-mode-html-tag-face ((,class (:foreground ,accent5))))
  `(jde-java-font-lock-package-face ((t (:foreground ,accent6))))
  `(jde-java-font-lock-public-face ((t (:foreground ,accent1))))
  `(jde-java-font-lock-private-face ((t (:foreground ,accent1))))
  `(jde-java-font-lock-constant-face ((t (:foreground ,accent4))))
  `(jde-java-font-lock-modifier-face ((t (:foreground ,accent7))))
  `(jde-jave-font-lock-protected-face ((t (:foreground ,accent1))))
  `(jde-java-font-lock-number-face ((t (:foreground ,accent6))))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
    (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'lio-fotia-dark)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; lio-fotia-dark-theme.el ends here
```

## lio-fotia-light-theme.el

[Get the theme
here!](https://themer.dev/?colors.dark.shade0=%23A06088&colors.dark.shade7=%23C8E0A8&colors.light.shade0=%23C8E0A8&colors.light.shade7=%23A06088&activeColorSet=dark)

``` commonlisp
;;; lio-fotia-light-theme.el

;; Version: 1.0.1
;; Package-Requires: ((emacs "24"))

;;; Commentary:

;; Generated by the emacs theme template for themer.

;;; Code:

(deftheme lio-fotia-light)
(let ((class '((class color) (min-colors 89)))
  (shade0 "#c8e0a8")
  (shade1 "#c2cea3")
  (shade2 "#bdbb9f")
  (shade3 "#b7a99a")
  (shade4 "#b19796")
  (shade5 "#ab8591")
  (shade6 "#a6728d")
  (shade7 "#a06088")
  (accent0 "#a06088")
  (accent1 "#a06088")
  (accent2 "#a06088")
  (accent3 "#a06088")
  (accent4 "#a06088")
  (accent5 "#a06088")
  (accent6 "#a06088")
  (accent7 "#a06088"))
(custom-theme-set-faces
  'lio-fotia-light
  `(default ((,class (:background ,shade0 :foreground ,shade7))))
  `(font-lock-builtin-face ((,class (:foreground ,accent5))))
  `(font-lock-comment-face ((,class (:foreground ,shade3))))
  `(font-lock-negation-char-face ((,class (:foreground ,accent4))))
  `(font-lock-reference-face ((,class (:foreground ,accent4))))
  `(font-lock-constant-face ((,class (:foreground ,accent4))))
  `(font-lock-doc-face ((,class (:foreground ,shade3))))
  `(font-lock-function-name-face ((,class (:foreground ,accent6 :bold t))))
  `(font-lock-keyword-face ((,class (:bold ,class :foreground ,accent1))))
  `(font-lock-string-face ((,class (:foreground ,accent3))))
  `(font-lock-type-face ((,class (:foreground ,accent5 ))))
  `(font-lock-variable-name-face ((,class (:foreground ,accent6))))
  `(font-lock-warning-face ((,class (:foreground ,accent0 :background ,shade1))))
  `(region ((,class (:background ,shade7 :foreground ,shade0))))
  `(highlight ((,class (:foreground ,shade5 :background ,shade2))))
  `(hl-line ((,class (:background  ,shade1))))
  `(fringe ((,class (:background ,shade1 :foreground ,shade4))))
  `(cursor ((,class (:background ,shade2))))
  `(show-paren-match-face ((,class (:background ,accent0))))
  `(isearch ((,class (:bold t :foreground ,accent0 :background ,shade2))))
  `(mode-line ((,class (:box (:line-width 1 :color nil) :bold t :foreground ,shade4 :background ,shade0))))
  `(mode-line-inactive ((,class (:box (:line-width 1 :color nil :style pressed-button) :foreground ,accent7 :background ,shade1 :weight normal))))
  `(mode-line-buffer-id ((,class (:bold t :foreground ,accent6 :background nil))))
  `(mode-line-highlight ((,class (:foreground ,accent1 :box nil :weight bold))))
  `(mode-line-emphasis ((,class (:foreground ,shade7))))
  `(vertical-border ((,class (:foreground ,shade5))))
  `(minibuffer-prompt ((,class (:bold t :foreground ,accent1))))
  `(default-italic ((,class (:italic t))))
  `(link ((,class (:foreground ,accent4 :underline t))))
  `(org-code ((,class (:foreground ,shade6))))
  `(org-hide ((,class (:foreground ,shade4))))
  `(org-level-1 ((,class (:bold t :foreground ,accent4 :height 1.1))))
  `(org-level-2 ((,class (:bold nil :foreground ,accent5))))
  `(org-level-3 ((,class (:bold t :foreground ,accent6))))
  `(org-level-4 ((,class (:bold nil :foreground ,accent7))))
  `(org-date ((,class (:underline t :foreground ,accent2) )))
  `(org-footnote  ((,class (:underline t :foreground ,shade4))))
  `(org-link ((,class (:underline t :foreground ,accent5 ))))
  `(org-special-keyword ((,class (:foreground ,accent1))))
  `(org-block ((,class (:foreground ,shade5))))
  `(org-quote ((,class (:inherit org-block :slant italic))))
  `(org-verse ((,class (:inherit org-block :slant italic))))
  `(org-todo ((,class (:box (:line-width 1 :color ,shade1) :foreground ,accent1 :bold t))))
  `(org-done ((,class (:box (:line-width 1 :color ,shade1) :foreground ,shade5 :bold t))))
  `(org-warning ((,class (:underline t :foreground ,accent2))))
  `(org-agenda-structure ((,class (:weight bold :foreground ,shade5 :box (:color ,shade4) :background ,shade2))))
  `(org-agenda-date ((,class (:foreground ,accent6 :height 1.1 ))))
  `(org-agenda-date-weekend ((,class (:weight normal :foreground ,shade4))))
  `(org-agenda-date-today ((,class (:weight bold :foreground ,accent1 :height 1.4))))
  `(org-agenda-done ((,class (:foreground ,shade3))))
  `(org-scheduled ((,class (:foreground ,accent5))))
  `(org-scheduled-today ((,class (:foreground ,accent6 :weight bold :height 1.2))))
  `(org-ellipsis ((,class (:foreground ,accent5))))
  `(org-verbatim ((,class (:foreground ,shade4))))
  `(org-document-info-keyword ((,class (:foreground ,accent6))))
  `(font-latex-bold-face ((,class (:foreground ,accent5))))
  `(font-latex-italic-face ((,class (:foreground ,accent7 :italic t))))
  `(font-latex-string-face ((,class (:foreground ,accent3))))
  `(font-latex-match-reference-keywords ((,class (:foreground ,accent4))))
  `(font-latex-match-variable-keywords ((,class (:foreground ,accent6))))
  `(ido-only-match ((,class (:foreground ,accent0))))
  `(org-sexp-date ((,class (:foreground ,shade4))))
  `(ido-first-match ((,class (:foreground ,accent1 :bold t))))
  `(gnus-header-content ((,class (:foreground ,accent1))))
  `(gnus-header-from ((,class (:foreground ,accent6))))
  `(gnus-header-name ((,class (:foreground ,accent5))))
  `(gnus-header-subject ((,class (:foreground ,accent6 :bold t))))
  `(mu4e-view-url-number-face ((,class (:foreground ,accent5))))
  `(mu4e-cited-1-face ((,class (:foreground ,shade6))))
  `(mu4e-cited-7-face ((,class (:foreground ,shade5))))
  `(mu4e-header-marks-face ((,class (:foreground ,accent5))))
  `(ffap ((,class (:foreground ,shade4))))
  `(js2-private-function-call ((,class (:foreground ,accent4))))
  `(js2-jsdoc-html-tag-delimiter ((,class (:foreground ,accent3))))
  `(js2-jsdoc-html-tag-name ((,class (:foreground ,accent2))))
  `(js2-external-variable ((,class (:foreground ,accent5  ))))
  `(js2-function-param ((,class (:foreground ,accent4))))
  `(js2-jsdoc-value ((,class (:foreground ,accent3))))
  `(js2-private-member ((,class (:foreground ,shade5))))
  `(js3-warning-face ((,class (:underline ,accent1))))
  `(js3-error-face ((,class (:underline ,accent0))))
  `(js3-external-variable-face ((,class (:foreground ,accent6))))
  `(js3-function-param-face ((,class (:foreground ,accent7))))
  `(js3-jsdoc-tag-face ((,class (:foreground ,accent1))))
  `(js3-instance-member-face ((,class (:foreground ,accent4))))
  `(warning ((,class (:foreground ,accent0))))
  `(ac-completion-face ((,class (:underline t :foreground ,accent1))))
  `(info-quoted-name ((,class (:foreground ,accent5))))
  `(info-string ((,class (:foreground ,accent3))))
  `(icompletep-determined ((,class :foreground ,accent5)))
  `(undo-tree-visualizer-current-face ((,class :foreground ,accent5)))
  `(undo-tree-visualizer-default-face ((,class :foreground ,shade6)))
  `(undo-tree-visualizer-unmodified-face ((,class :foreground ,accent6)))
  `(undo-tree-visualizer-register-face ((,class :foreground ,accent5)))
  `(slime-repl-inputed-output-face ((,class (:foreground ,accent5))))
  `(trailing-whitespace ((,class :foreground nil :background ,accent0)))
  `(rainbow-delimiters-depth-1-face ((,class :foreground ,shade7)))
  `(rainbow-delimiters-depth-2-face ((,class :foreground ,accent5)))
  `(rainbow-delimiters-depth-3-face ((,class :foreground ,accent6)))
  `(rainbow-delimiters-depth-4-face ((,class :foreground ,accent4)))
  `(rainbow-delimiters-depth-5-face ((,class :foreground ,accent1)))
  `(rainbow-delimiters-depth-6-face ((,class :foreground ,shade5)))
  `(rainbow-delimiters-depth-7-face ((,class :foreground ,accent5)))
  `(rainbow-delimiters-depth-8-face ((,class :foreground ,accent3)))
  `(magit-item-highlight ((,class :background ,shade2)))
  `(magit-section-heading        ((,class (:foreground ,accent1 :weight bold))))
  `(magit-hunk-heading           ((,class (:background ,shade2))))
  `(magit-section-highlight      ((,class (:background ,shade1))))
  `(magit-hunk-heading-highlight ((,class (:background ,shade2))))
  `(magit-diff-context-highlight ((,class (:background ,shade2 :foreground ,shade5))))
  `(magit-diffstat-added   ((,class (:foreground ,accent5))))
  `(magit-diffstat-removed ((,class (:foreground ,accent6))))
  `(magit-process-ok ((,class (:foreground ,accent6 :weight bold))))
  `(magit-process-ng ((,class (:foreground ,accent0 :weight bold))))
  `(magit-branch ((,class (:foreground ,accent4 :weight bold))))
  `(magit-log-author ((,class (:foreground ,shade5))))
  `(magit-hash ((,class (:foreground ,shade6))))
  `(magit-diff-file-header ((,class (:foreground ,shade6 :background ,shade2))))
  `(lazy-highlight ((,class (:foreground ,shade6 :background ,shade2))))
  `(term ((,class (:foreground ,shade6 :background ,shade0))))
  `(term-color-black ((,class (:foreground ,shade6 :background ,shade0))))
  `(term-color-blue ((,class (:foreground ,accent5 :background ,shade0))))
  `(term-color-red ((,class (:foreground ,accent0 :background ,shade0))))
  `(term-color-green ((,class (:foreground ,accent3 :background ,shade0))))
  `(term-color-yellow ((,class (:foreground ,accent2 :background ,shade0))))
  `(term-color-magenta ((,class (:foreground ,accent7 :background ,shade0))))
  `(term-color-cyan ((,class (:foreground ,accent4 :background ,shade0))))
  `(term-color-white ((,class (:foreground ,shade2 :background ,shade0))))
  `(rainbow-delimiters-unmatched-face ((,class :foreground ,accent0)))
  `(helm-header ((,class (:foreground ,shade6 :background ,shade0 :underline nil :box nil))))
  `(helm-source-header ((,class (:foreground ,accent1 :background ,shade0 :underline nil :weight bold))))
  `(helm-selection ((,class (:background ,shade1 :underline nil))))
  `(helm-selection-line ((,class (:background ,shade1))))
  `(helm-visible-mark ((,class (:foreground ,shade0 :background ,shade2))))
  `(helm-candidate-number ((,class (:foreground ,shade0 :background ,shade7))))
  `(helm-separator ((,class (:foreground ,accent5 :background ,shade0))))
  `(helm-time-zone-current ((,class (:foreground ,accent5 :background ,shade0))))
  `(helm-time-zone-home ((,class (:foreground ,accent5 :background ,shade0))))
  `(helm-buffer-not-saved ((,class (:foreground ,accent5 :background ,shade0))))
  `(helm-buffer-process ((,class (:foreground ,accent5 :background ,shade0))))
  `(helm-buffer-saved-out ((,class (:foreground ,shade7 :background ,shade0))))
  `(helm-buffer-size ((,class (:foreground ,shade7 :background ,shade0))))
  `(helm-ff-directory ((,class (:foreground ,accent6 :background ,shade0 :weight bold))))
  `(helm-ff-file ((,class (:foreground ,shade7 :background ,shade0 :weight normal))))
  `(helm-ff-executable ((,class (:foreground ,accent2 :background ,shade0 :weight normal))))
  `(helm-ff-invalid-symlink ((,class (:foreground ,accent7 :background ,shade0 :weight bold))))
  `(helm-ff-symlink ((,class (:foreground ,accent1 :background ,shade0 :weight bold))))
  `(helm-ff-prefix ((,class (:foreground ,shade0 :background ,accent1 :weight normal))))
  `(helm-grep-cmd-line ((,class (:foreground ,shade7 :background ,shade0))))
  `(helm-grep-file ((,class (:foreground ,shade7 :background ,shade0))))
  `(helm-grep-finish ((,class (:foreground ,shade6 :background ,shade0))))
  `(helm-grep-lineno ((,class (:foreground ,shade7 :background ,shade0))))
  `(helm-grep-match ((,class (:foreground nil :background nil :inherit helm-match))))
  `(helm-grep-running ((,class (:foreground ,accent6 :background ,shade0))))
  `(helm-moccur-buffer ((,class (:foreground ,accent6 :background ,shade0))))
  `(helm-source-go-package-godoc-description ((,class (:foreground ,accent3))))
  `(helm-bookmark-w3m ((,class (:foreground ,accent5))))
  `(company-echo-common ((,class (:foreground ,shade0 :background ,shade7))))
  `(company-preview ((,class (:background ,shade0 :foreground ,accent2))))
  `(company-preview-common ((,class (:foreground ,shade1 :foreground ,shade5))))
  `(company-preview-search ((,class (:foreground ,accent5 :background ,shade0))))
  `(company-scrollbar-bg ((,class (:background ,shade2))))
  `(company-scrollbar-fg ((,class (:foreground ,accent1))))
  `(company-tooltip ((,class (:foreground ,shade6 :background ,shade0 :bold t))))
  `(company-tooltop-annotation ((,class (:foreground ,accent4))))
  `(company-tooltip-common ((,class ( :foreground ,shade5))))
  `(company-tooltip-common-selection ((,class (:foreground ,accent3))))
  `(company-tooltip-mouse ((,class (:inherit highlight))))
  `(company-tooltip-selection ((,class (:background ,shade2 :foreground ,shade5))))
  `(company-template-field ((,class (:inherit region))))
  `(web-mode-builtin-face ((,class (:inherit ,font-lock-builtin-face))))
  `(web-mode-comment-face ((,class (:inherit ,font-lock-comment-face))))
  `(web-mode-constant-face ((,class (:inherit ,font-lock-constant-face))))
  `(web-mode-keyword-face ((,class (:foreground ,accent1))))
  `(web-mode-doctype-face ((,class (:inherit ,font-lock-comment-face))))
  `(web-mode-function-name-face ((,class (:inherit ,font-lock-function-name-face))))
  `(web-mode-string-face ((,class (:foreground ,accent3))))
  `(web-mode-type-face ((,class (:inherit ,font-lock-type-face))))
  `(web-mode-html-attr-name-face ((,class (:foreground ,accent6))))
  `(web-mode-html-attr-value-face ((,class (:foreground ,accent1))))
  `(web-mode-warning-face ((,class (:inherit ,font-lock-warning-face))))
  `(web-mode-html-tag-face ((,class (:foreground ,accent5))))
  `(jde-java-font-lock-package-face ((t (:foreground ,accent6))))
  `(jde-java-font-lock-public-face ((t (:foreground ,accent1))))
  `(jde-java-font-lock-private-face ((t (:foreground ,accent1))))
  `(jde-java-font-lock-constant-face ((t (:foreground ,accent4))))
  `(jde-java-font-lock-modifier-face ((t (:foreground ,accent7))))
  `(jde-jave-font-lock-protected-face ((t (:foreground ,accent1))))
  `(jde-java-font-lock-number-face ((t (:foreground ,accent6))))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
    (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'lio-fotia-light)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; lio-fotia-light-theme.el ends here
```

## flamingo-pink-purple-dark-theme.el

[Get the theme
here!](https://themer.dev/?colors.dark.shade0=%23ab5dee&colors.dark.shade7=%23fca78e&colors.light.shade0=%23fca78e&colors.light.shade7=%23ab5dee&activeColorSet=dark)

``` commonlisp
;;; flamingo-pink-purple-dark-theme.el

;; Version: 1.0.1
;; Package-Requires: ((emacs "24"))

;;; Commentary:

;; Generated by the emacs theme template for themer.

;;; Code:

(deftheme flamingo-pink-purple-dark)
(let ((class '((class color) (min-colors 89)))
  (shade0 "#ab5dee")
  (shade1 "#b768e0")
  (shade2 "#c272d3")
  (shade3 "#ce7dc5")
  (shade4 "#d987b7")
  (shade5 "#e592a9")
  (shade6 "#f09c9c")
  (shade7 "#fca78e")
  (accent0 "#fca78e")
  (accent1 "#fca78e")
  (accent2 "#fca78e")
  (accent3 "#fca78e")
  (accent4 "#fca78e")
  (accent5 "#fca78e")
  (accent6 "#fca78e")
  (accent7 "#fca78e"))
(custom-theme-set-faces
  'flamingo-pink-purple-dark
  `(default ((,class (:background ,shade0 :foreground ,shade7))))
  `(font-lock-builtin-face ((,class (:foreground ,accent5))))
  `(font-lock-comment-face ((,class (:foreground ,shade3))))
  `(font-lock-negation-char-face ((,class (:foreground ,accent4))))
  `(font-lock-reference-face ((,class (:foreground ,accent4))))
  `(font-lock-constant-face ((,class (:foreground ,accent4))))
  `(font-lock-doc-face ((,class (:foreground ,shade3))))
  `(font-lock-function-name-face ((,class (:foreground ,accent6 :bold t))))
  `(font-lock-keyword-face ((,class (:bold ,class :foreground ,accent1))))
  `(font-lock-string-face ((,class (:foreground ,accent3))))
  `(font-lock-type-face ((,class (:foreground ,accent5 ))))
  `(font-lock-variable-name-face ((,class (:foreground ,accent6))))
  `(font-lock-warning-face ((,class (:foreground ,accent0 :background ,shade1))))
  `(region ((,class (:background ,shade7 :foreground ,shade0))))
  `(highlight ((,class (:foreground ,shade5 :background ,shade2))))
  `(hl-line ((,class (:background  ,shade1))))
  `(fringe ((,class (:background ,shade1 :foreground ,shade4))))
  `(cursor ((,class (:background ,shade2))))
  `(show-paren-match-face ((,class (:background ,accent0))))
  `(isearch ((,class (:bold t :foreground ,accent0 :background ,shade2))))
  `(mode-line ((,class (:box (:line-width 1 :color nil) :bold t :foreground ,shade4 :background ,shade0))))
  `(mode-line-inactive ((,class (:box (:line-width 1 :color nil :style pressed-button) :foreground ,accent7 :background ,shade1 :weight normal))))
  `(mode-line-buffer-id ((,class (:bold t :foreground ,accent6 :background nil))))
  `(mode-line-highlight ((,class (:foreground ,accent1 :box nil :weight bold))))
  `(mode-line-emphasis ((,class (:foreground ,shade7))))
  `(vertical-border ((,class (:foreground ,shade5))))
  `(minibuffer-prompt ((,class (:bold t :foreground ,accent1))))
  `(default-italic ((,class (:italic t))))
  `(link ((,class (:foreground ,accent4 :underline t))))
  `(org-code ((,class (:foreground ,shade6))))
  `(org-hide ((,class (:foreground ,shade4))))
  `(org-level-1 ((,class (:bold t :foreground ,accent4 :height 1.1))))
  `(org-level-2 ((,class (:bold nil :foreground ,accent5))))
  `(org-level-3 ((,class (:bold t :foreground ,accent6))))
  `(org-level-4 ((,class (:bold nil :foreground ,accent7))))
  `(org-date ((,class (:underline t :foreground ,accent2) )))
  `(org-footnote  ((,class (:underline t :foreground ,shade4))))
  `(org-link ((,class (:underline t :foreground ,accent5 ))))
  `(org-special-keyword ((,class (:foreground ,accent1))))
  `(org-block ((,class (:foreground ,shade5))))
  `(org-quote ((,class (:inherit org-block :slant italic))))
  `(org-verse ((,class (:inherit org-block :slant italic))))
  `(org-todo ((,class (:box (:line-width 1 :color ,shade1) :foreground ,accent1 :bold t))))
  `(org-done ((,class (:box (:line-width 1 :color ,shade1) :foreground ,shade5 :bold t))))
  `(org-warning ((,class (:underline t :foreground ,accent2))))
  `(org-agenda-structure ((,class (:weight bold :foreground ,shade5 :box (:color ,shade4) :background ,shade2))))
  `(org-agenda-date ((,class (:foreground ,accent6 :height 1.1 ))))
  `(org-agenda-date-weekend ((,class (:weight normal :foreground ,shade4))))
  `(org-agenda-date-today ((,class (:weight bold :foreground ,accent1 :height 1.4))))
  `(org-agenda-done ((,class (:foreground ,shade3))))
  `(org-scheduled ((,class (:foreground ,accent5))))
  `(org-scheduled-today ((,class (:foreground ,accent6 :weight bold :height 1.2))))
  `(org-ellipsis ((,class (:foreground ,accent5))))
  `(org-verbatim ((,class (:foreground ,shade4))))
  `(org-document-info-keyword ((,class (:foreground ,accent6))))
  `(font-latex-bold-face ((,class (:foreground ,accent5))))
  `(font-latex-italic-face ((,class (:foreground ,accent7 :italic t))))
  `(font-latex-string-face ((,class (:foreground ,accent3))))
  `(font-latex-match-reference-keywords ((,class (:foreground ,accent4))))
  `(font-latex-match-variable-keywords ((,class (:foreground ,accent6))))
  `(ido-only-match ((,class (:foreground ,accent0))))
  `(org-sexp-date ((,class (:foreground ,shade4))))
  `(ido-first-match ((,class (:foreground ,accent1 :bold t))))
  `(gnus-header-content ((,class (:foreground ,accent1))))
  `(gnus-header-from ((,class (:foreground ,accent6))))
  `(gnus-header-name ((,class (:foreground ,accent5))))
  `(gnus-header-subject ((,class (:foreground ,accent6 :bold t))))
  `(mu4e-view-url-number-face ((,class (:foreground ,accent5))))
  `(mu4e-cited-1-face ((,class (:foreground ,shade6))))
  `(mu4e-cited-7-face ((,class (:foreground ,shade5))))
  `(mu4e-header-marks-face ((,class (:foreground ,accent5))))
  `(ffap ((,class (:foreground ,shade4))))
  `(js2-private-function-call ((,class (:foreground ,accent4))))
  `(js2-jsdoc-html-tag-delimiter ((,class (:foreground ,accent3))))
  `(js2-jsdoc-html-tag-name ((,class (:foreground ,accent2))))
  `(js2-external-variable ((,class (:foreground ,accent5  ))))
  `(js2-function-param ((,class (:foreground ,accent4))))
  `(js2-jsdoc-value ((,class (:foreground ,accent3))))
  `(js2-private-member ((,class (:foreground ,shade5))))
  `(js3-warning-face ((,class (:underline ,accent1))))
  `(js3-error-face ((,class (:underline ,accent0))))
  `(js3-external-variable-face ((,class (:foreground ,accent6))))
  `(js3-function-param-face ((,class (:foreground ,accent7))))
  `(js3-jsdoc-tag-face ((,class (:foreground ,accent1))))
  `(js3-instance-member-face ((,class (:foreground ,accent4))))
  `(warning ((,class (:foreground ,accent0))))
  `(ac-completion-face ((,class (:underline t :foreground ,accent1))))
  `(info-quoted-name ((,class (:foreground ,accent5))))
  `(info-string ((,class (:foreground ,accent3))))
  `(icompletep-determined ((,class :foreground ,accent5)))
  `(undo-tree-visualizer-current-face ((,class :foreground ,accent5)))
  `(undo-tree-visualizer-default-face ((,class :foreground ,shade6)))
  `(undo-tree-visualizer-unmodified-face ((,class :foreground ,accent6)))
  `(undo-tree-visualizer-register-face ((,class :foreground ,accent5)))
  `(slime-repl-inputed-output-face ((,class (:foreground ,accent5))))
  `(trailing-whitespace ((,class :foreground nil :background ,accent0)))
  `(rainbow-delimiters-depth-1-face ((,class :foreground ,shade7)))
  `(rainbow-delimiters-depth-2-face ((,class :foreground ,accent5)))
  `(rainbow-delimiters-depth-3-face ((,class :foreground ,accent6)))
  `(rainbow-delimiters-depth-4-face ((,class :foreground ,accent4)))
  `(rainbow-delimiters-depth-5-face ((,class :foreground ,accent1)))
  `(rainbow-delimiters-depth-6-face ((,class :foreground ,shade5)))
  `(rainbow-delimiters-depth-7-face ((,class :foreground ,accent5)))
  `(rainbow-delimiters-depth-8-face ((,class :foreground ,accent3)))
  `(magit-item-highlight ((,class :background ,shade2)))
  `(magit-section-heading        ((,class (:foreground ,accent1 :weight bold))))
  `(magit-hunk-heading           ((,class (:background ,shade2))))
  `(magit-section-highlight      ((,class (:background ,shade1))))
  `(magit-hunk-heading-highlight ((,class (:background ,shade2))))
  `(magit-diff-context-highlight ((,class (:background ,shade2 :foreground ,shade5))))
  `(magit-diffstat-added   ((,class (:foreground ,accent5))))
  `(magit-diffstat-removed ((,class (:foreground ,accent6))))
  `(magit-process-ok ((,class (:foreground ,accent6 :weight bold))))
  `(magit-process-ng ((,class (:foreground ,accent0 :weight bold))))
  `(magit-branch ((,class (:foreground ,accent4 :weight bold))))
  `(magit-log-author ((,class (:foreground ,shade5))))
  `(magit-hash ((,class (:foreground ,shade6))))
  `(magit-diff-file-header ((,class (:foreground ,shade6 :background ,shade2))))
  `(lazy-highlight ((,class (:foreground ,shade6 :background ,shade2))))
  `(term ((,class (:foreground ,shade6 :background ,shade0))))
  `(term-color-black ((,class (:foreground ,shade6 :background ,shade0))))
  `(term-color-blue ((,class (:foreground ,accent5 :background ,shade0))))
  `(term-color-red ((,class (:foreground ,accent0 :background ,shade0))))
  `(term-color-green ((,class (:foreground ,accent3 :background ,shade0))))
  `(term-color-yellow ((,class (:foreground ,accent2 :background ,shade0))))
  `(term-color-magenta ((,class (:foreground ,accent7 :background ,shade0))))
  `(term-color-cyan ((,class (:foreground ,accent4 :background ,shade0))))
  `(term-color-white ((,class (:foreground ,shade2 :background ,shade0))))
  `(rainbow-delimiters-unmatched-face ((,class :foreground ,accent0)))
  `(helm-header ((,class (:foreground ,shade6 :background ,shade0 :underline nil :box nil))))
  `(helm-source-header ((,class (:foreground ,accent1 :background ,shade0 :underline nil :weight bold))))
  `(helm-selection ((,class (:background ,shade1 :underline nil))))
  `(helm-selection-line ((,class (:background ,shade1))))
  `(helm-visible-mark ((,class (:foreground ,shade0 :background ,shade2))))
  `(helm-candidate-number ((,class (:foreground ,shade0 :background ,shade7))))
  `(helm-separator ((,class (:foreground ,accent5 :background ,shade0))))
  `(helm-time-zone-current ((,class (:foreground ,accent5 :background ,shade0))))
  `(helm-time-zone-home ((,class (:foreground ,accent5 :background ,shade0))))
  `(helm-buffer-not-saved ((,class (:foreground ,accent5 :background ,shade0))))
  `(helm-buffer-process ((,class (:foreground ,accent5 :background ,shade0))))
  `(helm-buffer-saved-out ((,class (:foreground ,shade7 :background ,shade0))))
  `(helm-buffer-size ((,class (:foreground ,shade7 :background ,shade0))))
  `(helm-ff-directory ((,class (:foreground ,accent6 :background ,shade0 :weight bold))))
  `(helm-ff-file ((,class (:foreground ,shade7 :background ,shade0 :weight normal))))
  `(helm-ff-executable ((,class (:foreground ,accent2 :background ,shade0 :weight normal))))
  `(helm-ff-invalid-symlink ((,class (:foreground ,accent7 :background ,shade0 :weight bold))))
  `(helm-ff-symlink ((,class (:foreground ,accent1 :background ,shade0 :weight bold))))
  `(helm-ff-prefix ((,class (:foreground ,shade0 :background ,accent1 :weight normal))))
  `(helm-grep-cmd-line ((,class (:foreground ,shade7 :background ,shade0))))
  `(helm-grep-file ((,class (:foreground ,shade7 :background ,shade0))))
  `(helm-grep-finish ((,class (:foreground ,shade6 :background ,shade0))))
  `(helm-grep-lineno ((,class (:foreground ,shade7 :background ,shade0))))
  `(helm-grep-match ((,class (:foreground nil :background nil :inherit helm-match))))
  `(helm-grep-running ((,class (:foreground ,accent6 :background ,shade0))))
  `(helm-moccur-buffer ((,class (:foreground ,accent6 :background ,shade0))))
  `(helm-source-go-package-godoc-description ((,class (:foreground ,accent3))))
  `(helm-bookmark-w3m ((,class (:foreground ,accent5))))
  `(company-echo-common ((,class (:foreground ,shade0 :background ,shade7))))
  `(company-preview ((,class (:background ,shade0 :foreground ,accent2))))
  `(company-preview-common ((,class (:foreground ,shade1 :foreground ,shade5))))
  `(company-preview-search ((,class (:foreground ,accent5 :background ,shade0))))
  `(company-scrollbar-bg ((,class (:background ,shade2))))
  `(company-scrollbar-fg ((,class (:foreground ,accent1))))
  `(company-tooltip ((,class (:foreground ,shade6 :background ,shade0 :bold t))))
  `(company-tooltop-annotation ((,class (:foreground ,accent4))))
  `(company-tooltip-common ((,class ( :foreground ,shade5))))
  `(company-tooltip-common-selection ((,class (:foreground ,accent3))))
  `(company-tooltip-mouse ((,class (:inherit highlight))))
  `(company-tooltip-selection ((,class (:background ,shade2 :foreground ,shade5))))
  `(company-template-field ((,class (:inherit region))))
  `(web-mode-builtin-face ((,class (:inherit ,font-lock-builtin-face))))
  `(web-mode-comment-face ((,class (:inherit ,font-lock-comment-face))))
  `(web-mode-constant-face ((,class (:inherit ,font-lock-constant-face))))
  `(web-mode-keyword-face ((,class (:foreground ,accent1))))
  `(web-mode-doctype-face ((,class (:inherit ,font-lock-comment-face))))
  `(web-mode-function-name-face ((,class (:inherit ,font-lock-function-name-face))))
  `(web-mode-string-face ((,class (:foreground ,accent3))))
  `(web-mode-type-face ((,class (:inherit ,font-lock-type-face))))
  `(web-mode-html-attr-name-face ((,class (:foreground ,accent6))))
  `(web-mode-html-attr-value-face ((,class (:foreground ,accent1))))
  `(web-mode-warning-face ((,class (:inherit ,font-lock-warning-face))))
  `(web-mode-html-tag-face ((,class (:foreground ,accent5))))
  `(jde-java-font-lock-package-face ((t (:foreground ,accent6))))
  `(jde-java-font-lock-public-face ((t (:foreground ,accent1))))
  `(jde-java-font-lock-private-face ((t (:foreground ,accent1))))
  `(jde-java-font-lock-constant-face ((t (:foreground ,accent4))))
  `(jde-java-font-lock-modifier-face ((t (:foreground ,accent7))))
  `(jde-jave-font-lock-protected-face ((t (:foreground ,accent1))))
  `(jde-java-font-lock-number-face ((t (:foreground ,accent6))))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
    (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'flamingo-pink-purple-dark)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; flamingo-pink-purple-dark-theme.el ends here
```

## flamingo-pink-purple-light-theme.el

[Get the theme
here!](https://themer.dev/?colors.dark.shade0=%23ab5dee&colors.dark.shade7=%23fca78e&colors.light.shade0=%23fca78e&colors.light.shade7=%23ab5dee&activeColorSet=dark)

``` commonlisp
;;; flamingo-pink-purple-light-theme.el

;; Version: 1.0.1
;; Package-Requires: ((emacs "24"))

;;; Commentary:

;; Generated by the emacs theme template for themer.

;;; Code:

(deftheme flamingo-pink-purple-light)
(let ((class '((class color) (min-colors 89)))
  (shade0 "#fca78e")
  (shade1 "#f09c9c")
  (shade2 "#e592a9")
  (shade3 "#d987b7")
  (shade4 "#ce7dc5")
  (shade5 "#c272d3")
  (shade6 "#b768e0")
  (shade7 "#ab5dee")
  (accent0 "#ab5dee")
  (accent1 "#ab5dee")
  (accent2 "#ab5dee")
  (accent3 "#ab5dee")
  (accent4 "#ab5dee")
  (accent5 "#ab5dee")
  (accent6 "#ab5dee")
  (accent7 "#ab5dee"))
(custom-theme-set-faces
  'flamingo-pink-purple-light
  `(default ((,class (:background ,shade0 :foreground ,shade7))))
  `(font-lock-builtin-face ((,class (:foreground ,accent5))))
  `(font-lock-comment-face ((,class (:foreground ,shade3))))
  `(font-lock-negation-char-face ((,class (:foreground ,accent4))))
  `(font-lock-reference-face ((,class (:foreground ,accent4))))
  `(font-lock-constant-face ((,class (:foreground ,accent4))))
  `(font-lock-doc-face ((,class (:foreground ,shade3))))
  `(font-lock-function-name-face ((,class (:foreground ,accent6 :bold t))))
  `(font-lock-keyword-face ((,class (:bold ,class :foreground ,accent1))))
  `(font-lock-string-face ((,class (:foreground ,accent3))))
  `(font-lock-type-face ((,class (:foreground ,accent5 ))))
  `(font-lock-variable-name-face ((,class (:foreground ,accent6))))
  `(font-lock-warning-face ((,class (:foreground ,accent0 :background ,shade1))))
  `(region ((,class (:background ,shade7 :foreground ,shade0))))
  `(highlight ((,class (:foreground ,shade5 :background ,shade2))))
  `(hl-line ((,class (:background  ,shade1))))
  `(fringe ((,class (:background ,shade1 :foreground ,shade4))))
  `(cursor ((,class (:background ,shade2))))
  `(show-paren-match-face ((,class (:background ,accent0))))
  `(isearch ((,class (:bold t :foreground ,accent0 :background ,shade2))))
  `(mode-line ((,class (:box (:line-width 1 :color nil) :bold t :foreground ,shade4 :background ,shade0))))
  `(mode-line-inactive ((,class (:box (:line-width 1 :color nil :style pressed-button) :foreground ,accent7 :background ,shade1 :weight normal))))
  `(mode-line-buffer-id ((,class (:bold t :foreground ,accent6 :background nil))))
  `(mode-line-highlight ((,class (:foreground ,accent1 :box nil :weight bold))))
  `(mode-line-emphasis ((,class (:foreground ,shade7))))
  `(vertical-border ((,class (:foreground ,shade5))))
  `(minibuffer-prompt ((,class (:bold t :foreground ,accent1))))
  `(default-italic ((,class (:italic t))))
  `(link ((,class (:foreground ,accent4 :underline t))))
  `(org-code ((,class (:foreground ,shade6))))
  `(org-hide ((,class (:foreground ,shade4))))
  `(org-level-1 ((,class (:bold t :foreground ,accent4 :height 1.1))))
  `(org-level-2 ((,class (:bold nil :foreground ,accent5))))
  `(org-level-3 ((,class (:bold t :foreground ,accent6))))
  `(org-level-4 ((,class (:bold nil :foreground ,accent7))))
  `(org-date ((,class (:underline t :foreground ,accent2) )))
  `(org-footnote  ((,class (:underline t :foreground ,shade4))))
  `(org-link ((,class (:underline t :foreground ,accent5 ))))
  `(org-special-keyword ((,class (:foreground ,accent1))))
  `(org-block ((,class (:foreground ,shade5))))
  `(org-quote ((,class (:inherit org-block :slant italic))))
  `(org-verse ((,class (:inherit org-block :slant italic))))
  `(org-todo ((,class (:box (:line-width 1 :color ,shade1) :foreground ,accent1 :bold t))))
  `(org-done ((,class (:box (:line-width 1 :color ,shade1) :foreground ,shade5 :bold t))))
  `(org-warning ((,class (:underline t :foreground ,accent2))))
  `(org-agenda-structure ((,class (:weight bold :foreground ,shade5 :box (:color ,shade4) :background ,shade2))))
  `(org-agenda-date ((,class (:foreground ,accent6 :height 1.1 ))))
  `(org-agenda-date-weekend ((,class (:weight normal :foreground ,shade4))))
  `(org-agenda-date-today ((,class (:weight bold :foreground ,accent1 :height 1.4))))
  `(org-agenda-done ((,class (:foreground ,shade3))))
  `(org-scheduled ((,class (:foreground ,accent5))))
  `(org-scheduled-today ((,class (:foreground ,accent6 :weight bold :height 1.2))))
  `(org-ellipsis ((,class (:foreground ,accent5))))
  `(org-verbatim ((,class (:foreground ,shade4))))
  `(org-document-info-keyword ((,class (:foreground ,accent6))))
  `(font-latex-bold-face ((,class (:foreground ,accent5))))
  `(font-latex-italic-face ((,class (:foreground ,accent7 :italic t))))
  `(font-latex-string-face ((,class (:foreground ,accent3))))
  `(font-latex-match-reference-keywords ((,class (:foreground ,accent4))))
  `(font-latex-match-variable-keywords ((,class (:foreground ,accent6))))
  `(ido-only-match ((,class (:foreground ,accent0))))
  `(org-sexp-date ((,class (:foreground ,shade4))))
  `(ido-first-match ((,class (:foreground ,accent1 :bold t))))
  `(gnus-header-content ((,class (:foreground ,accent1))))
  `(gnus-header-from ((,class (:foreground ,accent6))))
  `(gnus-header-name ((,class (:foreground ,accent5))))
  `(gnus-header-subject ((,class (:foreground ,accent6 :bold t))))
  `(mu4e-view-url-number-face ((,class (:foreground ,accent5))))
  `(mu4e-cited-1-face ((,class (:foreground ,shade6))))
  `(mu4e-cited-7-face ((,class (:foreground ,shade5))))
  `(mu4e-header-marks-face ((,class (:foreground ,accent5))))
  `(ffap ((,class (:foreground ,shade4))))
  `(js2-private-function-call ((,class (:foreground ,accent4))))
  `(js2-jsdoc-html-tag-delimiter ((,class (:foreground ,accent3))))
  `(js2-jsdoc-html-tag-name ((,class (:foreground ,accent2))))
  `(js2-external-variable ((,class (:foreground ,accent5  ))))
  `(js2-function-param ((,class (:foreground ,accent4))))
  `(js2-jsdoc-value ((,class (:foreground ,accent3))))
  `(js2-private-member ((,class (:foreground ,shade5))))
  `(js3-warning-face ((,class (:underline ,accent1))))
  `(js3-error-face ((,class (:underline ,accent0))))
  `(js3-external-variable-face ((,class (:foreground ,accent6))))
  `(js3-function-param-face ((,class (:foreground ,accent7))))
  `(js3-jsdoc-tag-face ((,class (:foreground ,accent1))))
  `(js3-instance-member-face ((,class (:foreground ,accent4))))
  `(warning ((,class (:foreground ,accent0))))
  `(ac-completion-face ((,class (:underline t :foreground ,accent1))))
  `(info-quoted-name ((,class (:foreground ,accent5))))
  `(info-string ((,class (:foreground ,accent3))))
  `(icompletep-determined ((,class :foreground ,accent5)))
  `(undo-tree-visualizer-current-face ((,class :foreground ,accent5)))
  `(undo-tree-visualizer-default-face ((,class :foreground ,shade6)))
  `(undo-tree-visualizer-unmodified-face ((,class :foreground ,accent6)))
  `(undo-tree-visualizer-register-face ((,class :foreground ,accent5)))
  `(slime-repl-inputed-output-face ((,class (:foreground ,accent5))))
  `(trailing-whitespace ((,class :foreground nil :background ,accent0)))
  `(rainbow-delimiters-depth-1-face ((,class :foreground ,shade7)))
  `(rainbow-delimiters-depth-2-face ((,class :foreground ,accent5)))
  `(rainbow-delimiters-depth-3-face ((,class :foreground ,accent6)))
  `(rainbow-delimiters-depth-4-face ((,class :foreground ,accent4)))
  `(rainbow-delimiters-depth-5-face ((,class :foreground ,accent1)))
  `(rainbow-delimiters-depth-6-face ((,class :foreground ,shade5)))
  `(rainbow-delimiters-depth-7-face ((,class :foreground ,accent5)))
  `(rainbow-delimiters-depth-8-face ((,class :foreground ,accent3)))
  `(magit-item-highlight ((,class :background ,shade2)))
  `(magit-section-heading        ((,class (:foreground ,accent1 :weight bold))))
  `(magit-hunk-heading           ((,class (:background ,shade2))))
  `(magit-section-highlight      ((,class (:background ,shade1))))
  `(magit-hunk-heading-highlight ((,class (:background ,shade2))))
  `(magit-diff-context-highlight ((,class (:background ,shade2 :foreground ,shade5))))
  `(magit-diffstat-added   ((,class (:foreground ,accent5))))
  `(magit-diffstat-removed ((,class (:foreground ,accent6))))
  `(magit-process-ok ((,class (:foreground ,accent6 :weight bold))))
  `(magit-process-ng ((,class (:foreground ,accent0 :weight bold))))
  `(magit-branch ((,class (:foreground ,accent4 :weight bold))))
  `(magit-log-author ((,class (:foreground ,shade5))))
  `(magit-hash ((,class (:foreground ,shade6))))
  `(magit-diff-file-header ((,class (:foreground ,shade6 :background ,shade2))))
  `(lazy-highlight ((,class (:foreground ,shade6 :background ,shade2))))
  `(term ((,class (:foreground ,shade6 :background ,shade0))))
  `(term-color-black ((,class (:foreground ,shade6 :background ,shade0))))
  `(term-color-blue ((,class (:foreground ,accent5 :background ,shade0))))
  `(term-color-red ((,class (:foreground ,accent0 :background ,shade0))))
  `(term-color-green ((,class (:foreground ,accent3 :background ,shade0))))
  `(term-color-yellow ((,class (:foreground ,accent2 :background ,shade0))))
  `(term-color-magenta ((,class (:foreground ,accent7 :background ,shade0))))
  `(term-color-cyan ((,class (:foreground ,accent4 :background ,shade0))))
  `(term-color-white ((,class (:foreground ,shade2 :background ,shade0))))
  `(rainbow-delimiters-unmatched-face ((,class :foreground ,accent0)))
  `(helm-header ((,class (:foreground ,shade6 :background ,shade0 :underline nil :box nil))))
  `(helm-source-header ((,class (:foreground ,accent1 :background ,shade0 :underline nil :weight bold))))
  `(helm-selection ((,class (:background ,shade1 :underline nil))))
  `(helm-selection-line ((,class (:background ,shade1))))
  `(helm-visible-mark ((,class (:foreground ,shade0 :background ,shade2))))
  `(helm-candidate-number ((,class (:foreground ,shade0 :background ,shade7))))
  `(helm-separator ((,class (:foreground ,accent5 :background ,shade0))))
  `(helm-time-zone-current ((,class (:foreground ,accent5 :background ,shade0))))
  `(helm-time-zone-home ((,class (:foreground ,accent5 :background ,shade0))))
  `(helm-buffer-not-saved ((,class (:foreground ,accent5 :background ,shade0))))
  `(helm-buffer-process ((,class (:foreground ,accent5 :background ,shade0))))
  `(helm-buffer-saved-out ((,class (:foreground ,shade7 :background ,shade0))))
  `(helm-buffer-size ((,class (:foreground ,shade7 :background ,shade0))))
  `(helm-ff-directory ((,class (:foreground ,accent6 :background ,shade0 :weight bold))))
  `(helm-ff-file ((,class (:foreground ,shade7 :background ,shade0 :weight normal))))
  `(helm-ff-executable ((,class (:foreground ,accent2 :background ,shade0 :weight normal))))
  `(helm-ff-invalid-symlink ((,class (:foreground ,accent7 :background ,shade0 :weight bold))))
  `(helm-ff-symlink ((,class (:foreground ,accent1 :background ,shade0 :weight bold))))
  `(helm-ff-prefix ((,class (:foreground ,shade0 :background ,accent1 :weight normal))))
  `(helm-grep-cmd-line ((,class (:foreground ,shade7 :background ,shade0))))
  `(helm-grep-file ((,class (:foreground ,shade7 :background ,shade0))))
  `(helm-grep-finish ((,class (:foreground ,shade6 :background ,shade0))))
  `(helm-grep-lineno ((,class (:foreground ,shade7 :background ,shade0))))
  `(helm-grep-match ((,class (:foreground nil :background nil :inherit helm-match))))
  `(helm-grep-running ((,class (:foreground ,accent6 :background ,shade0))))
  `(helm-moccur-buffer ((,class (:foreground ,accent6 :background ,shade0))))
  `(helm-source-go-package-godoc-description ((,class (:foreground ,accent3))))
  `(helm-bookmark-w3m ((,class (:foreground ,accent5))))
  `(company-echo-common ((,class (:foreground ,shade0 :background ,shade7))))
  `(company-preview ((,class (:background ,shade0 :foreground ,accent2))))
  `(company-preview-common ((,class (:foreground ,shade1 :foreground ,shade5))))
  `(company-preview-search ((,class (:foreground ,accent5 :background ,shade0))))
  `(company-scrollbar-bg ((,class (:background ,shade2))))
  `(company-scrollbar-fg ((,class (:foreground ,accent1))))
  `(company-tooltip ((,class (:foreground ,shade6 :background ,shade0 :bold t))))
  `(company-tooltop-annotation ((,class (:foreground ,accent4))))
  `(company-tooltip-common ((,class ( :foreground ,shade5))))
  `(company-tooltip-common-selection ((,class (:foreground ,accent3))))
  `(company-tooltip-mouse ((,class (:inherit highlight))))
  `(company-tooltip-selection ((,class (:background ,shade2 :foreground ,shade5))))
  `(company-template-field ((,class (:inherit region))))
  `(web-mode-builtin-face ((,class (:inherit ,font-lock-builtin-face))))
  `(web-mode-comment-face ((,class (:inherit ,font-lock-comment-face))))
  `(web-mode-constant-face ((,class (:inherit ,font-lock-constant-face))))
  `(web-mode-keyword-face ((,class (:foreground ,accent1))))
  `(web-mode-doctype-face ((,class (:inherit ,font-lock-comment-face))))
  `(web-mode-function-name-face ((,class (:inherit ,font-lock-function-name-face))))
  `(web-mode-string-face ((,class (:foreground ,accent3))))
  `(web-mode-type-face ((,class (:inherit ,font-lock-type-face))))
  `(web-mode-html-attr-name-face ((,class (:foreground ,accent6))))
  `(web-mode-html-attr-value-face ((,class (:foreground ,accent1))))
  `(web-mode-warning-face ((,class (:inherit ,font-lock-warning-face))))
  `(web-mode-html-tag-face ((,class (:foreground ,accent5))))
  `(jde-java-font-lock-package-face ((t (:foreground ,accent6))))
  `(jde-java-font-lock-public-face ((t (:foreground ,accent1))))
  `(jde-java-font-lock-private-face ((t (:foreground ,accent1))))
  `(jde-java-font-lock-constant-face ((t (:foreground ,accent4))))
  `(jde-java-font-lock-modifier-face ((t (:foreground ,accent7))))
  `(jde-jave-font-lock-protected-face ((t (:foreground ,accent1))))
  `(jde-java-font-lock-number-face ((t (:foreground ,accent6))))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
    (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'flamingo-pink-purple-light)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; flamingo-pink-purple-light-theme.el ends here
```

## orange-purple-dark-theme.el

[Get the theme
here!](https://themer.dev/?colors.dark.shade0=%23ab5dee&colors.dark.shade7=%23ffb86c&colors.light.shade0=%23fca78e&colors.light.shade7=%23ab5dee&activeColorSet=dark)

``` commonlisp
;;; orange-purple-dark-theme.el

;; Version: 1.0.1
;; Package-Requires: ((emacs "24"))

;;; Commentary:

;; Generated by the emacs theme template for themer.

;;; Code:

(deftheme orange-purple-dark)
(let ((class '((class color) (min-colors 89)))
  (shade0 "#ab5dee")
  (shade1 "#b76adb")
  (shade2 "#c377c9")
  (shade3 "#cf84b6")
  (shade4 "#db91a4")
  (shade5 "#e79e91")
  (shade6 "#f3ab7f")
  (shade7 "#ffb86c")
  (accent0 "#ffb86c")
  (accent1 "#ffb86c")
  (accent2 "#ffb86c")
  (accent3 "#ffb86c")
  (accent4 "#ffb86c")
  (accent5 "#ffb86c")
  (accent6 "#ffb86c")
  (accent7 "#ffb86c"))
(custom-theme-set-faces
  'orange-purple-dark
  `(default ((,class (:background ,shade0 :foreground ,shade7))))
  `(font-lock-builtin-face ((,class (:foreground ,accent5))))
  `(font-lock-comment-face ((,class (:foreground ,shade3))))
  `(font-lock-negation-char-face ((,class (:foreground ,accent4))))
  `(font-lock-reference-face ((,class (:foreground ,accent4))))
  `(font-lock-constant-face ((,class (:foreground ,accent4))))
  `(font-lock-doc-face ((,class (:foreground ,shade3))))
  `(font-lock-function-name-face ((,class (:foreground ,accent6 :bold t))))
  `(font-lock-keyword-face ((,class (:bold ,class :foreground ,accent1))))
  `(font-lock-string-face ((,class (:foreground ,accent3))))
  `(font-lock-type-face ((,class (:foreground ,accent5 ))))
  `(font-lock-variable-name-face ((,class (:foreground ,accent6))))
  `(font-lock-warning-face ((,class (:foreground ,accent0 :background ,shade1))))
  `(region ((,class (:background ,shade7 :foreground ,shade0))))
  `(highlight ((,class (:foreground ,shade5 :background ,shade2))))
  `(hl-line ((,class (:background  ,shade1))))
  `(fringe ((,class (:background ,shade1 :foreground ,shade4))))
  `(cursor ((,class (:background ,shade2))))
  `(show-paren-match-face ((,class (:background ,accent0))))
  `(isearch ((,class (:bold t :foreground ,accent0 :background ,shade2))))
  `(mode-line ((,class (:box (:line-width 1 :color nil) :bold t :foreground ,shade4 :background ,shade0))))
  `(mode-line-inactive ((,class (:box (:line-width 1 :color nil :style pressed-button) :foreground ,accent7 :background ,shade1 :weight normal))))
  `(mode-line-buffer-id ((,class (:bold t :foreground ,accent6 :background nil))))
  `(mode-line-highlight ((,class (:foreground ,accent1 :box nil :weight bold))))
  `(mode-line-emphasis ((,class (:foreground ,shade7))))
  `(vertical-border ((,class (:foreground ,shade5))))
  `(minibuffer-prompt ((,class (:bold t :foreground ,accent1))))
  `(default-italic ((,class (:italic t))))
  `(link ((,class (:foreground ,accent4 :underline t))))
  `(org-code ((,class (:foreground ,shade6))))
  `(org-hide ((,class (:foreground ,shade4))))
  `(org-level-1 ((,class (:bold t :foreground ,accent4 :height 1.1))))
  `(org-level-2 ((,class (:bold nil :foreground ,accent5))))
  `(org-level-3 ((,class (:bold t :foreground ,accent6))))
  `(org-level-4 ((,class (:bold nil :foreground ,accent7))))
  `(org-date ((,class (:underline t :foreground ,accent2) )))
  `(org-footnote  ((,class (:underline t :foreground ,shade4))))
  `(org-link ((,class (:underline t :foreground ,accent5 ))))
  `(org-special-keyword ((,class (:foreground ,accent1))))
  `(org-block ((,class (:foreground ,shade5))))
  `(org-quote ((,class (:inherit org-block :slant italic))))
  `(org-verse ((,class (:inherit org-block :slant italic))))
  `(org-todo ((,class (:box (:line-width 1 :color ,shade1) :foreground ,accent1 :bold t))))
  `(org-done ((,class (:box (:line-width 1 :color ,shade1) :foreground ,shade5 :bold t))))
  `(org-warning ((,class (:underline t :foreground ,accent2))))
  `(org-agenda-structure ((,class (:weight bold :foreground ,shade5 :box (:color ,shade4) :background ,shade2))))
  `(org-agenda-date ((,class (:foreground ,accent6 :height 1.1 ))))
  `(org-agenda-date-weekend ((,class (:weight normal :foreground ,shade4))))
  `(org-agenda-date-today ((,class (:weight bold :foreground ,accent1 :height 1.4))))
  `(org-agenda-done ((,class (:foreground ,shade3))))
  `(org-scheduled ((,class (:foreground ,accent5))))
  `(org-scheduled-today ((,class (:foreground ,accent6 :weight bold :height 1.2))))
  `(org-ellipsis ((,class (:foreground ,accent5))))
  `(org-verbatim ((,class (:foreground ,shade4))))
  `(org-document-info-keyword ((,class (:foreground ,accent6))))
  `(font-latex-bold-face ((,class (:foreground ,accent5))))
  `(font-latex-italic-face ((,class (:foreground ,accent7 :italic t))))
  `(font-latex-string-face ((,class (:foreground ,accent3))))
  `(font-latex-match-reference-keywords ((,class (:foreground ,accent4))))
  `(font-latex-match-variable-keywords ((,class (:foreground ,accent6))))
  `(ido-only-match ((,class (:foreground ,accent0))))
  `(org-sexp-date ((,class (:foreground ,shade4))))
  `(ido-first-match ((,class (:foreground ,accent1 :bold t))))
  `(gnus-header-content ((,class (:foreground ,accent1))))
  `(gnus-header-from ((,class (:foreground ,accent6))))
  `(gnus-header-name ((,class (:foreground ,accent5))))
  `(gnus-header-subject ((,class (:foreground ,accent6 :bold t))))
  `(mu4e-view-url-number-face ((,class (:foreground ,accent5))))
  `(mu4e-cited-1-face ((,class (:foreground ,shade6))))
  `(mu4e-cited-7-face ((,class (:foreground ,shade5))))
  `(mu4e-header-marks-face ((,class (:foreground ,accent5))))
  `(ffap ((,class (:foreground ,shade4))))
  `(js2-private-function-call ((,class (:foreground ,accent4))))
  `(js2-jsdoc-html-tag-delimiter ((,class (:foreground ,accent3))))
  `(js2-jsdoc-html-tag-name ((,class (:foreground ,accent2))))
  `(js2-external-variable ((,class (:foreground ,accent5  ))))
  `(js2-function-param ((,class (:foreground ,accent4))))
  `(js2-jsdoc-value ((,class (:foreground ,accent3))))
  `(js2-private-member ((,class (:foreground ,shade5))))
  `(js3-warning-face ((,class (:underline ,accent1))))
  `(js3-error-face ((,class (:underline ,accent0))))
  `(js3-external-variable-face ((,class (:foreground ,accent6))))
  `(js3-function-param-face ((,class (:foreground ,accent7))))
  `(js3-jsdoc-tag-face ((,class (:foreground ,accent1))))
  `(js3-instance-member-face ((,class (:foreground ,accent4))))
  `(warning ((,class (:foreground ,accent0))))
  `(ac-completion-face ((,class (:underline t :foreground ,accent1))))
  `(info-quoted-name ((,class (:foreground ,accent5))))
  `(info-string ((,class (:foreground ,accent3))))
  `(icompletep-determined ((,class :foreground ,accent5)))
  `(undo-tree-visualizer-current-face ((,class :foreground ,accent5)))
  `(undo-tree-visualizer-default-face ((,class :foreground ,shade6)))
  `(undo-tree-visualizer-unmodified-face ((,class :foreground ,accent6)))
  `(undo-tree-visualizer-register-face ((,class :foreground ,accent5)))
  `(slime-repl-inputed-output-face ((,class (:foreground ,accent5))))
  `(trailing-whitespace ((,class :foreground nil :background ,accent0)))
  `(rainbow-delimiters-depth-1-face ((,class :foreground ,shade7)))
  `(rainbow-delimiters-depth-2-face ((,class :foreground ,accent5)))
  `(rainbow-delimiters-depth-3-face ((,class :foreground ,accent6)))
  `(rainbow-delimiters-depth-4-face ((,class :foreground ,accent4)))
  `(rainbow-delimiters-depth-5-face ((,class :foreground ,accent1)))
  `(rainbow-delimiters-depth-6-face ((,class :foreground ,shade5)))
  `(rainbow-delimiters-depth-7-face ((,class :foreground ,accent5)))
  `(rainbow-delimiters-depth-8-face ((,class :foreground ,accent3)))
  `(magit-item-highlight ((,class :background ,shade2)))
  `(magit-section-heading        ((,class (:foreground ,accent1 :weight bold))))
  `(magit-hunk-heading           ((,class (:background ,shade2))))
  `(magit-section-highlight      ((,class (:background ,shade1))))
  `(magit-hunk-heading-highlight ((,class (:background ,shade2))))
  `(magit-diff-context-highlight ((,class (:background ,shade2 :foreground ,shade5))))
  `(magit-diffstat-added   ((,class (:foreground ,accent5))))
  `(magit-diffstat-removed ((,class (:foreground ,accent6))))
  `(magit-process-ok ((,class (:foreground ,accent6 :weight bold))))
  `(magit-process-ng ((,class (:foreground ,accent0 :weight bold))))
  `(magit-branch ((,class (:foreground ,accent4 :weight bold))))
  `(magit-log-author ((,class (:foreground ,shade5))))
  `(magit-hash ((,class (:foreground ,shade6))))
  `(magit-diff-file-header ((,class (:foreground ,shade6 :background ,shade2))))
  `(lazy-highlight ((,class (:foreground ,shade6 :background ,shade2))))
  `(term ((,class (:foreground ,shade6 :background ,shade0))))
  `(term-color-black ((,class (:foreground ,shade6 :background ,shade0))))
  `(term-color-blue ((,class (:foreground ,accent5 :background ,shade0))))
  `(term-color-red ((,class (:foreground ,accent0 :background ,shade0))))
  `(term-color-green ((,class (:foreground ,accent3 :background ,shade0))))
  `(term-color-yellow ((,class (:foreground ,accent2 :background ,shade0))))
  `(term-color-magenta ((,class (:foreground ,accent7 :background ,shade0))))
  `(term-color-cyan ((,class (:foreground ,accent4 :background ,shade0))))
  `(term-color-white ((,class (:foreground ,shade2 :background ,shade0))))
  `(rainbow-delimiters-unmatched-face ((,class :foreground ,accent0)))
  `(helm-header ((,class (:foreground ,shade6 :background ,shade0 :underline nil :box nil))))
  `(helm-source-header ((,class (:foreground ,accent1 :background ,shade0 :underline nil :weight bold))))
  `(helm-selection ((,class (:background ,shade1 :underline nil))))
  `(helm-selection-line ((,class (:background ,shade1))))
  `(helm-visible-mark ((,class (:foreground ,shade0 :background ,shade2))))
  `(helm-candidate-number ((,class (:foreground ,shade0 :background ,shade7))))
  `(helm-separator ((,class (:foreground ,accent5 :background ,shade0))))
  `(helm-time-zone-current ((,class (:foreground ,accent5 :background ,shade0))))
  `(helm-time-zone-home ((,class (:foreground ,accent5 :background ,shade0))))
  `(helm-buffer-not-saved ((,class (:foreground ,accent5 :background ,shade0))))
  `(helm-buffer-process ((,class (:foreground ,accent5 :background ,shade0))))
  `(helm-buffer-saved-out ((,class (:foreground ,shade7 :background ,shade0))))
  `(helm-buffer-size ((,class (:foreground ,shade7 :background ,shade0))))
  `(helm-ff-directory ((,class (:foreground ,accent6 :background ,shade0 :weight bold))))
  `(helm-ff-file ((,class (:foreground ,shade7 :background ,shade0 :weight normal))))
  `(helm-ff-executable ((,class (:foreground ,accent2 :background ,shade0 :weight normal))))
  `(helm-ff-invalid-symlink ((,class (:foreground ,accent7 :background ,shade0 :weight bold))))
  `(helm-ff-symlink ((,class (:foreground ,accent1 :background ,shade0 :weight bold))))
  `(helm-ff-prefix ((,class (:foreground ,shade0 :background ,accent1 :weight normal))))
  `(helm-grep-cmd-line ((,class (:foreground ,shade7 :background ,shade0))))
  `(helm-grep-file ((,class (:foreground ,shade7 :background ,shade0))))
  `(helm-grep-finish ((,class (:foreground ,shade6 :background ,shade0))))
  `(helm-grep-lineno ((,class (:foreground ,shade7 :background ,shade0))))
  `(helm-grep-match ((,class (:foreground nil :background nil :inherit helm-match))))
  `(helm-grep-running ((,class (:foreground ,accent6 :background ,shade0))))
  `(helm-moccur-buffer ((,class (:foreground ,accent6 :background ,shade0))))
  `(helm-source-go-package-godoc-description ((,class (:foreground ,accent3))))
  `(helm-bookmark-w3m ((,class (:foreground ,accent5))))
  `(company-echo-common ((,class (:foreground ,shade0 :background ,shade7))))
  `(company-preview ((,class (:background ,shade0 :foreground ,accent2))))
  `(company-preview-common ((,class (:foreground ,shade1 :foreground ,shade5))))
  `(company-preview-search ((,class (:foreground ,accent5 :background ,shade0))))
  `(company-scrollbar-bg ((,class (:background ,shade2))))
  `(company-scrollbar-fg ((,class (:foreground ,accent1))))
  `(company-tooltip ((,class (:foreground ,shade6 :background ,shade0 :bold t))))
  `(company-tooltop-annotation ((,class (:foreground ,accent4))))
  `(company-tooltip-common ((,class ( :foreground ,shade5))))
  `(company-tooltip-common-selection ((,class (:foreground ,accent3))))
  `(company-tooltip-mouse ((,class (:inherit highlight))))
  `(company-tooltip-selection ((,class (:background ,shade2 :foreground ,shade5))))
  `(company-template-field ((,class (:inherit region))))
  `(web-mode-builtin-face ((,class (:inherit ,font-lock-builtin-face))))
  `(web-mode-comment-face ((,class (:inherit ,font-lock-comment-face))))
  `(web-mode-constant-face ((,class (:inherit ,font-lock-constant-face))))
  `(web-mode-keyword-face ((,class (:foreground ,accent1))))
  `(web-mode-doctype-face ((,class (:inherit ,font-lock-comment-face))))
  `(web-mode-function-name-face ((,class (:inherit ,font-lock-function-name-face))))
  `(web-mode-string-face ((,class (:foreground ,accent3))))
  `(web-mode-type-face ((,class (:inherit ,font-lock-type-face))))
  `(web-mode-html-attr-name-face ((,class (:foreground ,accent6))))
  `(web-mode-html-attr-value-face ((,class (:foreground ,accent1))))
  `(web-mode-warning-face ((,class (:inherit ,font-lock-warning-face))))
  `(web-mode-html-tag-face ((,class (:foreground ,accent5))))
  `(jde-java-font-lock-package-face ((t (:foreground ,accent6))))
  `(jde-java-font-lock-public-face ((t (:foreground ,accent1))))
  `(jde-java-font-lock-private-face ((t (:foreground ,accent1))))
  `(jde-java-font-lock-constant-face ((t (:foreground ,accent4))))
  `(jde-java-font-lock-modifier-face ((t (:foreground ,accent7))))
  `(jde-jave-font-lock-protected-face ((t (:foreground ,accent1))))
  `(jde-java-font-lock-number-face ((t (:foreground ,accent6))))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
    (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'orange-purple-dark)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; orange-purple-dark-theme.el ends here
```

## orange-purple-light-theme.el

[Get the theme
here!](https://themer.dev/?colors.dark.shade0=%23ab5dee&colors.dark.shade7=%23ffb86c&colors.light.shade0=%23fca78e&colors.light.shade7=%23ab5dee&activeColorSet=dark)

``` commonlisp
;;; orange-purple-light-theme.el

;; Version: 1.0.1
;; Package-Requires: ((emacs "24"))

;;; Commentary:

;; Generated by the emacs theme template for themer.

;;; Code:

(deftheme orange-purple-light)
(let ((class '((class color) (min-colors 89)))
  (shade0 "#ffb86c")
  (shade1 "#f3ab7f")
  (shade2 "#e79e91")
  (shade3 "#db91a4")
  (shade4 "#cf84b6")
  (shade5 "#c377c9")
  (shade6 "#b76adb")
  (shade7 "#ab5dee")
  (accent0 "#ab5dee")
  (accent1 "#ab5dee")
  (accent2 "#ab5dee")
  (accent3 "#ab5dee")
  (accent4 "#ab5dee")
  (accent5 "#ab5dee")
  (accent6 "#ab5dee")
  (accent7 "#ab5dee"))
(custom-theme-set-faces
  'orange-purple-light
  `(default ((,class (:background ,shade0 :foreground ,shade7))))
  `(font-lock-builtin-face ((,class (:foreground ,accent5))))
  `(font-lock-comment-face ((,class (:foreground ,shade3))))
  `(font-lock-negation-char-face ((,class (:foreground ,accent4))))
  `(font-lock-reference-face ((,class (:foreground ,accent4))))
  `(font-lock-constant-face ((,class (:foreground ,accent4))))
  `(font-lock-doc-face ((,class (:foreground ,shade3))))
  `(font-lock-function-name-face ((,class (:foreground ,accent6 :bold t))))
  `(font-lock-keyword-face ((,class (:bold ,class :foreground ,accent1))))
  `(font-lock-string-face ((,class (:foreground ,accent3))))
  `(font-lock-type-face ((,class (:foreground ,accent5 ))))
  `(font-lock-variable-name-face ((,class (:foreground ,accent6))))
  `(font-lock-warning-face ((,class (:foreground ,accent0 :background ,shade1))))
  `(region ((,class (:background ,shade7 :foreground ,shade0))))
  `(highlight ((,class (:foreground ,shade5 :background ,shade2))))
  `(hl-line ((,class (:background  ,shade1))))
  `(fringe ((,class (:background ,shade1 :foreground ,shade4))))
  `(cursor ((,class (:background ,shade2))))
  `(show-paren-match-face ((,class (:background ,accent0))))
  `(isearch ((,class (:bold t :foreground ,accent0 :background ,shade2))))
  `(mode-line ((,class (:box (:line-width 1 :color nil) :bold t :foreground ,shade4 :background ,shade0))))
  `(mode-line-inactive ((,class (:box (:line-width 1 :color nil :style pressed-button) :foreground ,accent7 :background ,shade1 :weight normal))))
  `(mode-line-buffer-id ((,class (:bold t :foreground ,accent6 :background nil))))
  `(mode-line-highlight ((,class (:foreground ,accent1 :box nil :weight bold))))
  `(mode-line-emphasis ((,class (:foreground ,shade7))))
  `(vertical-border ((,class (:foreground ,shade5))))
  `(minibuffer-prompt ((,class (:bold t :foreground ,accent1))))
  `(default-italic ((,class (:italic t))))
  `(link ((,class (:foreground ,accent4 :underline t))))
  `(org-code ((,class (:foreground ,shade6))))
  `(org-hide ((,class (:foreground ,shade4))))
  `(org-level-1 ((,class (:bold t :foreground ,accent4 :height 1.1))))
  `(org-level-2 ((,class (:bold nil :foreground ,accent5))))
  `(org-level-3 ((,class (:bold t :foreground ,accent6))))
  `(org-level-4 ((,class (:bold nil :foreground ,accent7))))
  `(org-date ((,class (:underline t :foreground ,accent2) )))
  `(org-footnote  ((,class (:underline t :foreground ,shade4))))
  `(org-link ((,class (:underline t :foreground ,accent5 ))))
  `(org-special-keyword ((,class (:foreground ,accent1))))
  `(org-block ((,class (:foreground ,shade5))))
  `(org-quote ((,class (:inherit org-block :slant italic))))
  `(org-verse ((,class (:inherit org-block :slant italic))))
  `(org-todo ((,class (:box (:line-width 1 :color ,shade1) :foreground ,accent1 :bold t))))
  `(org-done ((,class (:box (:line-width 1 :color ,shade1) :foreground ,shade5 :bold t))))
  `(org-warning ((,class (:underline t :foreground ,accent2))))
  `(org-agenda-structure ((,class (:weight bold :foreground ,shade5 :box (:color ,shade4) :background ,shade2))))
  `(org-agenda-date ((,class (:foreground ,accent6 :height 1.1 ))))
  `(org-agenda-date-weekend ((,class (:weight normal :foreground ,shade4))))
  `(org-agenda-date-today ((,class (:weight bold :foreground ,accent1 :height 1.4))))
  `(org-agenda-done ((,class (:foreground ,shade3))))
  `(org-scheduled ((,class (:foreground ,accent5))))
  `(org-scheduled-today ((,class (:foreground ,accent6 :weight bold :height 1.2))))
  `(org-ellipsis ((,class (:foreground ,accent5))))
  `(org-verbatim ((,class (:foreground ,shade4))))
  `(org-document-info-keyword ((,class (:foreground ,accent6))))
  `(font-latex-bold-face ((,class (:foreground ,accent5))))
  `(font-latex-italic-face ((,class (:foreground ,accent7 :italic t))))
  `(font-latex-string-face ((,class (:foreground ,accent3))))
  `(font-latex-match-reference-keywords ((,class (:foreground ,accent4))))
  `(font-latex-match-variable-keywords ((,class (:foreground ,accent6))))
  `(ido-only-match ((,class (:foreground ,accent0))))
  `(org-sexp-date ((,class (:foreground ,shade4))))
  `(ido-first-match ((,class (:foreground ,accent1 :bold t))))
  `(gnus-header-content ((,class (:foreground ,accent1))))
  `(gnus-header-from ((,class (:foreground ,accent6))))
  `(gnus-header-name ((,class (:foreground ,accent5))))
  `(gnus-header-subject ((,class (:foreground ,accent6 :bold t))))
  `(mu4e-view-url-number-face ((,class (:foreground ,accent5))))
  `(mu4e-cited-1-face ((,class (:foreground ,shade6))))
  `(mu4e-cited-7-face ((,class (:foreground ,shade5))))
  `(mu4e-header-marks-face ((,class (:foreground ,accent5))))
  `(ffap ((,class (:foreground ,shade4))))
  `(js2-private-function-call ((,class (:foreground ,accent4))))
  `(js2-jsdoc-html-tag-delimiter ((,class (:foreground ,accent3))))
  `(js2-jsdoc-html-tag-name ((,class (:foreground ,accent2))))
  `(js2-external-variable ((,class (:foreground ,accent5  ))))
  `(js2-function-param ((,class (:foreground ,accent4))))
  `(js2-jsdoc-value ((,class (:foreground ,accent3))))
  `(js2-private-member ((,class (:foreground ,shade5))))
  `(js3-warning-face ((,class (:underline ,accent1))))
  `(js3-error-face ((,class (:underline ,accent0))))
  `(js3-external-variable-face ((,class (:foreground ,accent6))))
  `(js3-function-param-face ((,class (:foreground ,accent7))))
  `(js3-jsdoc-tag-face ((,class (:foreground ,accent1))))
  `(js3-instance-member-face ((,class (:foreground ,accent4))))
  `(warning ((,class (:foreground ,accent0))))
  `(ac-completion-face ((,class (:underline t :foreground ,accent1))))
  `(info-quoted-name ((,class (:foreground ,accent5))))
  `(info-string ((,class (:foreground ,accent3))))
  `(icompletep-determined ((,class :foreground ,accent5)))
  `(undo-tree-visualizer-current-face ((,class :foreground ,accent5)))
  `(undo-tree-visualizer-default-face ((,class :foreground ,shade6)))
  `(undo-tree-visualizer-unmodified-face ((,class :foreground ,accent6)))
  `(undo-tree-visualizer-register-face ((,class :foreground ,accent5)))
  `(slime-repl-inputed-output-face ((,class (:foreground ,accent5))))
  `(trailing-whitespace ((,class :foreground nil :background ,accent0)))
  `(rainbow-delimiters-depth-1-face ((,class :foreground ,shade7)))
  `(rainbow-delimiters-depth-2-face ((,class :foreground ,accent5)))
  `(rainbow-delimiters-depth-3-face ((,class :foreground ,accent6)))
  `(rainbow-delimiters-depth-4-face ((,class :foreground ,accent4)))
  `(rainbow-delimiters-depth-5-face ((,class :foreground ,accent1)))
  `(rainbow-delimiters-depth-6-face ((,class :foreground ,shade5)))
  `(rainbow-delimiters-depth-7-face ((,class :foreground ,accent5)))
  `(rainbow-delimiters-depth-8-face ((,class :foreground ,accent3)))
  `(magit-item-highlight ((,class :background ,shade2)))
  `(magit-section-heading        ((,class (:foreground ,accent1 :weight bold))))
  `(magit-hunk-heading           ((,class (:background ,shade2))))
  `(magit-section-highlight      ((,class (:background ,shade1))))
  `(magit-hunk-heading-highlight ((,class (:background ,shade2))))
  `(magit-diff-context-highlight ((,class (:background ,shade2 :foreground ,shade5))))
  `(magit-diffstat-added   ((,class (:foreground ,accent5))))
  `(magit-diffstat-removed ((,class (:foreground ,accent6))))
  `(magit-process-ok ((,class (:foreground ,accent6 :weight bold))))
  `(magit-process-ng ((,class (:foreground ,accent0 :weight bold))))
  `(magit-branch ((,class (:foreground ,accent4 :weight bold))))
  `(magit-log-author ((,class (:foreground ,shade5))))
  `(magit-hash ((,class (:foreground ,shade6))))
  `(magit-diff-file-header ((,class (:foreground ,shade6 :background ,shade2))))
  `(lazy-highlight ((,class (:foreground ,shade6 :background ,shade2))))
  `(term ((,class (:foreground ,shade6 :background ,shade0))))
  `(term-color-black ((,class (:foreground ,shade6 :background ,shade0))))
  `(term-color-blue ((,class (:foreground ,accent5 :background ,shade0))))
  `(term-color-red ((,class (:foreground ,accent0 :background ,shade0))))
  `(term-color-green ((,class (:foreground ,accent3 :background ,shade0))))
  `(term-color-yellow ((,class (:foreground ,accent2 :background ,shade0))))
  `(term-color-magenta ((,class (:foreground ,accent7 :background ,shade0))))
  `(term-color-cyan ((,class (:foreground ,accent4 :background ,shade0))))
  `(term-color-white ((,class (:foreground ,shade2 :background ,shade0))))
  `(rainbow-delimiters-unmatched-face ((,class :foreground ,accent0)))
  `(helm-header ((,class (:foreground ,shade6 :background ,shade0 :underline nil :box nil))))
  `(helm-source-header ((,class (:foreground ,accent1 :background ,shade0 :underline nil :weight bold))))
  `(helm-selection ((,class (:background ,shade1 :underline nil))))
  `(helm-selection-line ((,class (:background ,shade1))))
  `(helm-visible-mark ((,class (:foreground ,shade0 :background ,shade2))))
  `(helm-candidate-number ((,class (:foreground ,shade0 :background ,shade7))))
  `(helm-separator ((,class (:foreground ,accent5 :background ,shade0))))
  `(helm-time-zone-current ((,class (:foreground ,accent5 :background ,shade0))))
  `(helm-time-zone-home ((,class (:foreground ,accent5 :background ,shade0))))
  `(helm-buffer-not-saved ((,class (:foreground ,accent5 :background ,shade0))))
  `(helm-buffer-process ((,class (:foreground ,accent5 :background ,shade0))))
  `(helm-buffer-saved-out ((,class (:foreground ,shade7 :background ,shade0))))
  `(helm-buffer-size ((,class (:foreground ,shade7 :background ,shade0))))
  `(helm-ff-directory ((,class (:foreground ,accent6 :background ,shade0 :weight bold))))
  `(helm-ff-file ((,class (:foreground ,shade7 :background ,shade0 :weight normal))))
  `(helm-ff-executable ((,class (:foreground ,accent2 :background ,shade0 :weight normal))))
  `(helm-ff-invalid-symlink ((,class (:foreground ,accent7 :background ,shade0 :weight bold))))
  `(helm-ff-symlink ((,class (:foreground ,accent1 :background ,shade0 :weight bold))))
  `(helm-ff-prefix ((,class (:foreground ,shade0 :background ,accent1 :weight normal))))
  `(helm-grep-cmd-line ((,class (:foreground ,shade7 :background ,shade0))))
  `(helm-grep-file ((,class (:foreground ,shade7 :background ,shade0))))
  `(helm-grep-finish ((,class (:foreground ,shade6 :background ,shade0))))
  `(helm-grep-lineno ((,class (:foreground ,shade7 :background ,shade0))))
  `(helm-grep-match ((,class (:foreground nil :background nil :inherit helm-match))))
  `(helm-grep-running ((,class (:foreground ,accent6 :background ,shade0))))
  `(helm-moccur-buffer ((,class (:foreground ,accent6 :background ,shade0))))
  `(helm-source-go-package-godoc-description ((,class (:foreground ,accent3))))
  `(helm-bookmark-w3m ((,class (:foreground ,accent5))))
  `(company-echo-common ((,class (:foreground ,shade0 :background ,shade7))))
  `(company-preview ((,class (:background ,shade0 :foreground ,accent2))))
  `(company-preview-common ((,class (:foreground ,shade1 :foreground ,shade5))))
  `(company-preview-search ((,class (:foreground ,accent5 :background ,shade0))))
  `(company-scrollbar-bg ((,class (:background ,shade2))))
  `(company-scrollbar-fg ((,class (:foreground ,accent1))))
  `(company-tooltip ((,class (:foreground ,shade6 :background ,shade0 :bold t))))
  `(company-tooltop-annotation ((,class (:foreground ,accent4))))
  `(company-tooltip-common ((,class ( :foreground ,shade5))))
  `(company-tooltip-common-selection ((,class (:foreground ,accent3))))
  `(company-tooltip-mouse ((,class (:inherit highlight))))
  `(company-tooltip-selection ((,class (:background ,shade2 :foreground ,shade5))))
  `(company-template-field ((,class (:inherit region))))
  `(web-mode-builtin-face ((,class (:inherit ,font-lock-builtin-face))))
  `(web-mode-comment-face ((,class (:inherit ,font-lock-comment-face))))
  `(web-mode-constant-face ((,class (:inherit ,font-lock-constant-face))))
  `(web-mode-keyword-face ((,class (:foreground ,accent1))))
  `(web-mode-doctype-face ((,class (:inherit ,font-lock-comment-face))))
  `(web-mode-function-name-face ((,class (:inherit ,font-lock-function-name-face))))
  `(web-mode-string-face ((,class (:foreground ,accent3))))
  `(web-mode-type-face ((,class (:inherit ,font-lock-type-face))))
  `(web-mode-html-attr-name-face ((,class (:foreground ,accent6))))
  `(web-mode-html-attr-value-face ((,class (:foreground ,accent1))))
  `(web-mode-warning-face ((,class (:inherit ,font-lock-warning-face))))
  `(web-mode-html-tag-face ((,class (:foreground ,accent5))))
  `(jde-java-font-lock-package-face ((t (:foreground ,accent6))))
  `(jde-java-font-lock-public-face ((t (:foreground ,accent1))))
  `(jde-java-font-lock-private-face ((t (:foreground ,accent1))))
  `(jde-java-font-lock-constant-face ((t (:foreground ,accent4))))
  `(jde-java-font-lock-modifier-face ((t (:foreground ,accent7))))
  `(jde-jave-font-lock-protected-face ((t (:foreground ,accent1))))
  `(jde-java-font-lock-number-face ((t (:foreground ,accent6))))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
    (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'orange-purple-light)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; orange-purple-light-theme.el ends here
```
