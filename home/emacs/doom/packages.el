;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;(package! some-package)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/radian-software/straight.el#the-recipe-format
;(package! another-package
;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;(package! this-package
;  :recipe (:host github :repo "username/repo"
;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;(package! builtin-package :recipe (:nonrecursive t))
;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see radian-software/straight.el#279)
;(package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
;(package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
;(unpin! pinned-package)
;; ...or multiple packages
;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
;(unpin! t)

;; disable multiple-cursors
(package! js2-refactor :disable t)

(package! nerd-fonts
  :pin "eb187fd3a356032f4796d92fbb12225d32c8d973"
  :recipe (:host github :repo "twlz0ne/nerd-fonts.el"))

(package! all-the-icons-nerd-fonts
  :pin "a66e9ed4682d59a04777a7e61578f4b2e74855e2"
  :recipe (:host github :repo "mohkale/all-the-icons-nerd-fonts"))

(package! dired-sidebar :pin "5569d3b53585f5413cf87a694650d0fd6e040803")

(package! ryo-modal :pin "b9e6a0f33b9e2aeb6088accd23ed312083d8f707")

(package! point-undo :pin "d875c94522ad6430294c37d627e332d726221e02")

;; local modules

(package! evil-core
  :recipe (:local-repo "./my"))

(package! char-occurrence
  :recipe (:local-repo "./my/commands"))

(package! open-line
  :recipe (:local-repo "./my/commands"))

(package! region-boundaries
  :recipe (:local-repo "./my/commands"))

(package! repeat-nth
  :recipe (:local-repo "./my/commands"))

(package! save-kmacro
  :recipe (:local-repo "./my/commands"))

(package! save-point
  :recipe (:local-repo "./my/commands"))

(package! secondary-selection
  :recipe (:local-repo "./my/commands"))

(package! smarter-commands
  :recipe (:local-repo "./my/commands"))

(package! text-object
  :recipe (:local-repo "./my/commands"))
