;; -*- no-byte-compile: t; -*-
;;; ~/.doom.d/packages.el

;;; Examples:
;; (package! some-package)
;; (package! another-package :recipe (:fetcher github :repo "username/repo"))
;; (package! builtin-package :disable t)

;; (package! evil-fringe-mark)
;; (package! snails :recipe (:host github :repo "manateelazycat/snails" :files ("*.el"))) ;; doesnt work with straight
(package! dired-sidebar)
(package! deadgrep)
(package! evil-owl)
(package! forge :disable t)
(package! undo-propose)
(package! imenu-list)

;; https://github.com/hlissner/doom-emacs/blob/develop/docs/faq.org#installing-a-third-party-theme doesnt work
;; (package! vscode-dark-theme :recipe (:host github :repo "ianpan870102/vscode-dark-emacs-theme"))
;; (package! vscode-dark-plus-theme) ;; i dont know how to load a custom theme
;; (package! vscode-dark-plus-theme :recipe (:host github :repo "ianpan870102/vscode-dark-plus-emacs-theme"))
