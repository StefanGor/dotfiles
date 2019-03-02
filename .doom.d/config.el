;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; TODO
;; do all mappings the doom emacs way
;; use after! for mappings
;; move framegeometry file out into its own file
;; tweak counsel-rg-base-command? i want to ignore the directories a path is under with fuzzy search?
;; customise pretty-code-symbols-alist
;; disable magit or get emacs sqlite "emacsql-sqlite-ensure-binary: No EmacSQL SQLite binary available, aborting
;; moving up and down candidates when selecting buffers is quite slow - is ivy-rich to blame?
;; do you need a lambda in add-hook!

(map! (:leader
      :desc "Find file in project" :nv "p f" #'projectile-find-file
      :desc "M-x" :n "SPC" #'execute-extended-command

      :desc "restart server" :n "m s" #'omnisharp-reload-solution
      :desc "run code refactoring" :n "m r" #'omnisharp-run-code-action-refactoring

      :desc "clear errors" :n "e c" #'flycheck-clear

      :desc "find symbol" :n "m f s" #'omnisharp-helm-find-symbols

      :desc "toggle wrap" :n "t w" #'toggle-truncate-lines

      (:desc "jump" :prefix "j"
        :desc "character" :n "c" #'evil-avy-goto-char))

      ;; non-leader bindings
      :m "]h" #'git-gutter:next-hunk
      :m "[h" #'git-gutter:previous-hunk
      :m "C-s" #'save-buffer
      :m "<f5>" (lambda () (interactive) (find-file "~/.doom.d/config.el"))
      :m "gi" #'omnisharp-find-implementations
      :m "<f6>" #'treemacs
      :m "C-/" #'comment-line
      :m "C-_" #'text-scale-decrease
      :m "C-+" #'text-scale-increase
)

(after! projectile
  (setq
   projectile-indexing-method 'alien
   ;; do I even need tags
   projectile-tags-command "ctags.exe -R -e --language-force=C#"
   projectile-enable-caching t
   projectile-globally-ignored-file-suffixes '(".exe" ".dll")
   projectile-globally-ignored-files '("TAGS" "tags")
   )
  )

(after! ivy
  (setq
   ;; https://emacs.stackexchange.com/questions/36745/enable-ivy-fuzzy-matching-everywhere-except-in-swiper
   ;; https://oremacs.com/2016/04/26/ivy-0.8.0/
   ivy-re-builders-alist '((counsel-ag . ivy--regex-plus) ;; non-fuzzy for SPC / p - quite slow
                           (t      . ivy--regex-fuzzy))
   ivy-use-ignore-default nil ;; dont ignore files that start with a dot. can toggle this with c-c c-a, or if it is off, start your search with a '.'
   )
  )

(setq
 evil-escape-key-sequence "fd"
 doom-font (font-spec :family "Hack" :size 14)
 large-file-warning-threshold nil ;; Warning about opening tags file
 electric-indent-mode t
 treemacs-silent-refresh t
 inhibit-compacting-font-caches t
 omnisharp-expected-server-version "1.32.5" ;; fix omnisharp-emacs 'already exists' issue
 helm-buffer-max-length nil
 mode-require-final-newline nil ;; Stop emacs adding new lines at EOF?
 require-final-newline nil
 display-line-numbers-type 'relative
 )

(setq-default
 frame-title-format "Code"
 )

(add-hook! web-mode
  (setq web-mode-enable-block-face nil) ;; disable black bg for code in razor files
  )

(add-hook! c-mode
  (lambda ()
    (c-set-offset 'arglist-cont-nonempty +)
    (c-offsets-alist 'arglist-close c-lineup-arglist)
    )
  )

(add-hook! csharp-mode
  (lambda ()
    ;; suggested by https://github.com/josteink/csharp-mode
    ;; (electric-pair-mode 1) makes new methods not work correctly...
    ;; (electric-pair-local-mode 1)
    (c-set-offset 'substatement-open 0)
    (c-set-offset 'brace-list-open 0)
    )
  )

(add-to-list 'auto-mode-alist '("\\.cshtml$" . web-mode))

(defun transparent(alpha-level no-focus-alpha-level)
 (interactive "nAlpha level (0-100): \nnNo focus alpha level (0-100): ")
 (set-frame-parameter (selected-frame) 'alpha (list alpha-level no-focus-alpha-level))
 (add-to-list 'default-frame-alist `(alpha ,alpha-level)))

(transparent 100 95)

(load! "restoreframe")
