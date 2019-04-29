;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; TODO
;; use after!/localleader/:map for mappings
;; customise pretty-code-symbols-alist
;; disable magit or get emacs sqlite "emacsql-sqlite-ensure-binary: No EmacSQL SQLite binary available, aborting
;; moving up and down candidates when selecting buffers is quite slow - is ivy-rich to blame?
;; do you need a lambda in add-hook!
;; doom-modeline config? seagle's one is now being used
;; projectile-switch-project-action ? may not be forced to select a file
;; might not need spaces between 'p' and 'f' etc?

(map! (:leader
      :desc "Find file in project" :nv "p f" #'projectile-find-file
      :desc "M-x" :n "SPC" #'execute-extended-command

      :desc "restart server" :n "m s" #'omnisharp-reload-solution
      :desc "run code refactoring" :n "m r" #'omnisharp-run-code-action-refactoring

      :desc "clear errors" :n "e c" #'flycheck-clear

      ;; :desc "find symbol" :n "m f s" #'omnisharp-helm-find-symbols ;; should be in map

      :desc "toggle wrap" :n "t w" #'toggle-truncate-lines
      :desc "toggle debug on error" :n "t d" #'toggle-debug-on-error

      (:desc "jump" :prefix "j"
        :desc "character" :n "c" #'evil-avy-goto-char))

      ;; non-leader bindings
      :n "<f4>" (lambda () (interactive) (find-file "~/Desktop/Other/notes/scratchpad-em.txt"))
      :n "<f5>" (lambda () (interactive) (find-file "~/.doom.d/config.el"))
      ;; :n "<f6>" #'ranger
      :n "<f7>" #'treemacs ;; broken?
      :n "]h" #'git-gutter:next-hunk
      :n "[h" #'git-gutter:previous-hunk
      "C-s" #'save-buffer
      :n "gi" #'omnisharp-find-implementations ;; should be in map
      :m "C-/" #'comment-line ;; :m works better than :nv - it does one line too many in visual mode
      :m "C-_" #'text-scale-decrease ;; same with this one
      "C-+" #'text-scale-increase
      :m "M-_" #'downscale-all ;; :m makes it override undo-tree
      "M-+" #'upscale-all
)

;; frame font sizes - https://stackoverflow.com/a/24809045
(defun downscale-all()
  (interactive)
  (let ((old-face-attribute (face-attribute 'default :height)))
    (set-face-attribute 'default nil :height (- old-face-attribute 10))))

(defun upscale-all()
  (interactive)
  (let ((old-face-attribute (face-attribute 'default :height)))
    (set-face-attribute 'default nil :height (+ old-face-attribute 10))))

(after! helm
  (setq
   helm-mode-fuzzy-match t
   helm-completion-in-region-fuzzy-match t
   ))

(after! doom-modeline
  (setq
   doom-modeline-buffer-file-name-style 'relative-to-project
   )
   (doom-modeline-def-modeline 'main
   '(bar matches buffer-info buffer-position selection-info)
   '(misc-info major-mode process vcs checker))
   )

;; only here for direct comparison
;; (doom-modeline-def-modeline 'main
;;   '(bar matches buffer-info remote-host buffer-position selection-info)
;;   '(misc-info persp-name irc mu4e github debug indent input-method buffer-encoding lsp major-mode process vcs checker))
(after! projectile
  (setq
   projectile-indexing-method 'alien
   ;; do I even need tags, probably not tbh
   ;; projectile-tags-command "ctags.exe -R -e --language-force=C#"
   projectile-enable-caching t
   projectile-globally-ignored-file-suffixes '(".exe" ".dll") ;; TODO this doesnt work...
   projectile-globally-ignored-files '("TAGS" "tags")
   )
  )

(after! ivy
  (setq
   ;; https://emacs.stackexchange.com/questions/36745/enable-ivy-fuzzy-matching-everywhere-except-in-swiper
   ;; https://oremacs.com/2016/04/26/ivy-0.8.0/
   ;; ivy-re-builders-alist '((counsel-ag . ivy--regex-plus) ;; non-fuzzy for SPC / p - quite slow
   ;;                         (t      . ivy--regex-fuzzy))
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
 frame-title-format "%b"
 )

(add-hook! web-mode
  (setq web-mode-enable-block-face nil) ;; disable black bg for code in razor files
  )

;; todo do I still need both of these hooks and if so, what do they do?
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
(add-to-list 'auto-mode-alist '("\\.config$" . fundamental-mode)) ;; xml mode really slow

(defun transparent(alpha-level no-focus-alpha-level)
 (interactive "nAlpha level (0-100): \nnNo focus alpha level (0-100): ")
 (set-frame-parameter (selected-frame) 'alpha (list alpha-level no-focus-alpha-level))
 (add-to-list 'default-frame-alist `(alpha ,alpha-level)))

(transparent 100 95)

(load! "restoreframe")
