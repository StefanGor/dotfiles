;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here
;; (after! projecile
  (setq 
	projectile-indexing-method 'alien
	projectile-tags-command "ctags.exe -R -e --language-force=C#"
	projectile-enable-caching t
	)
  ;; )

(map! (:leader
      :desc "Find file in project" :nv "p f" #'projectile-find-file
      :desc "M-x" :n "SPC" #'execute-extended-command

      :desc "restart server" :n "m s" #'omnisharp-reload-solution
      :desc "run code refactoring" :n "m r" #'omnisharp-run-code-action-refactoring

      :desc "clear errors" :n "e c" #'flycheck-clear

      (:desc "jump" :prefix "j"
        :desc "character" :n "c" #'evil-avy-goto-char
        )

      )

      ;; non-leader bindings
      :m "]h" #'git-gutter:next-hunk
      :m "[h" #'git-gutter:previous-hunk
      :m "C-s" #'save-buffer
      :m "<f5>" (lambda () (interactive) (find-file "~/.doom.d/config.el"))

      ;; :desc "open config.el" :n "f e d" #'(find-file "~/.doom.d/config.el")
      )

(setq
	evil-escape-key-sequence "fd"
	doom-font (font-spec :family "Hack" :size 12)
    large-file-warning-threshold nil
    require-final-newline nil
    electric-indent-mode t
    treemacs-silent-refresh t
    inhibit-compacting-font-caches t
    omnisharp-expected-server-version "1.32.5"
)

(setq-default frame-title-format "Code")

;; web mode
(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-enable-block-face nil) ;; disable black bg for code in razor files
  )
(add-hook 'web-mode-hook  'my-web-mode-hook)

(add-to-list 'auto-mode-alist '("\\.cshtml$" . web-mode))

(add-hook 'c-mode-common-hook
          (lambda ()
            (c-set-offset 'arglist-cont-nonempty +)
            (c-offsets-alist 'arglist-close c-lineup-arglist)
            ))

(add-hook 'csharp-mode-hook
          (lambda ()
            ; suggested by https://github.com/josteink/csharp-mode
            ;; (electric-pair-mode 1) makes new methods not work correctly...
            ;; (electric-pair-local-mode 1)
            (c-set-offset 'substatement-open 0)
            (c-set-offset 'brace-list-open 0)
            ))

(define-key undo-tree-map (kbd "C-/") nil)
(define-key evil-motion-state-map (kbd "C-/") 'comment-line)

(defun transparent(alpha-level no-focus-alpha-level)
 (interactive "nAlpha level (0-100): \nnNo focus alpha level (0-100): ")
 (set-frame-parameter (selected-frame) 'alpha (list alpha-level no-focus-alpha-level))
 (add-to-list 'default-frame-alist `(alpha ,alpha-level)))

(transparent 100 95)

;; Frame geometry
(defun save-framegeometry ()
  "Gets the current frame's geometry and saves to ~/.doom.d/framegeometry."
  (let (
        (framegeometry-left (frame-parameter (selected-frame) 'left))
        (framegeometry-top (frame-parameter (selected-frame) 'top))
        (framegeometry-width (frame-parameter (selected-frame) 'width))
        (framegeometry-height (frame-parameter (selected-frame) 'height))
        (framegeometry-file (expand-file-name "~/.doom.d/framegeometry"))
        )

    (when (not (number-or-marker-p framegeometry-left))
      (setq framegeometry-left 0))
    (when (not (number-or-marker-p framegeometry-top))
      (setq framegeometry-top 0))
    (when (not (number-or-marker-p framegeometry-width))
      (setq framegeometry-width 0))
    (when (not (number-or-marker-p framegeometry-height))
      (setq framegeometry-height 0))

    (with-temp-buffer
      (insert
       ";;; This is the previous emacs frame's geometry.\n"
       ";;; Last generated " (current-time-string) ".\n"
       "(setq initial-frame-alist\n"
       "      '(\n"
       (format "        (top . %d)\n" (max framegeometry-top 0))
       (format "        (left . %d)\n" (max framegeometry-left 0))
       (format "        (width . %d)\n" (max framegeometry-width 0))
       (format "        (height . %d)))\n" (max framegeometry-height 0)))
      (when (file-writable-p framegeometry-file)
        (write-file framegeometry-file))))
  )

(defun load-framegeometry ()
  "Loads ~/.doom.d/framegeometry which should load the previous frame's
geometry."
  (let ((framegeometry-file (expand-file-name "~/.doom.d/framegeometry")))
    (when (file-readable-p framegeometry-file)
      (load-file framegeometry-file)))
)

(if window-system
  (progn
	(add-hook 'after-init-hook 'load-framegeometry)
	(add-hook 'kill-emacs-hook 'save-framegeometry))
)
