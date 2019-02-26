;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; TODO
;; investigate ivy's rg command - is it using --follow for symlinks
;; do all mappings the doom emacs way
;; use after!
;; move framegeometry file out into its own file

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

      :desc "find symbol" :n "m f s" #'omnisharp-helm-find-symbols

      (:desc "jump" :prefix "j"
        :desc "character" :n "c" #'evil-avy-goto-char))

      ;; non-leader bindings
      :m "]h" #'git-gutter:next-hunk
      :m "[h" #'git-gutter:previous-hunk
      :m "C-s" #'save-buffer
      :m "<f5>" (lambda () (interactive) (find-file "~/.doom.d/config.el"))
      :m "gi" #'omnisharp-find-implementations
      :m "<f6>" #'treemacs
)

(setq
 evil-escape-key-sequence "fd"
 doom-font (font-spec :family "Hack" :size 14)
 large-file-warning-threshold nil
 require-final-newline nil
 electric-indent-mode t
 treemacs-silent-refresh t
 inhibit-compacting-font-caches t
 omnisharp-expected-server-version "1.32.5"
 helm-buffer-max-length nil
 mode-require-final-newline nil ;; Stop emacs adding new lines at EOF?
 require-final-newline nil
 display-line-numbers-type 'relative
 ;; https://emacs.stackexchange.com/questions/36745/enable-ivy-fuzzy-matching-everywhere-except-in-swiper
 ;; https://oremacs.com/2016/04/26/ivy-0.8.0/
 ivy-re-builders-alist '((counsel-ag . ivy--regex-plus) ;; non-fuzzy for SPC / p - quite slow
                         (t      . ivy--regex-fuzzy))
 )

(setq-default
 frame-title-format "Code"
 )

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

;; todo bind properly? copied this from spacemacs config
(define-key undo-tree-map (kbd "C-/") nil)
(define-key undo-tree-map (kbd "C-_") nil)
(define-key evil-motion-state-map (kbd "C-/") 'comment-line)

;; frame font sizes - https://stackoverflow.com/a/24809045
(global-set-key (kbd "C-_")
                (lambda ()
                  (interactive)
                  (let ((old-face-attribute (face-attribute 'default :height)))
                    (set-face-attribute 'default nil :height (- old-face-attribute 10)))))

(global-set-key (kbd "C-+")
                (lambda ()
                  (interactive)
                  (let ((old-face-attribute (face-attribute 'default :height)))
                    (set-face-attribute 'default nil :height (+ old-face-attribute 10)))))


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
