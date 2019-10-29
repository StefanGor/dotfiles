;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;;; Trial section

;; (use-package eglot
;;   :commands (eglot eglot-ensure)
;;   :hook ((python-mode . eglot-ensure)
;;          (csharp-mode . eglot-ensure))
;;   :config
;;   (progn
;;     (define-key eglot-mode-map (kbd "C-c e r") 'eglot-rename)
;;     (define-key eglot-mode-map (kbd "C-c e f") 'eglot-format)
;;     (define-key eglot-mode-map (kbd "C-c e h") 'eglot-help-at-point)
;;     (add-to-list 'eglot-server-programs
;;                  `(csharp-mode . ((concat omnisharp-cache-directory "server/v1.34.3/OmniSharp.exe") "-lsp")))
;;       ;; patch the argument. When nil, use "" instead.
;;     (defun eglot--format-markup-patch (args)
;;       (list (or (car args) "")))
;;     (advice-add 'eglot--format-markup :filter-args #'eglot--format-markup-patch)))

(define-key minibuffer-inactive-mode-map [mouse-1] #'ignore) ;https://www.reddit.com/r/emacs/comments/6smbgj/is_there_any_way_to_disable_opening_up_the/

(setq +word-wrap-extra-indent 'single)
(+global-word-wrap-mode +1)

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(setq auto-save-interval 20)
(setq auto-save-default t)
(setq make-backup-files t)
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(setq auto-window-vscroll nil) ;; https://github.com/Atman50/emacs-config#speed-up-line-movement

(menu-bar-mode 1)
(setq which-key-idle-delay 0.2) ;; needs to be set before entering which-key-mode
(setq which-key-max-description-length 35)
(remove-hook 'after-change-major-mode-hook #'doom|highlight-non-default-indentation)  ;; disable yellow highlighting for inconsistent tabs/space

;; https://github.com/hlissner/doom-emacs/issues/1568
(setq-hook! '(prog-mode-hook text-mode-hook conf-mode-hook) show-trailing-whitespace nil)

;;; Mappings
(map! (:leader
      :desc "Find file in project" :nv "." #'projectile-find-file
      :desc "M-x" :n "SPC" #'execute-extended-command

      :desc "clear errors" :n "e c" #'flycheck-clear
      :desc "check buffer" :n "e e" #'flycheck-buffer
      :desc "refresh errors" :n "e r" (λ! (flycheck-clear) (flycheck-buffer))

      :desc "toggle wrap" :n "t w" #'toggle-truncate-lines
      :desc "toggle debug on error" :n "t d" #'toggle-debug-on-error

      (:desc "jump" :prefix "j"
        :desc "character" :n "c" #'evil-avy-goto-char))

      ;; non-leader bindings
      :n "g'" #'quote-word-at-point
      :n "<f5>" (λ! (find-file "~/.doom.d/config.el"))
      :n "<f6>" #'neotree-toggle
      :n "<f7>" #'deadgrep
      :nvmi "<f8>" #'toggle-flycheck-error-buffer
      :n "<f9>" #'+treemacs/toggle

      "C-s" #'basic-save-buffer
      :nvmi "C-/" #'comment-line ;; this doesnt work well in visual mode
      :m "M-_" #'doom/decrease-font-size ;; :m makes it override undo-tree
      "M-+" #'doom/increase-font-size

      :n "<C-tab>" #'evil-switch-to-windows-last-buffer

      :n "]h" #'git-gutter:next-hunk
      :n "[h" #'git-gutter:previous-hunk
      ;; I don't like jumping around between files with [e
      :n "[e" #'flycheck-previous-error
      :n "]e" #'flycheck-next-error
      :n "]E" #'previous-error
      :n "[E" #'next-error
      :nvi "M-j" (λ! (beginning-of-defun -1)) ;; theres actually a evil/next-beginning-of-method btw
      :nvi "M-k" #'beginning-of-defun
)

(map! :map csharp-mode-map
      ;; (:localleader ;; TODO merge non-localleader with normal map??
      ;;   :n "e" :desc "solution errors" #'omnisharp-solution-errors)
      :i "C-." #'omnisharp-add-dot-and-auto-complete
      :nvmi "<M-return>" #'omnisharp-run-code-action-refactoring
      :n "gi" #'omnisharp-find-implementations
      :n "gu" #'omnisharp-find-usages
      ;; :n "gd" #'omnisharp-go-to-definition
      :n "go" #'omnisharp-go-to-definition-other-window)

(map! :map csharp-mode-map ;; needs to be csharp-mode-map or lambda descriptions
      :localleader
      :desc "refactor" :n "<return>" #'omnisharp-run-code-action-refactoring
      :desc "errors" :n "e"  (lambda () (interactive) (omnisharp-solution-errors t))
)

(map! :map emacs-lisp-mode-map
      :localleader
      :n "v" #'eval-defun)

(map! :map ivy-switch-buffer-map
      "C-c C-k" #'ivy-switch-buffer-kill) ;; This is on C-o C-k

(map! :map ivy-minibuffer-map
      "C-i" #'ivy-rotate-preferred-builders)

;;; Package config
(after! org
  (setq
   ;; org-startup-folded nil ;; makes all sections expanded by default
   )
  )

(after! doom-modeline
  (setq
   doom-modeline-buffer-file-name-style 'file-name
   doom-modeline-buffer-encoding nil
   )
   (doom-modeline-def-modeline 'main
   '(bar matches buffer-info)
   '(misc-info major-mode process vcs checker))
   )

;; only here for direct comparison
;; (doom-modeline-def-modeline 'main
;;   '(bar matches buffer-info remote-host buffer-position selection-info)
;;   '(misc-info persp-name irc mu4e github debug indent input-method buffer-encoding lsp major-mode process vcs checker))

(after! projectile
  (setq
   projectile-indexing-method 'alien
   projectile-enable-caching t
   projectile-globally-ignored-file-suffixes '(".exe" ".dll") ;; TODO this doesnt work...
   projectile-globally-ignored-files '("TAGS" "tags")
   )
  )

(after! ivy
  (setq
   ivy-use-ignore-default nil
   counsel-rg-base-command "rg -M 140 --no-heading --line-number --color never %s ."
   )
  )

(after! evil
  (setq-default
   evil-escape-key-sequence "fd"
   )
  (setq
   evil-want-fine-undo t)
  )

(after! evil-owl
  (setq evil-owl-extra-posframe-args '(:width 50 :height 20)
        evil-owl-max-string-length 50))
(evil-owl-mode)

(after! company
  (setq
   company-idle-delay 0.2
   company-tooltip-idle-delay 0.2
   company-search-regexp-function #'company-search-flex-regexp ;; doesnt do anything?
   ))

;;; Generic setq
(setq
 evil-escape-key-sequence "fd"
 doom-font (font-spec :family "Hack" :size 12)
 large-file-warning-threshold nil ;; Warning about opening tags file
 electric-indent-mode t
 treemacs-silent-refresh t
 inhibit-compacting-font-caches t
 mode-require-final-newline nil ;; Stop emacs adding new lines at EOF?
 require-final-newline nil
 display-line-numbers-type 'relative
 treemacs-git-mode 'deferred
 sql-product 'ms
 w32-pipe-read-delay 0
 )

(setq-default
 frame-title-format "%b"
 )

(add-hook! web-mode
  (setq web-mode-enable-block-face nil) ;; disable black bg for code in razor files
  )

(add-hook! csharp-mode
  ;; (lambda ()
    ;; suggested by https://github.com/josteink/csharp-mode
    ;; (electric-pair-mode 1) makes new methods not work correctly...
    ;; (electric-pair-local-mode 1)
    (c-set-offset 'arglist-intro '+) ;; First argument after open bracket (for constructors)
    (c-set-offset 'arglist-cont-nonempty '+)
    ;; (c-offsets-alist 'arglist-close 'c-lineup-arglist)
    (c-set-offset 'substatement-open 0)
    (c-set-offset 'brace-list-open 0)
    (setq lsp-clients-csharp-language-server-path (expand-file-name "~/.omnisharp/omnisharp-win-x64/OmniSharp.exe"))
    ;; )
  )

(add-to-list 'auto-mode-alist '("\\.cshtml$" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.config$" . fundamental-mode)) ;; xml mode really slow

;;; Functions
(defun transparent(alpha-level no-focus-alpha-level)
 (interactive "nAlpha level (0-100): \nnNo focus alpha level (0-100): ")
 (set-frame-parameter (selected-frame) 'alpha (list alpha-level no-focus-alpha-level))
 (add-to-list 'default-frame-alist `(alpha ,alpha-level)))
(transparent 100 95)

(load! "restoreframe")

;; https://emacs.stackexchange.com/a/336
(setq compilation-finish-function
  (lambda (buf str)
    (if (null (string-match ".*exited abnormally.*" str))
        ;;no errors, make the compilation window go away in a few seconds
        (progn
          (run-at-time
           "2 sec" nil 'delete-windows-on
           (get-buffer-create "*compilation*"))
          (message "No Compilation Errors!")))))

;; fix 'Symbol’s function definition is void: compilation--default-buffer-name' error???
(defun compilation--default-buffer-name (name-of-mode)
  (cond ((or (eq major-mode (intern-soft name-of-mode))
             (eq major-mode (intern-soft (concat name-of-mode "-mode"))))
   (buffer-name))
  (t
   (concat "*" (downcase name-of-mode) "*"))))


(defun toggle-flycheck-error-buffer ()
  "toggle a flycheck error buffer."
  (interactive)
  (if (string-match-p "Flycheck errors" (format "%s" (window-list)))
      (dolist (w (window-list))
        (when (string-match-p "*Flycheck errors*" (buffer-name (window-buffer w)))
          (delete-window w)
          ))
    (flycheck-list-errors)
    )
  )

(defun quote-word-at-point ()
  (interactive)
  (cl-destructuring-bind (beg . end)
      (bounds-of-thing-at-point 'word)
    (evil-surround-region beg end nil ?\")))

;;; def-packages
(def-package! imenu-list
  :commands imenu-list-smart-toggle)
(map!
 :leader
 (:prefix "o"
   :desc "Imenu list" "i" #'imenu-list-smart-toggle))

;;; Trial section 2
(setq completion-ignore-case t)

;; C-x r w <letter> to save window layout to register, C-x r j <letter> to go back.

;; https://emacs.stackexchange.com/a/10446
(defun my-hl-line-range-function ()
  (cons (line-end-position) (line-beginning-position 2)))
(setq hl-line-range-function #'my-hl-line-range-function)

(when window-system
  (require 'hl-line)
  ;; (set-face-attribute 'hl-line nil :inherit nil :background "dark grey")
  (setq global-hl-line-sticky-flag t)
  (global-hl-line-mode 1))

;; describe-face doesnt work well with hl-line-mode: use this or SPC u C-x = instead.
(defun what-is-this-face-called ()
  (interactive)
  (message "This face is called: %s" (or (plist-get (text-properties-at (point)) 'face) "default")))

(setq
 company-show-numbers t
 company-selection-wrap-around t
 company-search-regexp-function 'company-search-flex-regexp
 omnisharp-auto-complete-want-importable-types t
 omnisharp-company-ignore-case nil ;; when does this work
 omnisharp-company-match-type 'company-match-server
 omnisharp-completing-read-function 'ivy-completing-read ;; what odes this actually do
 omnisharp-imenu-support t ;; what odes this actually do
 )