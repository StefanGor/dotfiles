;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;;; TRIAL SECTION

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
;;                  `(csharp-mode . ((concat omnisharp-cache-directory "/server/v1.32.11/OmniSharp.exe" ) "-lsp")))))

;; (setq +ivy-buffer-preview t) ;; preview buffers when using spc b b

(global-set-key (kbd "<C-f2>") 'bm-toggle)
(global-set-key (kbd "<f2>")   'bm-next)
(global-set-key (kbd "<S-f2>") 'bm-previous)

(global-set-key (kbd "<left-fringe> <mouse-5>") 'bm-next-mouse)
(global-set-key (kbd "<left-fringe> <mouse-4>") 'bm-previous-mouse)
(global-set-key (kbd "<left-fringe> <mouse-1>") 'bm-toggle-mouse)
(setq bm-marker 'bm-marker-right)

;; stolen from discord
(defun quote-word-at-point ()
  (interactive)
  (cl-destructuring-bind (beg . end)
      (bounds-of-thing-at-point 'word)
    (evil-surround-region beg end nil ?\")))
(map! :n "g'" #'quote-word-at-point)

(define-key minibuffer-inactive-mode-map [mouse-1] #'ignore) ;https://www.reddit.com/r/emacs/comments/6smbgj/is_there_any_way_to_disable_opening_up_the/
;;evil-forward-arg?
(setq +word-wrap-extra-indent 'single)
(+global-word-wrap-mode +1)

;; from discord
(setq doom-themes-treemacs-theme "doom-colors")

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(which-function-mode 1)
(defcustom  my/which-function-max-width 32 ;; https://github.com/Atman50/emacs-config#speed-up-line-movement
  "The maximum width of the which-function string."
  :group 'my-configuration
  :type 'integer)
(advice-add #'which-function :filter-return
            (lambda (s) (when (stringp s)
                          (if (< (string-width s) my/which-function-max-width) s
                            (concat (truncate-string-to-width s (- my/which-function-max-width 3)) "...")))))

(setq auto-save-interval 20)
(setq auto-save-default t)
(setq make-backup-files t)
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(setq auto-window-vscroll nil) ;; https://github.com/Atman50/emacs-config#speed-up-line-movement
;; https://github.com/abo-abo/swiper/issues/551 doesnt do anything i think, does this even apply to fuzzy
(setq ivy-sort-matches-functions-alist '((t . nil)
                                         (ivy-completion-in-region)
                                         (ivy-switch-buffer . ivy-sort-function-buffer)
                                         (counsel-find-file . ivy-sort-function-buffer)))

(menu-bar-mode 1)
(setq which-key-idle-delay 0.2) ;; needs to be set before entering which-key-mode
(setq which-key-max-description-length 35)
(remove-hook 'after-change-major-mode-hook #'doom|highlight-non-default-indentation)  ;; disable yellow highlighting for inconsistent tabs/space

;; https://github.com/Alexander-Miller/treemacs/issues/411
;; (setq treemacs-python-executable "C:\\Python37\\python.exe")

;; may help with git command speed?
(setq w32-pipe-read-delay 0)

;; todo make this work - marks arent rendered nicely
;;(global-evil-fringe-mark-mode)
(setq-default evil-fringe-mark-show-special t)
;; (setq-default left-fringe-width 16)
(setq-default right-fringe-width 30)
(setq-default evil-fringe-mark-side 'right-fringe)
;; basename?
(setq counsel-projectile-find-file-matcher 'counsel-projectile-find-file-matcher-basename)

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
      :n "<f5>" (λ! (find-file "~/.doom.d/config.el"))
      :n "<f6>" #'neotree-toggle
      :n "<f7>" #'deadgrep
      :nvmi "<f8>" #'toggle-flycheck-error-buffer
      :n "<f9>" #'+treemacs/toggle

      "C-s" #'basic-save-buffer
      :nvmi "C-/" #'comment-line ;; this doesnt work well in visual mode
      :m "C-_" #'text-scale-decrease
      "C-+" #'text-scale-increase
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
      :nvi "M-k" #'c-beginning-of-defun
      :nvi "M-j" #'c-end-of-defun
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

;;; Package config
(after! org
  (setq
   ;; org-startup-folded nil ;; makes all sections expanded by default
   )
  )

(after! helm
  (setq
   helm-mode-fuzzy-match t
   helm-completion-in-region-fuzzy-match t
   ))

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
   ;; https://emacs.stackexchange.com/questions/36745/enable-ivy-fuzzy-matching-everywhere-except-in-swiper
   ;; https://oremacs.com/2016/04/26/ivy-0.8.0/
   ;; ivy-re-builders-alist '((counsel-ag . ivy--regex-plus) ;; non-fuzzy for SPC / p - quite slow
   ;;                         (t      . ivy--regex-fuzzy))
   ivy-use-ignore-default nil ;; dont ignore files that start with a dot. can toggle this with c-c c-a, or if it is off, start your search with a '.'

   ;; https://github.com/abo-abo/swiper/issues/925#issuecomment-335789390
   ;; counsel-grep-base-command "rg -i -M 120 --no-heading --line-number --color never '%s' %s" ;; broken
   ;; counsel-rg-base-command "rg -i -M 120 --no-heading --line-number --color never '%s' %s"
   ;; ivy 0.13 says to not use '-S'
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

;;; def-packages
(def-package! imenu-list
  :commands imenu-list-smart-toggle)
(map!
 :leader
 (:prefix "o"
   :desc "Imenu list" "i" #'imenu-list-smart-toggle))
