;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;;; Trial section

;; Close compilation buffer when its done - stopped working
;; https://emacs.stackexchange.com/a/336
;; (setq compilation-finish-function
;;   (lambda (buf str)
;;     (if (null (string-match ".*exited abnormally.*" str))
;;         ;;no errors, make the compilation window go away in a few seconds
;;         (progn
;;           (run-at-time
;;            "2 sec" nil 'delete-windows-on
;;            (get-buffer-create "*compilation*"))
;;           (message "No Compilation Errors!")))))

;; Doesnt work either - says buffer is dedicated
(defun bury-compile-buffer-if-successful (buffer string)
  "Bury a compilation buffer if succeeded without warnings "
  (if (and
       (string-match "compilation" (buffer-name buffer))
       (string-match "finished" string)
       (not
        (with-current-buffer buffer
          (search-forward "warning" nil t))))
      (run-with-timer 1 nil
                      (lambda (buf)
                        (bury-buffer buf)
                        (switch-to-prev-buffer (get-buffer-window buf) 'kill))
                      buffer)))
(add-hook 'compilation-finish-functions 'bury-compile-buffer-if-successful)

;; (require sharper)
(global-set-key (kbd "C-c n") 'sharper-main-transient) ;; For "n" for "dot NET"

;; https://github.com/hlissner/doom-emacs/issues/2724
(after! evil-easymotion
  (put 'visible-buffer 'bounds-of-thing-at-point (lambda () (cons (window-start) (window-end))))
  (evilem-make-motion evilem-motion-forward-word-begin #'evil-forward-word-begin :scope 'visible-buffer)
  (evilem-make-motion evilem-motion-forward-WORD-begin #'evil-forward-WORD-begin :scope 'visible-buffer)
  (evilem-make-motion evilem-motion-forward-word-end #'evil-forward-word-end :scope 'visible-buffer)
  (evilem-make-motion evilem-motion-forward-WORD-end #'evil-forward-WORD-end :scope 'visible-buffer)
  (evilem-make-motion evilem-motion-backward-word-begin #'evil-backward-word-begin :scope 'visible-buffer)
  (evilem-make-motion evilem-motion-backward-WORD-begin #'evil-backward-WORD-begin :scope 'visible-buffer)
  (evilem-make-motion evilem-motion-backward-word-end #'evil-backward-word-end :scope 'visible-buffer)
  (evilem-make-motion evilem-motion-backward-WORD-end #'evil-backward-WORD-end :scope 'visible-buffer))

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/") ;; custom vscode themes
(load-theme 'vscode-dark-plus t)

;; https://github.com/hlissner/doom-emacs/issues/2648#issuecomment-593474410
(after! counsel
  (ivy-add-actions
   #'counsel-rg
   '(("a" (lambda (_path) (mapc #'counsel-git-grep-action ivy--all-candidates))
      "Open all matches"))))

(after! lsp-ui
  (setq lsp-ui-sideline-enable nil
        lsp-ui-sideline-show-hover nil)

  ;; https://www.reddit.com/r/emacs/comments/cejhmy/what_are_the_best_alternatives_for_lspui_or/eu40wb3/
  (mapcar (lambda (f) (set-face-foreground f "dim gray"))
          '(lsp-ui-sideline-code-action lsp-ui-sideline-current-symbol lsp-ui-sideline-symbol lsp-ui-sideline-symbol-info))
  ) ; gets in the way in two-window split

(after! flycheck
  (set-popup-rule! "^\\*Flycheck errors\\*" :select nil)) ;dont select

(define-key minibuffer-inactive-mode-map [mouse-1] #'ignore) ;https://www.reddit.com/r/emacs/comments/6smbgj/is_there_any_way_to_disable_opening_up_the/

(setq +word-wrap-extra-indent 'single)
;; (+global-word-wrap-mode +1)

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(menu-bar-mode 1)

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
        :desc "character" :n "c" #'evil-avy-goto-char-2))

      ;; non-leader bindings
      :n "gi" #'+lookup/implementations
      :n "g'" #'quote-word-at-point
      :n "<f5>" (λ! (find-file "~/.doom.d/config.el"))
      :n "<f6>" #'neotree-toggle
      :n "<f7>" #'deadgrep
      :nvmi "<f8>" #'toggle-flycheck-error-buffer
      ;; :n "<f9>" #'+treemacs/toggle ;; really slow on big project
      :nvmi "<f9>" #'doom/toggle-profiler

      "C-s" #'basic-save-buffer
      :nvmi "C-/" #'evilnc-comment-or-uncomment-lines ;; this doesnt work well in visual mode
      "M-_" #'doom/decrease-font-size ;; :m makes it override undo-tree
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
      :i "C-v" #'evil-paste-after
      :n "C-M-j" #'drag-stuff-down
      :n "C-M-k" #'drag-stuff-up
      :i "C-z" #'undo-fu-only-undo
)

(map! :leader "/" doom-leader-search-map) ; reinstate SPC /

(after! lsp ;; this doesnt work
  (map! (:leader
          :n "ch" #'lsp-ui-doc-glance))
  )

(map!
 :n "go" #'sg-find-definition-other-window
 :n "<M-RET>" #'lsp-execute-code-action)

;; TODO remove omnisharp section after I fully cutover to LSP
;; (after! omnisharp
;;   (map! :map omnisharp-mode-map
;;         ;; (:localleader ;; TODO merge non-localleader with normal map??
;;         ;;   :n "e" :desc "solution errors" #'omnisharp-solution-errors)
;;         :i "C-." #'omnisharp-add-dot-and-auto-complete
;;         :nvmi "<M-return>" #'omnisharp-run-code-action-refactoring
;;         :n "gi" #'omnisharp-find-implementations
;;         :n "gu" #'omnisharp-find-usages
;;         :n "go" #'omnisharp-go-to-definition-other-window)

;;   (map! :map csharp-mode-map ;; needs to be csharp-mode-map or lambda descriptions
;;         :localleader
;;         :desc "refactor" :n "<return>" #'omnisharp-run-code-action-refactoring
;;         :desc "errors" :n "e"  (lambda () (interactive) (omnisharp-solution-errors t))))

(map! :map emacs-lisp-mode-map
      :localleader
      :n "v" #'eval-defun)

 ;; This is on C-o C-k. This sucks now - brings up other buffers after use
 ;; Related: if you type a space the backspace the same thing happens
(map! :map ivy-switch-buffer-map
      "C-c C-k" #'ivy-switch-buffer-kill)

(defun mu-ivy-kill-buffer ()
  (interactive)
  (ivy-set-action 'kill-buffer)
  (ivy-done))

;; (define-key ivy-switch-buffer-map (kbd "C-i") 'mu-ivy-kill-buffer)

(map! :map org-mode-map
      "C-c t" #'org-timestamp-now
      "C-c d" #'org-timestamp-now-done)

;;; Package config
(after! org
  (setq
   org-log-done 'time ;; show time when marking task as done
   ;; org-startup-folded nil ;; makes all sections expanded by default
   )
  )

(after! doom-modeline
  (setq
   doom-modeline-buffer-file-name-style 'file-name
   doom-modeline-buffer-encoding nil
   )
   (doom-modeline-def-modeline 'minimal
   '(bar matches buffer-info)
   '(misc-info major-mode process vcs checker))

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
   projectile-globally-ignored-file-suffixes '(".exe" ".dll")
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

(after! company
  (setq
   company-idle-delay 0.1
   company-tooltip-idle-delay 0.1
   ))

;;; Generic setq
(setq
 evil-escape-key-sequence "fd"
 doom-font (font-spec :family "Hack" :size 20)
 large-file-warning-threshold nil ;; Warning about opening tags file
 electric-indent-mode t
 treemacs-silent-refresh t
 inhibit-compacting-font-caches t
 mode-require-final-newline nil ;; Stop emacs adding new lines at EOF?
 require-final-newline nil
 display-line-numbers-type 'relative
 sql-product 'ms
 w32-pipe-read-delay 0
 auto-window-vscroll nil ;; https://github.com/Atman50/emacs-config#speed-up-line-movement
 evil-split-window-below t ;; focus other window when splitting
 evil-vsplit-window-right t
 auto-save-interval 20
 auto-save-default t
 make-backup-files t
 backup-directory-alist `((".*" . ,temporary-file-directory))
 auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
 which-key-idle-delay 0.2
 which-key-idle-secondary-delay 0.2
 which-key-max-description-length 35
 )

(setq-default
 frame-title-format "%b"
 )

;;; Hooks
(add-hook! web-mode
  (setq web-mode-enable-block-face nil) ;; disable black bg for code in razor files
  )

(add-hook! csharp-mode
  ;; (lambda ()
    ;; suggested by https://github.com/josteink/csharp-mode
    (c-set-offset 'arglist-intro '+) ;; First argument after open bracket (for constructors)
    (c-set-offset 'arglist-cont-nonempty '+)
    (c-set-offset 'substatement-open 0)
    (c-set-offset 'brace-list-open 0)
  )

(remove-hook 'text-mode-hook 'auto-fill-mode) ; doom does this, i dont like it

(add-to-list 'auto-mode-alist '("\\.cshtml$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.json.config$" . json-mode))
;; (add-to-list 'auto-mode-alist '("\\.config$" . fundamental-mode)) ;; xml mode really slow

;;; Functions
(defun transparent(alpha-level no-focus-alpha-level)
 (interactive "nAlpha level (0-100): \nnNo focus alpha level (0-100): ")
 (set-frame-parameter (selected-frame) 'alpha (list alpha-level no-focus-alpha-level))
 (add-to-list 'default-frame-alist `(alpha ,alpha-level)))
(transparent 100 95)

(load! "restoreframe")

(defun toggle-flycheck-error-buffer ()
  "toggle a flycheck error buffer."
  (interactive)
  (if (string-match-p "Flycheck errors" (format "%s" (window-list)))
      (dolist (w (window-list))
        (when (string-match-p "*Flycheck errors*" (buffer-name (window-buffer w)))
          (delete-window w)
          ))
    (flycheck-list-errors)))

(defun quote-word-at-point ()
  (interactive)
  (cl-destructuring-bind (beg . end)
      (bounds-of-thing-at-point 'word)
    (evil-surround-region beg end nil ?\")))

(defun org-timestamp-now ()
  (interactive)
  (re-search-forward "$")
  (org-insert-time-stamp (current-time) t t " "))

(defun org-timestamp-now-done ()
  (interactive)
  (re-search-forward "$")
  (org-insert-time-stamp (current-time) t t " DONE "))

(defun sg-find-definition-other-window ()
  (interactive)
  (delete-other-windows)
  (evil-window-vsplit)
  (lsp-find-definition))

;;; def-packages
(use-package! imenu-list
  :commands imenu-list-smart-toggle)
(map!
 :leader
 (:prefix "o"
   :desc "Imenu list" "i" #'imenu-list-smart-toggle))

(use-package! evil-owl
  :config
  (setq evil-owl-display-method 'posframe
        evil-owl-extra-posframe-args '(:width 50 :height 50)
        evil-owl-max-string-length 50)
  (evil-owl-mode) ;; TODO reenable when fixed
  )

;; describe-face doesnt work well with hl-line-mode: use this or SPC u C-x = instead.
(defun what-is-this-face-called ()
  (interactive)
  (message "This face is called: %s" (or (plist-get (text-properties-at (point)) 'face) "default")))

;;; Advices

;; Center after searching https://www.reddit.com/r/emacs/comments/6ewd0h/how_can_i_center_the_search_results_vertically/
(advice-add 'evil-ex-search-next :after
            (lambda (&rest x) (evil-scroll-line-to-center (line-number-at-pos))))
(advice-add 'evil-ex-search-previous :after
            (lambda (&rest x) (evil-scroll-line-to-center (line-number-at-pos))))
