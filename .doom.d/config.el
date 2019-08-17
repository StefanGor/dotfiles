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

;; TRIAL SECTION

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
(setq sql-product 'ms)
;; basename?
(setq counsel-projectile-find-file-matcher 'counsel-projectile-find-file-matcher-basename)

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
(global-set-key (kbd "<f8>") 'toggle-flycheck-error-buffer)

;; https://github.com/hlissner/doom-emacs/issues/1568
(setq-hook! '(prog-mode-hook text-mode-hook conf-mode-hook) show-trailing-whitespace nil)

;; Mappings
(map! (:leader
      :desc "Find file in project" :nv "p f" #'projectile-find-file
      :desc "Find file in project" :nv "." #'projectile-find-file
      :desc "M-x" :n "SPC" #'execute-extended-command

      :desc "clear errors" :n "e c" #'flycheck-clear
      :desc "check buffer" :n "e e" #'flycheck-buffer

      :desc "toggle wrap" :n "t w" #'toggle-truncate-lines
      :desc "toggle debug on error" :n "t d" #'toggle-debug-on-error

      (:desc "jump" :prefix "j"
        :desc "character" :n "c" #'evil-avy-goto-char))

      ;; non-leader bindings
      :n "<f4>" (lambda () (interactive) (find-file "~/Desktop/Other/notes/scratchpad-em.txt"))
      :n "<f5>" (lambda () (interactive) (find-file "~/.doom.d/config.el"))
      :n "<f6>" #'neotree-toggle
      :n "<f7>" #'deadgrep
      :n "<f9>" #'treemacs
      ;; :n "] E" #'flycheck-next-error ;; todo put in an after + map?
      ;; :n "[ E" #'flycheck-previous-error

      "C-s" #'save-buffer
      :nvmi "C-/" #'comment-line ;; this doesnt work well in visual mode
      :m "C-_" #'text-scale-decrease
      "C-+" #'text-scale-increase
      :m "M-_" #'doom/decrease-font-size ;; :m makes it override undo-tree
      "M-+" #'doom/increase-font-size
)

(map!
 :n "]h" #'git-gutter:next-hunk
 :n "[h" #'git-gutter:previous-hunk
 ;; I don't like jumping around between files with [e
 :n "[e" #'flycheck-previous-error
 :n "]e" #'flycheck-next-error
 :n "]E" #'previous-error
 :n "[E" #'next-error
 )

(map! :map omnisharp-mode-map
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

(map! :map csharp-mode-map ;; needs to be csharp-mode-map or lambda descriptions dont work?
      :localleader
      :desc "refactor" :n "<return>" #'omnisharp-run-code-action-refactoring
      :desc "errors" :n "e"  (lambda () (interactive) (omnisharp-solution-errors t))
)

(map! :map emacs-lisp-mode-map
      :localleader
      :n "v" #'eval-defun)

(map! :map ivy-switch-buffer-map
      "C-c C-k" #'ivy-switch-buffer-kill) ;; This is on C-o C-k

;; Package config
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
   '(bar matches buffer-info)
   '(misc-info major-mode process checker))
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

   ;; https://github.com/abo-abo/swiper/issues/925#issuecomment-335789390
   ;; counsel-grep-base-command "rg -i -M 120 --no-heading --line-number --color never '%s' %s"
   ;; counsel-rg-base-command "rg -i -M 120 --no-heading --line-number --color never '%s' %s"
   )
  )

(after! company
  ;; https://github.com/patrickt/emacs/blob/master/init.el#L237
  (let ((map company-active-map))
    (mapc (lambda (x) (define-key map (format "%d" x)
                        `(lambda () (interactive) (company-complete-number ,x))))
          (number-sequence 0 9))))

(after! evil
  (setq-default
   evil-escape-key-sequence "fd"
   ))

(setq
 evil-escape-key-sequence "fd"
 doom-font (font-spec :family "Hack" :size 12)
 large-file-warning-threshold nil ;; Warning about opening tags file
 electric-indent-mode t
 treemacs-silent-refresh t
 inhibit-compacting-font-caches t
 ;;omnisharp-expected-server-version "1.32.11" ;; fix omnisharp-emacs 'already exists' issue
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
  ;; (lambda ()
    ;; (c-set-offset 'arglist-cont-nonempty +)
    ;; (c-offsets-alist 'arglist-close c-lineup-arglist)
    ;; ;; )
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
    ;; )
  )

(add-to-list 'auto-mode-alist '("\\.cshtml$" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.config$" . fundamental-mode)) ;; xml mode really slow

(defun transparent(alpha-level no-focus-alpha-level)
 (interactive "nAlpha level (0-100): \nnNo focus alpha level (0-100): ")
 (set-frame-parameter (selected-frame) 'alpha (list alpha-level no-focus-alpha-level))
 (add-to-list 'default-frame-alist `(alpha ,alpha-level)))

(transparent 100 95)

(load! "restoreframe")

;; notes
;; global-hl-line-mode to disable line highlighting + hl-line-mode
;;   can then use 'SPC h F' / M-x describe-face to get the face at point to see why its coloured the way it is
;; ivy-rg - use -tlist --ivy to provide additional args - see counsel-rg definition
;; counsel-projectile-sort-files to make complete matches show above partial matches? there was a
;;  github thread about this but i lost it
;;

;;new
;;(define-key treemacs-mode-map [mouse-1] #'treemacs-single-click-expand-action)
(setq
 treemacs-git-mode 'deferred
 )

