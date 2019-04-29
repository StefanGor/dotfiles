;; -*- mode: emacs-lisp; lexical-binding: t -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Layer configuration:
This function should only modify configuration layer settings."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs

   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'unused

   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t

   ;; If non-nil layers with lazy install support are lazy installed.
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()

   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press `SPC f e R' (Vim style) or
     ;; `M-m f e R' (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     helm
     (auto-completion :variables
                      auto-completion-enable-sort-by-usage t
                      auto-completion-return-key-behavior 'complete
                      auto-completion-tab-key-behavior 'complete
                      ;; auto-completion-private-snippets-directory
                      auto-completion-enable-snippets-in-popup t
                      auto-completion-enable-help-tooltip 'manual) ;; invoke with M-h

     ;; better-defaults ;; doesnt really apply to vim
     emacs-lisp
     ;; git
     ;; markdown
     ;; org
     ;; (shell :variables
     ;;        shell-default-height 30
     ;;        shell-default-position 'bottom)
     ;; spell-checking
     syntax-checking
     (version-control :variables
                      version-control-diff-side 'left)
     csharp
     themes-megapack
     sql
     ;; ycmd
     javascript
     html
     ;;gtags
     ;; persp-mode
     )

   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   ;; To use a local version of a package, use the `:location' property:
   ;; '(your-package :location "~/path/to/your-package/")
   ;; Also include the dependencies as they will not be resolved automatically.
   dotspacemacs-additional-packages '()

   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()

   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '(holy-mode)

   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and deletes any unused
   ;; packages as well as their unused dependencies. `used-but-keep-unused'
   ;; installs only the used packages but won't delete unused ones. `all'
   ;; installs *all* packages supported by Spacemacs and never uninstalls them.
   ;; (default is `used-only')
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization:
This function is called at the very beginning of Spacemacs startup,
before layer configuration.
It should only modify the values of Spacemacs settings."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non-nil then enable support for the portable dumper. You'll need
   ;; to compile Emacs 27 from source following the instructions in file
   ;; EXPERIMENTAL.org at to root of the git repository.
   ;; (default nil)
   dotspacemacs-enable-emacs-pdumper nil

   ;; File path pointing to emacs 27.1 executable compiled with support
   ;; for the portable dumper (this is currently the branch pdumper).
   ;; (default "emacs-27.0.50")
   dotspacemacs-emacs-pdumper-executable-file "emacs-27.0.50"

   ;; Name of the Spacemacs dump file. This is the file will be created by the
   ;; portable dumper in the cache directory under dumps sub-directory.
   ;; To load it when starting Emacs add the parameter `--dump-file'
   ;; when invoking Emacs 27.1 executable on the command line, for instance:
   ;;   ./emacs --dump-file=~/.emacs.d/.cache/dumps/spacemacs.pdmp
   ;; (default spacemacs.pdmp)
   dotspacemacs-emacs-dumper-dump-file "spacemacs.pdmp"

   ;; If non-nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t

   ;; Maximum allowed time in seconds to contact an ELPA repository.
   ;; (default 5)
   dotspacemacs-elpa-timeout 5

   ;; Set `gc-cons-threshold' and `gc-cons-percentage' when startup finishes.
   ;; This is an advanced option and should not be changed unless you suspect
   ;; performance issues due to garbage collection operations.
   ;; (default '(100000000 0.1))
   dotspacemacs-gc-cons '(100000000 0.1)

   ;; If non-nil then Spacelpa repository is the primary source to install
   ;; a locked version of packages. If nil then Spacemacs will install the
   ;; latest version of packages from MELPA. (default nil)
   dotspacemacs-use-spacelpa nil

   ;; If non-nil then verify the signature for downloaded Spacelpa archives.
   ;; (default nil)
   dotspacemacs-verify-spacelpa-archives nil

   ;; If non-nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil

   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'. (default 'emacs-version)
   dotspacemacs-elpa-subdirectory 'emacs-version

   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'vim

   ;; If non-nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil

   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner nil

   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'.
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((recents . 3)
                                (projects . 3))

   ;; True if the home buffer should respond to resize events. (default t)
   dotspacemacs-startup-buffer-responsive t

   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode

   ;; Initial message in the scratch buffer, such as "Welcome to Spacemacs!"
   ;; (default nil)
   dotspacemacs-initial-scratch-message nil

   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press `SPC T n' to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(atom-one-dark
                         spacemacs-dark)

   ;; Set the theme for the Spaceline. Supported themes are `spacemacs',
   ;; `all-the-icons', `custom', `doom', `vim-powerline' and `vanilla'. The
   ;; first three are spaceline themes. `doom' is the doom-emacs mode-line.
   ;; `vanilla' is default Emacs mode-line. `custom' is a user defined themes,
   ;; refer to the DOCUMENTATION.org for more info on how to create your own
   ;; spaceline theme. Value can be a symbol or list with additional properties.
   ;; (default '(spacemacs :separator wave :separator-scale 1.5))
   dotspacemacs-mode-line-theme '(doom)

   ;; If non-nil the cursor color matches the state color in GUI Emacs.
   ;; (default t)
   dotspacemacs-colorize-cursor-according-to-state t

   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   dotspacemacs-default-font '("Hack"
                               :size 14
                               :weight normal
                               :width normal)

   ;; The leader key (default "SPC")
   dotspacemacs-leader-key "SPC"

   ;; The key used for Emacs commands `M-x' (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"

   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"

   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"

   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","

   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m")
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"

   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs `C-i', `TAB' and `C-m', `RET'.
   ;; Setting it to a non-nil value, allows for separate commands under `C-i'
   ;; and TAB or `C-m' and `RET'.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab t

   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"

   ;; If non-nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil

   ;; If non-nil then the last auto saved layouts are resumed automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil

   ;; If non-nil, auto-generate layout name when creating new layouts. Only has
   ;; effect when using the "jump to layout by number" commands. (default nil)
   dotspacemacs-auto-generate-layout-names nil

   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1

   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache

   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5

   ;; If non-nil, the paste transient-state is enabled. While enabled, after you
   ;; paste something, pressing `C-j' and `C-k' several times cycles through the
   ;; elements in the `kill-ring'. (default nil)
   dotspacemacs-enable-paste-transient-state nil

   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4

   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom

   ;; Control where `switch-to-buffer' displays the buffer. If nil,
   ;; `switch-to-buffer' displays the buffer in the current window even if
   ;; another same-purpose window is available. If non-nil, `switch-to-buffer'
   ;; displays the buffer in a same-purpose window even if the buffer can be
   ;; displayed in the current window. (default nil)
   dotspacemacs-switch-to-buffer-prefers-purpose nil

   ;; If non-nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t

   ;; If non-nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil

   ;; If non-nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil

   ;; If non-nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 98

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90

   ;; If non-nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t

   ;; If non-nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t

   ;; If non-nil unicode symbols are displayed in the mode line.
   ;; If you use Emacs as a daemon and wants unicode characters only in GUI set
   ;; the value to quoted `display-graphic-p'. (default t)
   dotspacemacs-mode-line-unicode-symbols nil

   ;; If non-nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling nil

   ;; Control line numbers activation.
   ;; If set to `t' or `relative' line numbers are turned on in all `prog-mode' and
   ;; `text-mode' derivatives. If set to `relative', line numbers are relative.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; (default nil)
   dotspacemacs-line-numbers 'relative

   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil

   ;; If non-nil `smartparens-strict-mode' will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil

   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc…
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil

   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all

   ;; If non-nil, start an Emacs server if one is not already running.
   ;; (default nil)
   dotspacemacs-enable-server nil

   ;; Set the emacs server socket location.
   ;; If nil, uses whatever the Emacs default is, otherwise a directory path
   ;; like \"~/.emacs.d/server\". It has no effect if
   ;; `dotspacemacs-enable-server' is nil.
   ;; (default nil)
   dotspacemacs-server-socket-dir nil

   ;; If non-nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil

   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `rg', `ag', `pt', `ack' and `grep'.
   ;; (default '("rg" "ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("rg" "ag" "pt" "ack" "grep")

   ;; Format specification for setting the frame title.
   ;; %a - the `abbreviated-file-name', or `buffer-name'
   ;; %t - `projectile-project-name'
   ;; %I - `invocation-name'
   ;; %S - `system-name'
   ;; %U - contents of $USER
   ;; %b - buffer name
   ;; %f - visited file name
   ;; %F - frame name
   ;; %s - process status
   ;; %p - percent of buffer above top of window, or Top, Bot or All
   ;; %P - percent of buffer above bottom of window, perhaps plus Top, or Bot or All
   ;; %m - mode name
   ;; %n - Narrow if appropriate
   ;; %z - mnemonics of buffer, terminal, and keyboard coding systems
   ;; %Z - like %z, but including the end-of-line format
   ;; (default "%I@%S")
   dotspacemacs-frame-title-format "%t"

   ;; Format specification for setting the icon title format
   ;; (default nil - same as frame-title-format)
   dotspacemacs-icon-title-format nil

   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed' to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup 'changed

   ;; Either nil or a number of seconds. If non-nil zone out after the specified
   ;; number of seconds. (default nil)
   dotspacemacs-zone-out-when-idle nil

   ;; Run `spacemacs/prettify-org-buffer' when
   ;; visiting README.org files of Spacemacs.
   ;; (default nil)
   dotspacemacs-pretty-docs nil))

(defun dotspacemacs/user-env ()
  "Environment variables setup.
This function defines the environment variables for your Emacs session. By
default it calls `spacemacs/load-spacemacs-env' which loads the environment
variables declared in `~/.spacemacs.env' or `~/.spacemacs.d/.spacemacs.env'.
See the header of this file for more information."
  (spacemacs/load-spacemacs-env))

(defun dotspacemacs/user-init ()
  "Initialization for user code:
This function is called immediately after `dotspacemacs/init', before layer
configuration.
It is mostly for variables that should be set before packages are loaded.
If you are unsure, try setting them in `dotspacemacs/user-config' first."
  (setq
   neo-window-fixed-size nil
   ;; truncate-lines t
   large-file-warning-threshold nil
   initial-frame-alist '((top . 308) (left . 1195) (width . 100) (height . 50))
  )
  (setq-default
   truncate-lines t
  )
  
  ;; my custom file with static functions e.g. frame geometry
  ;; https://gist.github.com/synic/0357fdc2dcc777d89d1e
  (setq my-custom-file "~/.emacs.d/private/mycustom.el")
  (when (file-exists-p my-custom-file)
    (load-file my-custom-file)
	  (if window-system
      (progn
        (add-hook 'after-init-hook 'load-framegeometry)
        (add-hook 'kill-emacs-hook 'save-framegeometry))
	  ))
)

(defun dotspacemacs/user-load ()
  "Library to load while dumping.
This function is called only while dumping Spacemacs configuration. You can
`require' or `load' the libraries of your choice that will be included in the
dump."
  )

(defun dotspacemacs/user-config ()
  "Configuration for user code:
This function is called at the very end of Spacemacs startup, after layer
configuration.
Put your configuration code here, except for variables that should be set
before packages are loaded."

  (setq
   flycheck-check-syntax-automatically '(mode-enabled save) ;flycheck only on save

   ;; scrolling
   mouse-wheel-scroll-amount '(3 ((shift) . 1)) ;; two lines at a time
   mouse-wheel-progressive-speed nil ;; don't accelerate scrolling
   mouse-wheel-follow-mouse 't ;; scroll window under mouse
   scroll-conservatively 10
   scroll-margin 7
   scroll-preserve-screen-position 't

   ;; projectile
   ;; projectile-sort-order nil
   projectile-indexing-method 'alien
   projectile-enable-caching t
   projectile-tags-command "ctags.exe -R -e --language-force=C#"

   ;;smartparens
   sp-autoinsert-quote-if-followed-by-closing-pair nil ; inserting \" when typing " ?
   sp-escape-quotes-after-insert nil ; not sure if this setting or the one above

   ;; doom modeline
   doom-modeline-icon nil

   auto-window-vscroll nil ; https://emacs.stackexchange.com/questions/28736/emacs-pointcursor-movement-lag/28746

  )

  ;; (doom-modeline-def-modeline main
                              ;; )
  ;; https://github.com/seagle0128/doom-modeline/issues/12

  (spacemacs/toggle-vi-tilde-fringe-off) ;remove tildes on empty lines

  ; ycmd
  ;; (setq ycmd-server-command '("python" "-u" "C:/Users/stefang/.vim/plugged/YouCompleteMe/third_party/ycmd/ycmd"))
  ;; (setq ycmd-force-semantic-completion t)
  ;; (add-hook 'csharp-mode-hook 'ycmd-mode)

  ;; -experimental indentation section-
  (setq-default tab-width 4)
  (setq-local c-basic-offset 4)
  ;; (add-hook 'c-mode-hook
  ;;           (function (lambda ()
  ;;                       setq evil-shift-width 4)))

  (setq-default c-basic-offset 4)

  (add-hook 'c-mode-common-hook
            (lambda ()
              (c-set-offset 'arglist-cont-nonempty +)
              (c-offsets-alist 'arglist-close c-lineup-arglist)
              ))

  (add-hook 'csharp-mode-hook
            (lambda ()
              ; suggested by https://github.com/josteink/csharp-mode
              (electric-pair-mode 1)
              (electric-pair-local-mode 1)
              ))
  ;; (setq evil-shift-width 4)
  ;; (setq evil-shift-round nil)
  ;; (define-key evil-insert-state-map (kbd "TAB") 'tab-to-tab-stop)

  ;; -Keybinding config-
  (global-set-key (kbd "<backtab>") 'un-indent-by-removing-4-spaces)
  (defun un-indent-by-removing-4-spaces ()
    "remove 4 spaces from beginning of of line"
    (interactive)
    (save-excursion
      (save-match-data
        (beginning-of-line)
        ;; get rid of tabs at beginning of line
        (when (looking-at "^\\s-+")
          (untabify (match-beginning 0) (match-end 0)))
        (when (looking-at "^    ")
          (replace-match "")))))


  (global-set-key (kbd "<C-return>") 'newline-with-tab)
  (defun newline-with-tab ()
    (interactive)
    (c-indent-new-comment-line) ;; should be c mode only?
    (tab-to-tab-stop))

  (define-key evil-insert-state-map (kbd "TAB") 'tab-to-tab-stop)

  (define-key evil-normal-state-map (kbd "g i") 'omnisharp-find-implementations)

  (define-key undo-tree-map (kbd "C-/") nil)
  (define-key evil-motion-state-map (kbd "C-/") 'evilnc-comment-or-uncomment-lines)
  (define-key global-map (kbd "C-SPC" ) 'company-complete )

  ;; make q close the split that gets opened by evil rather than just killing the buffer
  (define-key evil-list-view-mode-map (kbd "q") 'quit-window)

  ;; Set TAB as yas-maybe-expand. If there's no expansion available, call
  ;; ordinary TAB command.
  (add-hook 'yas-minor-mode-hook (lambda ()
                                   (define-key yas-minor-mode-map (kbd "TAB") yas-maybe-expand) ))

  ;; -Web mode-
  (add-to-list 'auto-mode-alist '("\\.cshtml$" . web-mode))

  (defun my-web-mode-hook ()
    "Hooks for Web mode."
    (setq web-mode-enable-block-face nil) ;; disable black bg for code in razor files
    )
  (add-hook 'web-mode-hook  'my-web-mode-hook)

  (spacemacs/enable-transparency)

  ;; custom file doesnt reliably work - I still get it showing up in this file sometimes
  ;; (setq custom-file "~/.emacs.d/private/custom.el")
  ;; (when (file-exists-p custom-file)
  ;;   (load-file custom-file))

  ;; USEFUL LINKS
  ;; https://stackoverflow.com/questions/1475279/how-to-control-indentation-after-an-open-parenthesis-in-emacs

  ;; USEFUL KEYBINDS THAT I KEEP FORGETTING
  ;; M-: for evaluating a variable
  ;; C-c C-o for seeing indentation variable
  ;; alt-/ to expand a snippet from text
  ;; https://github.com/jscheid/dtrt-indent indentation guesser
  ;; dont go into state when using * or #
  ;; (define-key evil-normal-state-map (kbd "*") 'ahs-forward)
  ;; (define-key evil-normal-state-map (kbd "#") 'ahs-backward)
  ;; working custom variable file https://github.com/xcwen/spacemacs-config
  ;;
  ;; (add-hook 'typescript-mode-hook '(lambda ( )
  ;;                                    (my-set-evil-local-map "<tab>" 'yas-expand-for-vim )))
  ;; spc h d k to press a binding and see what is bound
  ; wdired-change-to-wdired-mode to mass rename files?
  ;; https://themegallery.robdor.com/ themes
  ;; SPC s c == :noh
  ;; pres C-h C-h when in the middle of 'which-key' to see available options at that point
  ;; C-h v to describe variable
  ;; SPC j i  - jump to member in current buffer
  ;; 'imenu-list' is similar to vim tagbar
  ;; M-x (doom-modeline-format--main) WITH the brackets to get the modeline config
  ;; there are modelines other than 'main' - see source code line 1309
  ;; https://github.com/Andrew-William-Smith/evil-fringe-mark
  ;; splits (e.g. :marks, :jump) are opened up differently depending on the size of the window
  ;;    if the window is wide then it will go to the right, otherwise bottom
  ;; kill buffers by selecting them with c-spc, then pressing M-D
  ;; csharp mode has an indentation issue with the following initialiser: 
  ;; var t = new    //works fine if you do 'new object'
  ;;     {   
  ;; m-/ does text completion suggestion?
)
(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (yasnippet-snippets winum web-mode toc-org sql-indent request omnisharp neotree kaolin-themes inkpot-theme hl-todo highlight-indentation helm-make google-translate evil-nerd-commenter evil-matchit evil-goggles editorconfig dumb-jump doom-themes doom-modeline diff-hl counsel-projectile counsel swiper ivy browse-at-remote auto-yasnippet apropospriate-theme ace-link tern iedit flycheck git-commit with-editor company projectile helm helm-core avy which-key use-package org-plus-contrib evil goto-chg hydra zenburn-theme zen-and-art-theme ws-butler writeroom-mode white-sand-theme web-beautify volatile-highlights vi-tilde-fringe uuidgen undo-tree underwater-theme ujelly-theme twilight-theme twilight-bright-theme twilight-anti-bright-theme toxi-theme tao-theme tangotango-theme tango-plus-theme tango-2-theme tagedit symon sunny-day-theme sublime-themes subatomic256-theme subatomic-theme string-inflection spaceline-all-the-icons spacegray-theme soothe-theme solarized-theme soft-stone-theme soft-morning-theme soft-charcoal-theme smyx-theme slim-mode shut-up shrink-path seti-theme scss-mode sass-mode reverse-theme restart-emacs rebecca-theme rainbow-delimiters railscasts-theme purple-haze-theme pug-mode professional-theme prettier-js popwin planet-theme phoenix-dark-pink-theme phoenix-dark-mono-theme persp-mode pcre2el password-generator paradox overseer organic-green-theme org-bullets open-junk-file omtose-phellack-theme oldlace-theme occidental-theme obsidian-theme noctilux-theme naquadah-theme nameless mustang-theme move-text monokai-theme monochrome-theme molokai-theme moe-theme minimal-theme material-theme majapahit-theme madhat2r-theme macrostep lush-theme lorem-ipsum livid-mode link-hint light-soap-theme json-navigator json-mode js2-refactor js-doc jbeans-theme jazz-theme ir-black-theme indent-guide impatient-mode hungry-delete highlight-parentheses highlight-numbers heroku-theme hemisu-theme helm-xref helm-themes helm-swoop helm-purpose helm-projectile helm-mode-manager helm-flx helm-descbinds helm-css-scss helm-company helm-c-yasnippet helm-ag hc-zenburn-theme gruvbox-theme gruber-darker-theme grandshell-theme gotham-theme golden-ratio git-gutter-fringe git-gutter-fringe+ gandalf-theme fuzzy font-lock+ flycheck-pos-tip flx-ido flatui-theme flatland-theme fill-column-indicator farmhouse-theme fancy-battery eziam-theme eyebrowse expand-region exotica-theme evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-numbers evil-lisp-state evil-lion evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-cleverparens evil-args evil-anzu eval-sexp-fu espresso-theme emmet-mode elisp-slime-nav eldoc-eval dracula-theme dotenv-mode django-theme diminish define-word darktooth-theme darkokai-theme darkmine-theme darkburn-theme dakrone-theme cyberpunk-theme csharp-mode company-web company-tern company-statistics company-quickhelp column-enforce-mode color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized clues-theme clean-aindent-mode cherry-blossom-theme centered-cursor-mode busybee-theme bubbleberry-theme birds-of-paradise-plus-theme bind-key badwolf-theme auto-highlight-symbol auto-compile atom-one-dark-theme async anti-zenburn-theme ample-zen-theme ample-theme alect-themes aggressive-indent afternoon-theme ace-window ace-jump-helm-line ac-ispell))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
)
