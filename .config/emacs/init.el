;; Set up MELPA bleeding-edge repository ---------------------------------------
;; OB; Set up MELPA bleeding-edge repository
;; https://melpa.org/#/getting-started
(eval-when-compile
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (package-initialize))

;; Set up use-package ----------------------------------------------------------
;; Set up use-package
;; https://github.com/jwiegley/use-package/blob/a7422fb8ab1baee19adb2717b5b47b9c3812a84c/README.md#getting-started
;; This is only needed once, near the top of the file
(eval-when-compile
  ;; Following line is not needed if use-package.el is in ~/.emacs.d
    ;; (add-to-list 'load-path "<path where use-package is installed>")
      (require 'use-package))
;; Ensure that all packages are installed globally and will have lazy loading
;; https://github.com/jwiegley/use-package/blob/a7422fb8ab1baee19adb2717b5b47b9c3812a84c/README.md#package-installation
(require 'use-package-ensure)
(setq use-package-always-ensure t)
(setq use-package-always-defer t)

;; Update packages automatically -----------------------------------------------
;; (use-package auto-package-update
;;   :config
;;   (setq auto-package-update-delete-old-versions t)
;;   (setq auto-package-update-hide-results t)
;;   (auto-package-update-maybe))

;; Profile initialization time -------------------------------------------------
(use-package esup
  :ensure t
  ;; To use MELPA Stable use ":pin melpa-stable",
  :pin melpa)

;; Auto save and backup---------------------------------------------------------
;; https://emacs.grym.io/#org95d343b
(defvar emacs-autosave-directory
  (concat user-emacs-directory "autosaves/")
  "This variable dictates where to put auto saves. It is set to a
  directory called autosaves located wherever your .emacs.d/ is
  located.")

(unless (file-exists-p emacs-autosave-directory)
    (make-directory emacs-autosave-directory))

(setq auto-save-file-name-transforms `((".*" ,emacs-autosave-directory t)))

(setq auto-save-timeout 20
      auto-save-interval 20)

(setq auto-save-default t)

(setq backup-directory-alist
      `((".*" . ,emacs-autosave-directory)))

(setq kept-new-versions 10
      kept-old-versions 0)

(setq delete-old-versions t)

(setq backup-by-copying t)

(setq vc-make-backup-files t)

(use-package backup-each-save
	     :hook (after-save . backup-each-save))

;; Auto revert buffer when file is modified ------------------------------------
(global-auto-revert-mode 1)

;; Remove some buffers ---------------------------------------------------------
;; https://unix.stackexchange.com/a/152151/88701
;; Makes *scratch* empty.
(setq initial-scratch-message "")

;; Removes *scratch* from buffer after the mode has been set.
(defun remove-scratch-buffer ()
  (if (get-buffer "*scratch*")
      (kill-buffer "*scratch*")))
(add-hook 'after-change-major-mode-hook 'remove-scratch-buffer)

;; Removes *messages* from the buffer.
;;(setq-default message-log-max nil)
(kill-buffer "*Messages*")

;; Removes *Completions* from buffer after you've opened a file.
(add-hook 'minibuffer-exit-hook
      (lambda ()
	 (let ((buffer "*Completions*"))
	   (and (get-buffer buffer)
		(kill-buffer buffer)))))

;; Don't show *Buffer list* when opening multiple files at the same time.
(setq inhibit-startup-buffer-menu t)

;; Show only one active window when opening multiple files at the same time.
(add-hook 'window-setup-hook 'delete-other-windows)

;; Speed bar -------------------------------------------------------------------
(use-package  sr-speedbar)

(add-to-list 'imenu-generic-expression '(nil "^\\s-*-\{4,\}$" 1))
(add-to-list 'imenu-generic-expression '(nil ".*?-\{4,\}$" 1))

;; Sidebar ---------------------------------------------------------------------
;; https://github.com/rougier/dotemacs/blob/61cff30bfafb04dab3c017952c8ca4a47701fc83/dotemacs.org#sidebar
(use-package  imenu-list
  :config
  (setq
   imenu-list-position 'left
   imenu-list-size 28
   imenu-list-focus-after-activation t)
  :bind ("<f7>" . imenu-list-smart-toggle))

;; Always on in latex and org mdoe


;; custom tags
;; http://www.gnu.org/software/emacs/manual/html_node/speedbar/Tagging-Extensions.html
;; See also describe-symbol speedbar-dynamic-tags-function-list

;; ;; ;;;
;; (defun re-seq (regexp string)
;;   "Get a list of all regexp matches in a string"
;;   (save-match-data
;;     (let ((pos 0)
;;           matches)
;;       (while (string-match regexp string pos)
;;         (push (match-string 0 string) matches)
;;         (setq pos (match-end 0)))
;;       matches)))

;; ;; (defvar tmp-var (re-seq "\\S+\\s(.+?)\\s-{4,}" (buffer-string)))

;; ;; ;;; Based on speedbar-fetch-dynamic-imenu
;; (defun speedbar-fetch-dynamic-separators (file)
;;   "Load FILE into a buffer, and generate tags using Imenu.
;; Returns the tag list, or t for an error."
;;   ;; Load this AND compile it in
;;   (set-buffer (find-file-noselect file))
;;   (re-seq "\\S+\\s(.+?)\\s-{4,}" (buffer-string))
;;   )

;; (defvar tmp-var (speedbar-fetch-dynamic-separators "~/.config/emacs/init.el"))
;; (describe-variable 'tmp-var)

;; (add-to-list 'speedbar-dynamic-tags-function-list
;;              '(speedbar-fetch-dynamic-separators
;;              . speedbar-create-tag-hierarchy))

;; Key bindings ----------------------------------------------------------------
(use-package key-chord
  :init
  (key-chord-mode t)
  (key-chord-define-global "//" 'sr-speedbar-toggle)
  )

;; No more typing the whole yes or no. Just y or n will do.
(fset 'yes-or-no-p 'y-or-n-p)

;; File navigation -------------------------------------------------------------
(ffap-bindings)

;; Window navigation -----------------------------------------------------------
;; split windows evenly by default
(setq window-resize-pixelwise  t)

;; tab-bar-mode
;; https://www.emacswiki.org/emacs/TabBarMode
(global-set-key [M-left] 'previous-buffer)
(global-set-key [M-right] 'next-buffer)

;; workgroups2 save layout to disk
(use-package workgroups2)

;; ace-window
(use-package ace-window
  :bind (("C-<tab>" . ace-window))
  :config (setq aw-scope 'frame))
;;  :bind ("M-o" . ace-window))

;; treemacs
(use-package treemacs
  :bind
  (:map global-map
	("M-0"       . treemacs-select-window)
	("C-x t 1"   . treemacs-delete-other-windows)
	("C-x t t"   . treemacs)
	("C-x t B"   . treemacs-bookmark)
	("C-x t C-t" . treemacs-find-file)
	("C-x t M-t" . treemacs-find-tag))
  )

;; Buffer navigation -----------------------------------------------------------
(use-package ibuffer
	     :bind ([f12] . ibuffer))
;; Hide some entries by default
;; https://www.emacswiki.org/emacs/IbufferMode#h5o-2
(require 'ibuf-ext)
(add-to-list 'ibuffer-never-show-predicates "^\\*")

;; Editing ---------------------------------------------------------------------
;; Commenting
(use-package comment-dwim-2
  :bind (("M-;" . comment-dwim-2)))

;; Custom occur query ----------------------------------------------------------
(defun occur-quick-layout()
  "Show all entries with four or more dashes."
  (interactive)
  (occur "----")
  (switch-to-buffer "*Occur*")
  )

(global-set-key (kbd "C-c o") 'occur-quick-layout)

;; -----------------------------------------------------------------------------
(use-package helpful
  :bind
  (:map global-map
	("C-c C-d"   . #'helpful-at-point))
  )

;; Helm ------------------------------------------------------------------------
(use-package helm)

;; Indentation settings --------------------------------------------------------
;; https://github.com/rougier/dotemacs/blob/61cff30bfafb04dab3c017952c8ca4a47701fc83/dotemacs.org#tabulations
(setq-default indent-tabs-mode nil        ; Stop using tabs to indent
              tab-always-indent 'complete ; Indent first then try completions
              tab-width 4)                ; Smaller width for tab characters

;; ;; Use spaces rather than tab as default
;; (setq-default indent-tabs-mode nil)

;; Whitespace settings ---------------------------------------------------------
(use-package whitespace
  :config
  (global-whitespace-mode t)
  (setq whitespace-style
        '(face
          empty ; empty lines at beginning and/or end of buffer
          trailing ; trailing blanks
          lines-tail ; part of line which goes beyond `whitespace-line-column'
          tabs ; all tabs
	      ;; indentation::tab  ; 8 or more spaces at line beginning
          ;; big-indent ; ???
          ;; space-after-tab::tab ; 8 or more SPACEs after a TAB
	      ;; space-after-tab::space
          ;; space-after-tab
          ;; space-before-tab::tab
	      ;; space-before-tab::space
          ;; space-before-tab
          )))

(use-package whitespace-cleanup-mode
  :init (whitespace-cleanup-mode t))

;; Paren settings --------------------------------------------------------------
;; https://www.emacswiki.org/emacs/ShowParenMode
(use-package paren
  :init (show-paren-mode t)
  :config
  (set-face-background 'show-paren-match (face-background 'default))
  (set-face-foreground 'show-paren-match "#def")
  (set-face-attribute 'show-paren-match nil :weight 'extra-bold)
  (setq show-paren-delay 0)
  )

;; Expand region ---------------------------------------------------------------
;;; https://github.com/magnars/expand-region.el
;;; https://elpa.gnu.org/packages/expand-region.html
(use-package expand-region
  :bind ("C-=" . er/expand-region))

;; Auto completion -------------------------------------------------------------
(use-package company
  :init (global-company-mode)
  )

;; Add column line -------------------------------------------------------------
;; https://github.com/rougier/dotemacs/blob/61cff30bfafb04dab3c017952c8ca4a47701fc83/dotemacs.org#typography
(setq-default fill-column 80                          ; Default line width
              sentence-end-double-space nil           ; Use a single space after dots
              bidi-paragraph-direction 'left-to-right ; Faster
              truncate-string-ellipsis "…")           ; Nicer ellipsis

;; https://emacs.stackexchange.com/a/50583
(global-display-fill-column-indicator-mode 1)
(setq-default display-fill-column-indicator-column 80)

;; Auto wrap lines -------------------------------------------------------------
;; auto line wrap
;; https://www.emacswiki.org/emacs/AutoFillMode
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; Clipboard settings ----------------------------------------------------------
;; https://unix.stackexchange.com/a/14935/88701
(use-package xclip
  :init (xclip-mode)
  )

;; Parenthesis settings --------------------------------------------------------
;; Activate electric pairs by default
(electric-pair-mode t)

(use-package smartparens
  :init (smartparens-mode)
  )

(use-package rainbow-mode
  :init (rainbow-mode)
  )

;; Flycheck --------------------------------------------------------------------
;; https://www.flycheck.org/en/latest/user/installation.html#use-package
(use-package flycheck
  :init (global-flycheck-mode)
  :config
  (setq flycheck-global-modes '(not ess-r-mode))
  )

;; Flyspell --------------------------------------------------------------------
(use-package flyspell)
;; flyspell without a mouse
;; https://www.emacswiki.org/emacs/FlySpell#h5o-7
(global-set-key (kbd "<f8>") 'ispell-word)

;; no spell check in source blocks
;; https://orgmode.org/worg/org-faq.html#orgdb49ea8
(add-to-list 'ispell-skip-region-alist '("#\\+begin_src". "#\\+end_src"))

;; Highlight keywords ----------------------------------------------------------
(use-package hl-todo
  :init (global-hl-todo-mode)
  :config
  (setq hl-todo-exclude-modes '(list org-mode))
  (setq hl-todo-keyword-faces
	    '(("todo.*"  . "#FF0000")
	      ("fixme.*" . "#FF0000")
	      ("TODO"    . "#FF0000")
	      ("FIXME"   . "#FF0000")
          ("GOTCHA"  . "#3DB7E9")
	      ("DEBUG"   . "#3DB7E9")
          ("INFO"    . "#3DB7E9")
	      ("STUB"    . "#F0E442")
          ("FILLME"  . "#F0E442")
          ("WARN"    . "#F0E442")
          )
	    )
  )

;; Rainbow delimiters ----------------------------------------------------------
;; (use-package rainbow-delimiters
;;   :init (require 'rainbow-delimiters)
;;   :init (rainbow-delimiters-mode))

;; Bulk editing ----------------------------------------------------------------
(use-package wgrep)

;; Search settings -------------------------------------------------------------
;; show number of matches
(use-package anzu
  :init (global-anzu-mode t))

;; Edit settings ---------------------------------------------------------------
(use-package iedit)

;; Which key -------------------------------------------------------------------
;; show keybind options
(use-package which-key
  :init (which-key-mode))

;; ix pastebin -----------------------------------------------------------------
(use-package ix)

;; graphviz dot settings -------------------------------------------------------
(use-package graphviz-dot-mode)

;; Git settings ----------------------------------------------------------------
(use-package magit
  :bind(("C-x g" . magit-status))
  :init (setq magit-status-margin '(t "%d %b %y %H:%M"
  magit-log-margin-width t 24))
  :config (setq magit-log-section-commit-count 25) )

(defun madit-status() (interactive)
       (defun magit-dit-filter (env)
	 "Add GIT_DIR and GIT_WORK_TREE to ENV when in a special directory.
https://github.com/magit/magit/issues/460 (@cpitclaudel)."
	 (let ((home (expand-file-name "~/")))
	   (let ((gitdir (expand-file-name "~/.dotfiles/")))
	     (push (format "GIT_WORK_TREE=%s" home) env)
	     (push (format "GIT_DIR=%s" gitdir) env)))
	 env)

       (advice-add 'magit-process-environment
		   :filter-return 'magit-dit-filter)
       (magit-status)
       )

;; gitignore settings ----------------------------------------------------------
(use-package gitignore-mode)

;; Org settings ----------------------------------------------------------------
(setq org-support-shift-select 'always)

;; empty line before, but never after, newly inserted headline
;; https://orgmode.org/worg/org-faq.html#blank-line-after-headlines-and-list-items
(setq org-blank-before-new-entry
      '((heading . t) (plain-list-item . nil)))

;; adapt indentation for all lines
;; https://emacs.stackexchange.com/a/70229
(setq org-adapt-indentation t)

;; set workflow states
;; https://orgmode.org/manual/Workflow-states.html
(setq org-todo-keywords
      '((sequence "TODO" "WIP" "REVWIEW" "|" "DONE" "CANC")))

;; export html when hitting F9
(add-hook 'org-mode-hook
	  (lambda() (local-set-key (kbd "<f9>") 'org-html-export-to-html)))

;; insert link from clipboard with sensible description
(use-package org-cliplink
  :bind ("C-x p" . org-cliplink))

(add-hook 'org-mode-hook
          (lambda() (org-babel-do-load-languages
                     'org-babel-load-languages
                     '((emacs-lisp . nil)
                       (R . t)
                       (shell . t)
                       ))))

;; support code block syntax highlighting in html files
(use-package htmlize)

;; fix wrong source block background
;; https://emacs.stackexchange.com/q/3374
(defun my/org-inline-css-hook (exporter)
  "Insert custom inline css to automatically set the
background of code to whatever theme I'm using's background"
  (when (eq exporter 'html)
    (let* ((my-pre-bg (face-background 'default))
	       (my-pre-fg (face-foreground 'default)))
      (setq
       org-html-head-extra
       (concat
	org-html-head-extra
	(format "<style type=\"text/css\">\n pre.src {background-color: %s; color: %s;}</style>\n"
		my-pre-bg my-pre-fg))))))

(add-hook 'org-export-before-processing-hook 'my/org-inline-css-hook)

;; Markdown settings------------------------------------------------------------
;;

;; CSV settings ----------------------------------------------------------------
(use-package csv-mode
  :custom ((csv-align-style 'auto))
  :hook ((csv-mode csv-align-mode) ; Always align
	 (csv-mode csv-header-lines)) ; Always show header line
  )

;; Shell settings---------------------------------------------------------------
;; shellcheck maybe?

;; PDF settings ----------------------------------------------------------------
;; https://github.com/jwiegley/use-package/blob/a7422fb8ab1baee19adb2717b5b47b9c3812a84c/README.md#magic-handlers
(use-package pdf-tools
  :load-path "site-lisp/pdf-tools/lisp"
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install :no-query))

;; Bibliographic references ----------------------------------------------------
;; bib file management
;; (use-package ebib) ;; commented out for performance (1.082sec)

;; Retrieve from public data bases
(use-package biblio)

;; LaTeX settings --------------------------------------------------------------
;; Latex Mode https://tex.stackexchange.com/a/209509
(defun my-LaTeX-mode()
  (add-to-list 'TeX-view-program-list '("Zathura" "zathura %o"))
  (setq TeX-view-program-selection '((output-pdf "Zathura")))
  )

(add-hook 'LaTeX-mode-hook 'my-LaTeX-mode)
(add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)

;; Latex preview pane
(use-package latex-preview-pane)

;; AucTeX settings -------------------------------------------------------------
;; (use-package auctex
;;   :custom ((TeX-auto-save t)
;;         (TeX-parse-self t)
;;         (TeX-master nil)
;;         (reftex-plug-into-auctex t)
;;         (font-latex-fontify-script nil))
;;   :hook ((LaTeX-mode . my-LaTeX-mode)
;;       (LaTeX-mode . flyspell-mode)
;;       (LaTeX-mode . LaTeX-math-mode)
;;       (LaTeX-mode . turn-on-reftex)
;;       (LaTeX-mode . rainbow-delimiters-mode))
;;   )

(use-package tex
  :ensure auctex
  :custom ((TeX-auto-save t)
	   (TeX-parse-self t)
	   (TeX-master nil)
	   (reftex-plug-into-auctex t)
	   (font-latex-fontify-script nil)
	   (LaTeX-electric-left-right-brace t)
	   )
  :hook ((LaTeX-mode . my-LaTeX-mode)
	 (LaTeX-mode . flyspell-mode)
	 (LaTeX-mode . LaTeX-math-mode)
	 (LaTeX-mode . TeX-fold-mode)
	 (LaTeX-mode . turn-on-reftex)
	 (LaTeX-mode . rainbow-delimiters-mode))
)

;; Polymode settings -----------------------------------------------------------
;; https://emacs.stackexchange.com/q/47842
;; https://polymode.github.io/installation/
     (use-package poly-markdown
       :ensure t)

(use-package poly-R)

;; https://github.com/vspinu/polymode
(use-package polymode
  :diminish (poly-org-mode
	     poly-markdown-mode
	     poly-noweb+r-mode
	     poly-noweb+r-mode
	     poly-markdown+r-mode
	     poly-rapport-mode
	     poly-html+r-mode
	     poly-brew+r-mode
	     poly-r+c++-mode
	     poly-c++r-mode)
;; :init
;; (require 'poly-R)
  ;; (require 'poly-markdown)
  :after (poly-R poly-markdown)
  :config
  (add-to-list 'auto-mode-alist '("\\.md$" . poly-markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.Rmd$" . poly-markdown+r-mode))
  (add-to-list 'auto-mode-alist '("\\.Rcpp$" . poly-r+c++-mode))
  (add-to-list 'auto-mode-alist '("\\.cppR$" . poly-c++r-mode))
  )

;; ESS settings ----------------------------------------------------------------
(use-package ess
  :custom ((ess-plain-first-buffername nil)
	   (ess-ask-about-transfile nil))
  )

;; (use-package lsp-mode
;;   :init
;;   ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
;;   (setq lsp-keymap-prefix "C-c l")
;;   :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
;; 	 (ess-mode . lsp)
;; 	 ;; if you want which-key integration
;; 	 (lsp-mode . lsp-enable-which-key-integration))
;;   :commands lsp)

(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb

;; optionally
(use-package lsp-ui :commands lsp-ui-mode)
;; if you are helm user
(use-package helm-lsp :commands helm-lsp-workspace-symbol)
;; if you are ivy user
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

;; optionally if you want to use debugger
;; (use-package dap-mode)
;; ;; (use-package dap-LANGUAGE) to load the dap adapter for your language

;; optional if you want which-key integration
(use-package which-key
    :config
    (which-key-mode))

;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Buffer-Display-Action-Alists.html
(defun goR()(interactive)
    (setq display-buffer-alist
	  `(
	    ("\\.R$" ; R source code window
	     (display-buffer-reuse-window display-buffer-in-direction)
	     (inhibit-same-window . nil)
	     (direction . left)
	     (window-width . 0.33)
	     (window-height . 1.00)
	     (dedicated . nil)
	     (reusable-frames . nil))
	    ("^\\*R Dired" ; File list window
	     (display-buffer-reuse-window display-buffer-in-side-window)
	     (side . right)
	     (window-width . 0.33)
	     (window-height . 0.33)
	     (dedicated . t)
	     (slot . 2)
	     (reusable-frames . nil))
	    ("^magit" ; Magit
	     (display-buffer-reuse-window display-buffer-in-side-window)
	     (side . right)
	     (window-width . 0.33)
	     (window-height . 0.33)
	     (dedicated . t)
	     (slot . 2)
	     (reusable-frames . nil))
	    ("^\\*R" ; R process window
	     (display-buffer-reuse-window display-buffer-in-side-window)
	     (side . right)
	     (window-width . 0.33)
	     (window-height . 0.33)
	     (dedicated . t)
	     (slot . 0)
	     (reusable-frames . nil))
	    ("^\\*Help" ; R help window
	     (display-buffer-reuse-window display-buffer-in-side-window)
	     (side . right)
	     (window-width . 0.33)
	     (window-height . 0.33)
	     (dedicated . t)
	     (slot . 1)
	     (reusable-frames . nil))
	    )
	  )
    (setq ess-ask-for-ess-directory nil)

    (dolist (file (file-expand-wildcards
		   (concat default-directory "/*.R")))
      (find-file file))
;;  (let ((files (file-expand-wildcards (concat default-directory "/*.R")))) (
;;  (while files
;;    (find-file (car files))
;;    (setq files (cdr files)))))
    (R)
    (ess-rdired)
    (magit)
    )

; Stan -------------------------------------------------------------------------
;; Right from https://github.com/stan-dev/stan-mode/tree/150bbbe5fd3ad2b5a3dbfba9d291e66eeea1a581#configuration
;; Uncomment the line below if not required elsewhere.
;; (require 'use-package)

;;; stan-mode.el
(use-package stan-mode
  :mode ("\\.stan\\'" . stan-mode)
  :hook (stan-mode . stan-mode-setup)
  ;;
  :config
  ;; The officially recommended offset is 2.
  (setq stan-indentation-offset 2))

;;; company-stan.el
(use-package company-stan
  :hook (stan-mode . company-stan-setup)
  ;;
  :config
  ;; Whether to use fuzzy matching in `company-stan'
  (setq company-stan-fuzzy nil))

;;; eldoc-stan.el
(use-package eldoc-stan
  :hook (stan-mode . eldoc-stan-setup)
  ;;
  :config
  ;; No configuration options as of now.
  )

;;; flycheck-stan.el
(use-package flycheck-stan
  ;; Add a hook to setup `flycheck-stan' upon `stan-mode' entry
  :hook ((stan-mode . flycheck-stan-stanc2-setup)
         (stan-mode . flycheck-stan-stanc3-setup))
  :config
  ;; A string containing the name or the path of the stanc2 executable
  ;; If nil, defaults to `stanc2'
  (setq flycheck-stanc-executable nil)
  ;; A string containing the name or the path of the stanc2 executable
  ;; If nil, defaults to `stanc3'
  (setq flycheck-stanc3-executable nil))

;;; stan-snippets.el
(use-package stan-snippets
  :hook (stan-mode . stan-snippets-initialize)
  ;;
  :config
  ;; No configuration options as of now.
  )

;;; ac-stan.el (Not on MELPA; Need manual installation)
(use-package ac-stan
  :load-path "path-to-your-directory/ac-stan/"
  ;; Delete the line below if using.
  :disabled t
  :hook (stan-mode . stan-ac-mode-setup)
  ;;
  :config
  ;; No configuration options as of now.
  )

;;Customizations by Custom -----------------------------------------------------
;; Other customizations
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-engine 'luatex)
 '(TeX-view-program-list '(("Zathura" ("zathura %o") "")))
 '(TeX-view-program-selection
   '(((output-dvi has-no-display-manager)
      "dvi2tty")
     ((output-dvi style-pstricks)
      "dvips and gv")
     (output-dvi "xdvi")
     (output-pdf "Zathura")
     (output-html "xdg-open")))
 '(desktop-load-locked-desktop t)
 '(desktop-restore-forces-onscreen nil t)
 '(desktop-restore-frames t)
 '(esup-depth 0)
 '(inhibit-startup-screen t)
 '(package-selected-packages
   '(idle-highlight-mode easy-mmode wrap-region org-superstar imenu-list poly-R ox-tiddly dap-mode lsp-treemacs lsp-ivy helm-lsp lsp-ui lsp-mode comment-dwim-2 ix\.el ix parsebib helm-bibtex ebib biblio org-cliplink wc-mode helpful latex-preview-pane wgrep pdf-tools sr-speedbar key-chord ox-twbs ox htmlize markdown-preview-mode ess xclip workgroups2 which-key use-package treemacs smartparens rainbow-mode poly-markdown magit iedit hl-todo helm gitignore-mode flycheck expand-region csv-mode company backup-each-save auctex anzu))
 '(safe-local-variable-values
   '((sr-speedbar-right-side)
     (sr-speedbar-toggle . t)
     (flycheck-mode)
     (eval TeX-run-style-hooks "beamer")))
 '(save-place-mode t)
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(tool-bar-mode nil))

;; (add-to-list 'desktop-path default-directory)

(setq desktop-restore-forces-onscreen nil)
(add-hook 'desktop-after-read-hook
	  (lambda ()
	    (frameset-restore
	     desktop-saved-frameset
	     :reuse-frames (eq desktop-restore-reuses-frames t)
	     :cleanup-frames (not (eq desktop-restore-reuses-frames 'keep))
	     :force-display desktop-restore-in-current-display
		 :force-onscreen desktop-restore-forces-onscreen)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-latex-math-face ((t (:foreground "yellow"))))
 '(font-latex-script-char-face ((t (:foreground "olive drab"))))
 '(font-latex-sectioning-5-face ((t (:foreground "#ff7983"))))
 '(font-latex-sedate-face ((t (:foreground "yellow"))))
 '(org-level-4 ((t (:extend nil :foreground "#8be9fd" :weight normal))))
 '(show-paren-match ((t (:background "#282a36" :foreground "#def" :underline t :weight extra-bold)))))

;;  Custom functions -----------------------------------------------------------
;; Insert commented line separator
;; Credits: pjb from #emacs at libera.chat
(defun insert-commented-line-separator (label)
  "Insert a commented line separator with a custom LABEL."
  (interactive "sLabel: ")
  (insert
   (let ((text label))
     (format "%s %s %s" comment-start text
	     (make-string
	      (-
	       (- display-fill-column-indicator-column 3) (length text)) ?-)))))

(global-set-key (kbd "C-c s") 'insert-commented-line-separator)

;; Call whitespace-cleanup on every file marked in dired
(defun whitespace-cleanup-marked-files()
  "Credits to bpalmer at emacs@libera.chat"
  (interactive)
  (dolist (f (dired-get-marked-files)) (find-file f)
	  (whitespace-cleanup) (save-buffer) (kill-buffer)))

(defun indent-region-marked-files()
  "Credits to bpalmer at emacs@libera.chat"
  (interactive)
  (dolist (f (dired-get-marked-files)) (find-file f)
	  (indent-region (point-min) (point-max)) (save-buffer) (kill-buffer)))

(defun execute-on-marked-files(command-name)
  "Read a command name and call it on files marked in dired buffer."
  (interactive "sCommand name: ")
  (dolist (f (dired-get-marked-files)) (find-file f)
	  ;; (push-mark)
	  ;; (push-mark (point-max) nil t)
	  ;; (goto-char (minibuffer-prompt-end))
	  ;;(command-execute (if (stringp command-name) (intern-soft command-name) command-name))
	  (command-execute (intern-soft command-name))
	  (save-buffer)
	  (kill-buffer)))

;; Custom theme-----------------------------------------------------------------
;; Fonts
;; https://github.com/rougier/dotemacs/blob/61cff30bfafb04dab3c017952c8ca4a47701fc83/dotemacs.org#fonts
(set-face-attribute 'default nil
                    :family "Roboto Mono"
                    :weight 'light
                    :height 120)

(set-face-attribute 'bold nil
                    :family "Roboto Mono"
                    :weight 'regular)

(set-face-attribute 'italic nil
                    :family "Victor Mono"
                    :weight 'semilight
                    :slant 'italic)

(set-fontset-font t 'unicode
                    (font-spec :name "Inconsolata Light"
                               :size 14) nil)

;; Dracula-dark (customized)
;; https://draculatheme.com/emacs/
;;(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(add-to-list 'custom-theme-load-path (concat user-emacs-directory "themes"))
(load-theme 'dracula t)

;; Export to HTML with useful anchors ------------------------------------------
;; https://github.com/alphapapa/unpackaged.el#export-to-html-with-useful-anchors
(eval-when-compile
  (require 'easy-mmode)
  (require 'ox))

(use-package ox
  :ensure nil
  :config
  (define-minor-mode unpackaged/org-export-html-with-useful-ids-mode
    "Attempt to export Org as HTML with useful link IDs.
Instead of random IDs like \"#orga1b2c3\", use heading titles,
made unique when necessary."
    :global t
    (if unpackaged/org-export-html-with-useful-ids-mode
	(advice-add #'org-export-get-reference :override #'unpackaged/org-export-get-reference)
      (advice-remove #'org-export-get-reference #'unpackaged/org-export-get-reference)))

  (defun unpackaged/org-export-get-reference (datum info)
    "Like `org-export-get-reference', except uses heading titles instead of random numbers."
    (let ((cache (plist-get info :internal-references)))
      (or (car (rassq datum cache))
	  (let* ((crossrefs (plist-get info :crossrefs))
		 (cells (org-export-search-cells datum))
		 ;; Preserve any pre-existing association between
		 ;; a search cell and a reference, i.e., when some
		 ;; previously published document referenced a location
		 ;; within current file (see
		 ;; `org-publish-resolve-external-link').
		 ;;
		 ;; However, there is no guarantee that search cells are
		 ;; unique, e.g., there might be duplicate custom ID or
		 ;; two headings with the same title in the file.
		 ;;
		 ;; As a consequence, before re-using any reference to
		 ;; an element or object, we check that it doesn't refer
		 ;; to a previous element or object.
		 (new (or (cl-some
			   (lambda (cell)
			     (let ((stored (cdr (assoc cell crossrefs))))
			       (when stored
				 (let ((old (org-export-format-reference stored)))
				   (and (not (assoc old cache)) stored)))))
			   cells)
			  (when (org-element-property :raw-value datum)
			    ;; Heading with a title
			    (unpackaged/org-export-new-title-reference datum cache))
			  ;; NOTE: This probably breaks some Org Export
			  ;; feature, but if it does what I need, fine.
			  (org-export-format-reference
			   (org-export-new-reference cache))))
		 (reference-string new))
	    ;; Cache contains both data already associated to
	    ;; a reference and in-use internal references, so as to make
	    ;; unique references.
	    (dolist (cell cells) (push (cons cell new) cache))
	    ;; Retain a direct association between reference string and
	    ;; DATUM since (1) not every object or element can be given
	    ;; a search cell (2) it permits quick lookup.
	    (push (cons reference-string datum) cache)
	    (plist-put info :internal-references cache)
	    reference-string))))

  (defun unpackaged/org-export-new-title-reference (datum cache)
    "Return new reference for DATUM that is unique in CACHE."
    (cl-macrolet ((inc-suffixf (place)
			       `(progn
				  (string-match (rx bos
						    (minimal-match (group (1+ anything)))
						    (optional "--" (group (1+ digit)))
						    eos)
						,place)
				  ;; HACK: `s1' instead of a gensym.
				  (-let* (((s1 suffix) (list (match-string 1 ,place)
							     (match-string 2 ,place)))
					  (suffix (if suffix
						      (string-to-number suffix)
						    0)))
				    (setf ,place (format "%s--%s" s1 (cl-incf suffix)))))))
      (let* ((title (org-element-property :raw-value datum))
	     (ref (url-hexify-string (substring-no-properties title)))
	     (parent (org-element-property :parent datum)))
	(while (--any (equal ref (car it))
		      cache)
	  ;; Title not unique: make it so.
	  (if parent
	      ;; Append ancestor title.
	      (setf title (concat (org-element-property :raw-value parent)
                                  "--" title)
		    ref (url-hexify-string (substring-no-properties title))
		    parent (org-element-property :parent parent))
	    ;; No more ancestors: add and increment a number.
	    (inc-suffixf ref)))
	ref))))

;; turn on minor mode when org-mode is loaded
(add-hook 'org-mode-hook
	  'unpackaged/org-export-html-with-useful-ids-mode)
(put 'downcase-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
(put 'upcase-region 'disabled nil)
