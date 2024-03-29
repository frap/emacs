;;;; -*- lexical-binding: t; -*-

(defun save-in-etc-file! (file-name)
  (expand-file-name (format "etc/%s" file-name) *emacs-cache/*))

(setup appearance
       (setq blinks-a-matching-paren nil)
       (setq display-time-default-load-average nil)
       (setq echo-keystrokes 0.1)
       (setq highlight-nonselected-windows nil)
       (setq idle-update-delay 1.0)
       (setq inhibit-startup-echo-area-message t)
       (setq inhibit-startup-screen t)
       (setq use-dialog-box nil)
       (setq use-file-dialog nil)
       (setq visible-bell nil)
       (setq x-gtk-use-system-tooltips nil)
       (setq x-stretch-cursor nil)
       (setq-default bidi-display-reordering 'left-to-right)
       (setq-default bidi-paragraph-direction 'left-to-right)
       (setq-default cursor-in-non-selected-windows nil)
       (setq-default cursor-type 'hbar)
       (setq-default display-line-numbers-widen t)
       (setq-default display-line-numbers-width 3)
       (setq-default indicate-buffer-boundaries nil)
       (setq-default truncate-lines t)

       (menu-bar-mode 1)
        ;; Configure common Emoji fonts, making it more likely that Emoji will work out of the box
       (set-fontset-font t 'symbol "Apple Color Emoji")
       (set-fontset-font t 'symbol "Noto Color Emoji" nil 'append)
       (set-fontset-font t 'symbol "Segoe UI Emoji" nil 'append)
       (set-fontset-font t 'symbol "Symbola" nil 'append)

       (:with-hook (prog-mode-hook text-mode-hook conf-mode-hook)
                   (:hook display-line-numbers-mode))
       (:with-hook text-mode-hook
                   (:hook visual-line-mode)))

(setup editor
  ;; Lines should be 80 characters wide, not 72
  (setq fill-column 80)
  (set-default 'fill-column 80)
  ;; Never insert tabs
  (set-default 'indent-tabs-mode nil)

  ;; Show me empty lines after buffer end
  (set-default 'indicate-empty-lines t)

  ;; Easily navigate sillycased words
  (global-subword-mode 1)

  ;; Don't break lines for me, please
  ;; (setq-default truncate-lines t)

  (setq-default lexical-binding t)

  ;; Configure mac modifiers to be what you expect, and turn off the bell noise
  (when (equal system-type 'darwin)
    (with-no-warnings
      (setq mac-command-modifier      'super
            ns-command-modifier       'super
            mac-option-modifier       'meta
            ns-option-modifier        'meta
            mac-right-option-modifier 'none
            ns-right-option-modifier  'none)))

  ;; add timestamps to files
  (add-hook 'before-save-hook 'time-stamp)
)


(setup whitespace
       (setq backward-delete-char-untabify-method 'hungry)
       (setq next-line-add-newlines nil)
       (setq sentence-end-double-space nil)
       (setq-default indent-tabs-mode nil)
       (setq-default indicate-empty-lines nil)
       (setq-default tab-always-indent nil)
       (setq-default tab-width 4)
       (:with-hook before-save-hook
         (:hook delete-trailing-whitespace)))

(setup encoding
       (setq coding-system-for-read 'utf-8-unix)
       (setq coding-system-for-write 'utf-8-unix)
       (setq default-process-coding-system '(utf-8-unix utf-8-unix))
       (setq locale-coding-system 'utf-8-unix)
       (setq selection-coding-system 'utf-8)
       (setq x-select-request-type nil)
       (setq-default buffer-file-coding-system 'utf-8-unix)
       (prefer-coding-system 'utf-8-unix)
       (set-clipboard-coding-system 'utf-8)
       (set-default-coding-systems 'utf-8-unix)
       (set-keyboard-coding-system 'utf-8-unix)
       (set-language-environment "UTF-8")
       (set-selection-coding-system 'utf-8)
       (set-terminal-coding-system 'utf-8-unix))

(setup files
       (setq auto-mode-case-fold nil)
       (setq auto-save-default t)
       (when (not (file-directory-p (expand-file-name "auto-save" *emacs-cache/*)))
       (make-directory (expand-file-name "auto-save" *emacs-cache/* )))
       (setq auto-save-file-name-transforms `((".*" ,(expand-file-name "auto-save/" *emacs-cache/*) t)))
       (setq auto-save-interval 32)
       (setq auto-save-list-file-prefix nil)
       (setq auto-save-timeout 10)
       (when (not (file-directory-p (expand-file-name "backups" *emacs-cache/*)))
       (make-directory (expand-file-name "backups" *emacs-cache/*)))
       (setq backup-directory-alist `((".*" . ,(expand-file-name "backups" *emacs-cache/*))))
       (setq backup-inhibited nil)
       (setq create-lockfiles nil)
       (setq delete-by-moving-to-trash nil)
       (setq find-file-suppress-same-file-warnings t)
       (setq find-file-visit-truename t)
       (setq load-prefer-newer t)
       (setq make-backup-files t)
       (setq require-final-newline t)
       (setq vc-follow-symlinks t)
       )

(setup minibuffer
       (file-name-shadow-mode 1)
       (minibuffer-depth-indicate-mode 1)
       (minibuffer-electric-default-mode 1)
       (fset #'yes-or-no-p #'y-or-n-p)
       (setq enable-recursive-minibuffers t)
       (setq file-name-shadow-properties '(invisible t intangible t))
       (setq minibuffer-eldef-shorten-default t)
       ;;(setq minibuffer-prompt-properties
       ;;      '(read-only t cursor-intangible t face minibuffer-prompt))
       (setq read-answer-short t)
       (setq read-extended-command-predicate #'command-completion-default-include-p)
       (setq use-short-answers t)
       ;;(:with-hook minibuffer-setup-hook
       ;;  (:hook cursor-intangible-mode))
       )

(setup misc
       (setq ad-redefinition-action 'accept)
       (setq bidi-inhibit-bpa t)
       (setq command-line-ns-option-alist nil)
       (setq confirm-kill-emacs 'y-or-n-p)
       (setq confirm-kill-processes nil)
       (setq custom-file (save-in-etc-file! "custom.el"))
       (setq default-input-method "TeX")
       (setq ffap-machine-p-known 'reject)
       (setq inhibit-compacting-font-caches t)
       (setq inhibit-default-init t)
       (setq jit-lock-defer-time nil)
       (setq jka-compr-verbose nil)
       (setq native-comp-async-jobs-number 24)
       (setq native-comp-async-report-warnings-errors nil)
       (setq read-file-name-completion-ignore-case t)
       (setq read-process-output-max (* 64 1024))
       (setq redisplay-skip-fontification-on-input t)
       (setq ring-bell-function 'ignore)
       )

(setup mouse
       (setq focus-follows-mouse t)
       (setq make-pointer-invisible t)
       (setq mouse-1-click-follows-link t)
       (setq mouse-autoselect-window t)
       (setq mouse-wheel-follow-mouse t)
       (setq mouse-wheel-progressive-speed nil)
       (setq mouse-wheel-scroll-amount '(3 ((shift) . hscroll)))
       (setq mouse-wheel-scroll-amount-horizontal 2)
       (setq mouse-yank-at-point t))

(setup scratch
       (setq initial-major-mode 'fundamental-mode)
       (setq initial-scratch-message nil))

(setup scrolling
       (setq auto-hscroll-mode t)
       (setq auto-window-vscroll nil)
       (setq fast-but-imprecise-scrolling t)
       (setq scroll-conservatively 101)
       (setq scroll-margin 8)
       (setq scroll-preserve-screen-position t)
       (setq scroll-step 1))

(setup selection
       (setq kill-do-not-save-duplicates t)
       (setq select-enable-clipboard t)
       (setq select-enable-primary t)
       (setq x-select-enable-clipboard-manager nil))

(setup windows
       (setq split-height-threshold nil)
       (setq split-width-threshold 160)
       (setq window-divider-default-bottom-width 8)
       (setq window-divider-default-places t)
       (setq window-divider-default-right-width 8)
       (setq window-resize-pixelwise nil))

(provide 'mf-settings)
