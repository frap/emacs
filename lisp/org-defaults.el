;;; org-defaults.el --- Org Defaults  -*- lexical-binding: t -*-
 ;;;; general settings
  (setq org-adapt-indentation nil)      ; No, non, nein, όχι!
  ;; Prevent auto insertion of blank lines before headings and list items
  (setq org-blank-before-new-entry '((heading)
                                     (plain-list-item)))
   ;; http://emacs.stackexchange.com/a/17513/115, values: nil, t, 'reverse
  (setq org-special-ctrl-a/e nil)
  (setq org-special-ctrl-k nil)
  (setq org-M-RET-may-split-line '((default . nil)))
  ;; Single key command execution when at beginning of a headline
  (setq org-use-speed-commands t)     ;? speed-key opens Speed Keys help
  (setq org-speed-commands-user '(("m" . org-mark-subtree)))
  (setq org-hide-emphasis-markers nil)  ;; so dont see text markers aka bold italic
  (setq org-hide-macro-markers nil)
  (setq org-hide-leading-stars nil)
  (setq org-cycle-separator-lines 0)
  (setq org-structure-template-alist    ; CHANGED in Org 9.3, Emacs 27.1
        '(("s" . "src")
          ("E" . "src emacs-lisp")
          ("e" . "example")
          ("q" . "quote")
          ("v" . "verse")
          ("V" . "verbatim")
          ("c" . "center")
          ("C" . "comment")))
  (setq org-catch-invisible-edits 'smart) ;; try not to accidently do wierd stuff in invisible regions : show smart, error
  (setq org-return-follows-link t)
  (setq org-loop-over-headlines-in-active-region 'start-level)
  ;; Setup list of Org modules that should always be loaded together
  ;; with Org.
  (setq org-modules '(org-id org-attach ol-info))
  ;; Allow _ and ^ characters to sub/super-script strings but only when
  ;; string is wrapped in braces
  (setq org-use-sub-superscripts '{}) ; In-buffer rendering
  (setq org-pretty-entities t)        ; special symbols, latex
  ;; Render subscripts and superscripts in Org buffers
  (setq org-pretty-entities-include-sub-superscripts t)
  (setq org-insert-heading-respect-content t)

  ;;;; code blocks
  (setq org-hide-block-startup nil)
  (setq org-fontify-quote-and-verse-blocks t
        org-fontify-whole-heading-line t)
  (setq org-confirm-babel-evaluate nil)
  (setq org-src-window-setup 'current-window)
  (setq org-edit-src-persistent-message nil)
  (setq org-src-fontify-natively t) ;; Display entities like \tilde, \alpha, etc in UTF-8 characters
  (setq org-src-preserve-indentation t)
  (setq org-src-tab-acts-natively t)
  (setq org-edit-src-content-indentation 0)

;;;; images
(setq org-startup-with-inline-images t)
(setq org-image-actual-width '(300))

;;;; export
  (setq org-export-with-toc t)
  (setq org-export-headline-levels 8)
  (setq org-export-in-background t)     ; run export processes in external emacs process
  (setq org-export-with-section-numbers nil)
  (setq org-export-with-smart-quotes t)
  (setq org-export-with-sub-superscripts '{}) ; don't treat lone _ / ^ as sub/superscripts, require _{} / ^{}
  (setq org-export-dispatch-use-expert-ui nil)
  (setq org-html-coding-system 'utf-8-unix)
  (setq org-html-todo-kwd-class-prefix "keyword ")
  (setq org-html-htmlize-output-type nil)
  (setq org-html-head-include-default-style nil)
  (setq org-html-head-include-scripts nil)
  (require 'ox-texinfo)
  (require 'ox-md)

  ;; fold / overview  - collapse everything, show only level 1 headlines
  ;; content          - show only headlines
  ;; nofold / showall - expand all headlines except the ones with :archive:
  ;;                    tag and property drawers
  ;; showeverything   - same as above but without exceptions
  (setq org-startup-folded 'content)

  ;; https://orgmode.org/manual/Clean-view.html
  (setq org-startup-indented t)       ;;; removed leading * for nicer view
  (with-eval-after-load 'org-indent
    (setq org-indent-indentation-per-level 1)) ;Default = 2

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (clojure . t)
     (shell . t)
     (python . t)
     (latex . t)
     ))
   ;; change CAPITAL Keywords to lowercase
  (defun org-syntax-convert-keyword-case-to-lower ()
    "Convert all #+KEYWORDS to #+keywords."
    (interactive)
    (save-excursion
      (goto-char (point-min))
      (let ((count 0)
            (case-fold-search nil))
        (while (re-search-forward "^[ \t]*#\\+[A-Z_]+" nil t)
          (unless (s-matches-p "RESULTS" (match-string 0))
            (replace-match (downcase (match-string 0)) t)
            (setq count (1+ count))))
        (message "Remplacement de %d occurrences" count))))
  (defun discard-history ()
    "Discard undo history of org src and capture blocks."
    (setq buffer-undo-list nil)
    (set-buffer-modified-p nil))
  (define-advice org-return (:around (f &rest args))
    (let ((org-src-preserve-indentation t))
      (apply f args)))
  (define-advice org-cycle (:around (f &rest args))
    (let ((org-src-preserve-indentation t))
      (apply f args)))
  (defun org-babel-edit-prep:emacs-lisp (_)
    "Setup Emacs Lisp buffer for Org Babel."
    (setq lexical-binding t)
    (setq-local flycheck-disabled-checkers '(emacs-lisp-checkdoc)))
  ;; open directory links in `dired'
  (add-to-list 'org-file-apps '(directory . emacs))

  ;; open files in the same window
  (add-to-list 'org-link-frame-setup '(file . find-file))
  (setq org-indirect-buffer-display 'current-window)

(defun gas/org-mode-setup ()
  (org-indent-mode)       ;; turn on org indent
  (variable-pitch-mode 1) ;; turn on variable-pitch
  (auto-fill-mode 0)      ;; turn off auto-fill
  (visual-line-mode 1)    ;; turn on visual-line-mode
  (show-paren-mode 1)     ;; show parentheses
  )

(provide 'org-defaults)
;;; org-defaults.el ends here
