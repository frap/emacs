  (use-package org-clock
    :straight nil
    :defer t
    :commands (org-clock-save)
    :init
    (setq
     org-clock-persist-file (expand-file-name "org-clock-save.el" *emacs-etc/* )
     ;; remove clocked tasks with 0:00 duration
     org-clock-out-remove-zero-time-clocks t
     org-clock-persist 'history
     ;; Resume when clocking into task with open clock
     org-clock-in-resume t)
    :config
    (add-hook 'kill-emacs-hook #'org-clock-save))



  (use-package org-refile
    :straight nil
    :defer t
    :init
   ;;;; refile, todo
    (setq org-refile-targets
          '((org-agenda-files . (:maxlevel . 2))
            (nil . (:maxlevel . 2))))
    (setq org-refile-use-outline-path t)
    (setq org-refile-allow-creating-parent-nodes 'confirm)
    (setq org-refile-use-cache t)
    (setq org-reverse-note-order nil)
    (setq org-outline-path-complete-in-steps nil)
    (setq org-refile-target-verify-function #'vulpea-refile-verify-target))

  

  (use-package org-archive
    :straight nil
    :defer t
    :init
    (setq-default
     org-archive-location
     (concat org-directory ".archive/%s_archive" "::" "datetree/*")
     org-archive-save-context-info
     '(time file ltags itags todo category olpath)))

  

  (use-package org-id
    :straight nil
    :defer t
    :hook ((before-save . vulpea-id-auto-assign)
           (org-capture-prepare-finalize . org-id-get-create))
    :init
    (setq org-id-uuid-program
          "uuidgen | tr \"[:upper:]\" \"[:lower:]\"")
    :config
    (org-link-set-parameters
     "id" :activate-func #'vulpea-activate-link)
    (setq
     org-id-track-globally t
     org-id-extra-files
     (list (expand-file-name ".archive/archive" org-directory)
           (expand-file-name ".archive/archive.org" org-directory))
     org-id-link-to-org-use-id t
     org-id-locations-file (expand-file-name "org-id-locations" *xdg-cache/*)))

  

  (use-package org-attach
    :straight nil
    :defer t
    :config
    (setq-default
     org-attach-id-dir (expand-file-name ".data/" vulpea-directory)
     org-attach-auto-tag nil
     org-attach-file-list-property nil
     org-attach-store-link-p 'attached))

  

;;; org-usepackage.el --- Org Use-package setup  -*- lexical-binding: t -*-
(use-package org
  :straight (:type built-in)
  :preface
  ;; Set my default org-export backends. This variable needs to be set before
  ;; org.el is loaded.
  (setq org-export-backends '(ascii html latex md))
  ;; Do not open links of mouse left clicks.
  ;; Default behavior caused inline images in Org buffers to pop up in their
  ;; own buffers when left clicked on by mistake. I can still intentionally
  ;; open links and such images in new buffers by doing C-c C-o.
  (setq org-mouse-1-follows-link nil)
  :mode ("\\.org\\'" . org-mode)
  :hook ((org-mode . visual-line-mode)
         (org-mode . adaptive-wrap-prefix-mode)
         ;; oh, how much I hate it in Org mode buffers
         (org-mode . editor-disable-electric-indent))
  :hook  ((org-capture-mode org-src-mode) . discard-history)
  :commands (org-check-agenda-file
             org-link-set-parameters)
  :custom-face
  (org-block ((t (:extend t))))
  (org-block-begin-line ((t ( :slant unspecified
                              :weight normal
                              :background unspecified
                              :inherit org-block
                              :extend t))))
  (org-block-end-line ((t ( :slant unspecified
                            :weight normal
                            :background unspecified
                            :inherit org-block-begin-line
                            :extend t))))
  (org-drawer ((t (:foreground nil :inherit shadow))))
  :custom
  (org-ellipsis "…")
  :init
  ;; This is where my ~heart~ org files are.
  (setq org-directory
        (if *is-termux?*
            "~/storage/shared/org"
          *notes/*))

  (defun add-path-to-org/ (path)
    (expand-file-name path org-directory))

  :config
 )

(use-package org-modern
:straight t
:config
  (setq org-modern-label-border 1)
  (setq org-modern-variable-pitch nil)
  (setq org-modern-timestamp t)
  (setq org-modern-table t)
  (setq org-modern-table-vertical 1)
  (setq org-modern-table-horizontal 0)
  (setq org-modern-list ; I swap the defaults for + and *
        '((?+ . "•")
          (?- . "–")
          (?* . "◦")))
  (:hook org-mode))

(provide 'org-usepackage)
;;; org-usepackage.el ends here
