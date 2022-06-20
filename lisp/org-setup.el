;;; org-setup.el --- Orgmode Setup  -*- lexical-binding: t -*-
;; setup of org-mode keybindings
(setup (:straight org)
  (:also-load org-tempo)
  (:hook gas/org-mode-setup)
  (:global "C-c a"  org-agenda
           "C-c c"  org-capture
           "C-c l"  org-store-link)
  (:bind   "C-'"  nil
           "C-,"  nil
           "<C-return>"    nil
           "<C-S-return>"  nil
           "C-M-S-<right>" nil
           "C-M-S-<left>"  nil
           "C-c S-l"       org-toggle-link-display
           "C-c C-S-l"     org-insert-last-stored-link)

  (push '("conf-unix" . conf-unix) org-src-lang-modes)
  (:load-after hl-fill-column
    (:hook gas/org-mode-setup)))

(setup (:require org-indent)
  (:load-after org
    (setq evil-auto-indent nil)
    (org-indent-mode 1)
    (:hide-mode)))

(setup (:straight org-appear)
  (:load-after org
    (:option org-appear-autoemphasis t
             org-appear-autoentities t
             org-appear-autokeywords t
             org-appear-autolinks nil
             org-appear-autosubmarkers t
             org-appear-delay 0)
    (:hook-into org-mode)))

  (setup org-faces
    (:load-after org-indent
      (dolist (face-cons '((org-document-title . 1.75)
                           (org-level-1 . 1.5)
                           (org-level-2 . 1.25)
                           (org-level-3 . 1.12)
                           (org-level-4 . 1.05)
                           (org-level-5 . 1.0)
                           (org-level-6 . 1.0)
                           (org-level-7 . 1.0)
                           (org-level-8 . 1.0)))
        (cl-destructuring-bind (face . height) face-cons
          (set-face-attribute face
                              nil
                              :weight 'bold
                              :font "Iosevka Aile"
                              :height height))))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-table nil  :inherit 'fixed-pitch)
  (set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)

  ;; Get rid of the background on column views
  (set-face-attribute 'org-column nil :background nil)
  (set-face-attribute 'org-column-title nil :background nil))
