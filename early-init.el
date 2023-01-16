;;; early-init.el --- Early Customisations -*- lexical-binding: t; no-byte-compile: t; -*-
;; Copyright (C) 2001-2023 Gas
;; Timestamp: <>
;; Author: Gas <gas@tuatara.red>
;; Version: 1.0
;; Package-Version: 0.8
;; Created: Sometime during the Covid-19 lockdown
;; Keywords: configuration, emacs
;; URL: https://github.com/frap/emacs
;; Package-Requires: ((emacs "27.2"))

;; This file is not part of GNU Emacs.

;;; Commentary:
;; This file maybe automatically tangled from config.org.
;; Hand edits will be overwritten!
;; Je t'ai prévenu putain!

;;
;;; Code:

(message "Chargement en cours early-init.el...")

(define-advice load (:before (feature &rest _))
    "Message the user when loading a library."
    (with-temp-message (format "En cours de chargement de la bibliothèque: '%s'" feature)))

;;; ============================================================================
;;; Turn off Emacs "package" manager
;;; ============================================================================
;; Pre-configure the package manager settings before it is loaded.
(setq package-enable-at-startup nil)
(setq package-quickstart nil)


;;; ============================================================================
;;; Emacs startup optimisations
;;; ============================================================================
;; Garbage collection slows down startup time, so we maximise the threshold for
;; it to run, and we will later reset it.
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6
      file-name-handler-alist nil
      site-run-file nil
      read-process-output-max (* 10 1024 1024)
      bidi-inhibit-bpa t)

(defvar gas/gc-cons-threshold (* 100 1024 1024))   ;; 100mb

(add-hook 'emacs-startup-hook
          #'(lambda () (setq gc-cons-threshold gas/gc-cons-threshold
                             gc-cons-percentage 0.1)))

;; file-name-handler-alist is consulted on various I/O functions such as
;; REQUIRE, slowing down startup time, so we set it to NIL, and establish
;; a hook to restore when Emacs is finished starting.
(unless (or (daemonp) noninteractive)
  (let ((file-name-handler-alist/old file-name-handler-alist))
    (setq file-name-handler-alist nil)
    (add-hook 'emacs-startup-hook
              (lambda ()
                (let ((value (delete-dups
                              (append file-name-handler-alist
                                      file-name-handler-alist/old))))
                  (setq file-name-handler-alist value))))))

(unless (daemonp)
  (advice-add #'tty-run-terminal-initialization :override #'ignore)
  (add-hook 'window-setup-hook
            (lambda ()
              (advice-remove #'tty-run-terminal-initialization #'ignore)
              (tty-run-terminal-initialization (selected-frame) nil t))))


;;; ============================================================================
;;; Frame parameters for faster startup
;;; ============================================================================
(setq-default
 initial-frame-alist '((width . 170)
                       (height . 56)
                       (tool-bar-lines . 0)
                       (tooltip-mode 0)
                       (vertical-scroll-bars . 0)
                       (bottom-divider-width . 0)
                       (right-divider-width . 1)
                       (font . "Iosevka Curly 14")
                       (blink-cursor-mode 0)
                       (column-number-mode 1)
                       (display-time-mode 0)
                       (fringe-mode '(4 . 0))
                       (window-divider-mode 1)
                       )
 default-frame-alist initial-frame-alist
 frame-inhibit-implied-resize t           ;; dont resize
 frame-resize-pixelwise t                 ;; as GUI use pixels
 x-gtk-resize-child-frames 'resize-mode
 fringe-indicator-alist (assq-delete-all 'truncation fringe-indicator-alist))

(setq truncate-partial-width-windows nil)

(when (fboundp #'tool-bar-mode)
  (tool-bar-mode -1))

(when (fboundp #'scroll-bar-mode)
  (scroll-bar-mode -1))
(add-hook 'minibuffer-setup-hook #'(lambda ()
                                     (setq gc-cons-threshold most-positive-fixnum)))
(add-hook 'minibuffer-exit-hook #'(lambda ()
                                    (garbage-collect)
                                    (setq gc-cons-threshold gas/gc-cons-threshold)))

(message "Chargement du early-init.el terminé!")
(provide 'early-init)
;;; early-init.el ends here
