;;; init.el --- Gas Emacs Init -*- lexical-binding: t -*-
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


;;; Code:

(eval-when-compile
  (require 'subr-x)) ;; adds string-trim
;;; Bootstrap
;;; Avoid the "loaded old bytecode instead of newer source" pitfall.
(setq load-prefer-newer t)
;; Add our custom lisp modules to the Emacs load path so they can be discovered.
(push (expand-file-name "lisp/" (file-name-directory user-init-file)) load-path)
;;; ============================================================================
;;; Specify the directory paths
;;; ============================================================================
(require 'config-path)

;;;============================================================================
;;; Set up the package manager
;;; ============================================================================
(require 'init-elpa)

;;; core
;; sanity settings
(require 'init-sanity)

;; startup packages & gcmh
;;(require 'init-startup)
(require 'init-kbd)

;; navigation & Editor setup
;;(require 'init-selection)
(require 'init-editor)
;;(use-package corgi-editor)
;;(use-package corgi-commands)

;;; utilities
(require 'init-selection)
(require 'init-project)
;; Powerful Git integration. Corgi already ships with a single keybinding for
;; Magit, which will be enabled if it's installed (`SPC g s' or `magit-status').
(require 'init-vcs)
;;(require 'ínit-kbd)

;;; Setup the Theme
(require 'init-ui)
;;(require 'init-buffer)
;;(require 'init-window)

;;; Coding Setup
(require 'init-ide)
;;(require 'init-lisp)
(require 'init-elisp)
;;(require 'init-clisp)
(require 'init-clojure)
;;(require 'init-fennel)
;; R
(require 'init-ess)
;;(require 'init-utils)
(require 'init-javascript)
(require 'init-utils-coding)

;; Org Note Taking
(require 'init-notes)
(require 'init-file-templates)

;;; user config & some defaults
(require 'init-usersetup)

;; I don't use `customize' interface, but .dir-locals.el put 'safe'
;; variables into `custom-file'. And to be honest, I hate to allow
;; them every time I restart Emacs.
(when (file-exists-p custom-file)
  (load custom-file nil 'nomessage))

(provide 'init)
;;; init.el ends here
