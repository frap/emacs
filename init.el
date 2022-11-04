;;; init.el --- Gas Emacs Init -*- lexical-binding: t -*-
;; Copyright (C) 2001-2022 Gas
;; Timestamp: <>
;; Author: Gas <gas@tuatara.red>
;; Version: 1.0
;; Package-Version: 0.7
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

;;; ============================================================================
;;; Set up the package manager
;;; ============================================================================
(require 'init-elpa)

;;; core
;; sanity settings
(require 'init-sanity)

;; startup packages & gcmh
(require 'init-startup)

;; navigation & Editor setup
(require 'init-selection)
(require 'init-editor)

;;(require 'init-ui)
;;(require 'init-buffer)
;;(require 'init-window)
;;; utilities
(require 'init-project)
;; Powerful Git integration. Corgi already ships with a single keybinding for
;; Magit, which will be enabled if it's installed (`SPC g s' or `magit-status').
(require 'init-vcs)
;;(require 'enfer-pkg-builtin)

;; Org Note Taking
(require 'init-notes)
(require 'init-file-templates)

;;; Setup the Theme
(require 'init-ui)

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


;;; user config & some defaults
(require 'init-usersetup)
(require 'init-kbd)

;; I don't use `customize' interface, but .dir-locals.el put 'safe'
;; variables into `custom-file'. And to be honest, I hate to allow
;; them every time I restart Emacs.
(when (file-exists-p custom-file)
  (load custom-file nil 'nomessage))

(provide 'init)
;;; init.el ends here
