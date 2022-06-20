;;; init.el --- Gas Init -*- lexical-binding: t -*-

;; Copyright (C) 2001-2022 Gas

;; Author: Gas <gas@tuatara.red>
;; Version: 1.0
;; Package-Version: 0.1
;; Created: Sometime during the Covid-19 lockdown
;; Keywords: configuration, emacs
;; URL: https://github.com/frap/emacs.d
;; Package-Requires: ((emacs "27.2"))

;; Timestamp: <>
;; This file is not part of GNU Emacs.

;;; Commentary:
;; This file maybe automatically tangled from config.org.
;; Hand edits will be overwritten!
;; Je t'ai prévenu putain!

;;; Code:
(defmacro with-message! (message &rest body)
"Execute BODY, with MESSAGE.
      If body executes without errors, ** MESSAGE... terminé will be displayed."
	(declare (indent 1))
	(let ((msg (gensym)))
	  `(let ((,msg ,message))
	     (unwind-protect (progn (message "%s..." ,msg)
				    ,@body)
	       (message "** %s... terminé!!" ,msg)))))

    ;; load if-let
    (require 'subr-x)
    ;;; Set up extra load paths and functionality
    ;; Since we might be running in CI or other environments, stick to
    ;; XDG_CONFIG_HOME value if possible.
    (let ((*emacs-config/* (if-let ((xdg (getenv "XDG_CONFIG_HOME")))
			     (expand-file-name "emacs/" xdg)
			   user-emacs-directory)))
    ;; Add Lisp directory to `load-path'.
    (add-to-list 'load-path (expand-file-name "lisp" *emacs-config/*)))

  ;;; Bootstrap
  ;;; ============================================================================
  ;;; Specify the directory paths
  ;;; ============================================================================
    (require 'config-path)

  ;;; ============================================================================
  ;;; Set up the package manager
  ;;; ============================================================================
    (require 'init-elpa)

  ;;; ============================================================================
  ;;; Specify the directory paths
  ;;; ============================================================================
    
    (setup (:require prot-common))
    
    ;;; Initialise Gas Constants
      (defconst *is-nativecomp?* (if (fboundp 'native-comp-available-p) (native-comp-available-p)))
      (defconst *is-gui?* (display-graphic-p))
      (defconst *is-mac?*     (eq system-type 'darwin))
      (defconst *is-linux?*   (eq system-type 'gnu/linux))
      (defconst *is-termux?*
        (string-suffix-p "Android" (string-trim (shell-command-to-string "uname -a"))))
      (defconst *hostname* (system-name))
    
      (defmacro fn (&rest body)
        `(lambda () ,@body))
    
      (defun doom-enlist (exp)
        "Return EXP wrapped in a list, or as-is if already a list."
        (declare (pure t) (side-effect-free t))
        (if (proper-list-p exp) exp (list exp)))

  ;; Setup `custom-file`.
    (setq custom-file (concat *emacs-config/* "custom.el"))

    ;; load autoloads file
    (unless elpa-bootstrap-p
	(unless (file-exists-p *emacs-autoloads-file*)
	  (error "Le fichier autoloads n'existe pas, veuillez exécuter '%s'"
		 "eru install emacs"))
	(load *emacs-autoloads-file* nil 'nomessage))

;;; core
(require 'init-env)
(require 'init-kbd)
(require 'init-startup)
(require 'init-fn-macros)
(require 'init-editor)
(require 'init-ui)
(require 'init-buffer)
(require 'init-window)
;;; utilities
(require 'init-selection)
(require 'init-project)
(require 'init-vcs)
;;(require 'enfer-pkg-builtin)

;;; languages
(require 'init-ide)
(require 'init-lisp)
(require 'init-elisp)
(require 'init-clisp)
(require 'init-clojure)
(require 'init-fennel)
(require 'init-ess)
(require 'init-utils)

;; Org & Roam
(require 'init-notes)
(require 'init-file-templates)

;;; user config & some defaults
(require 'init-usersetup)
(require 'init-sanity)

;; I don't use `customize' interface, but .dir-locals.el put 'safe'
;; variables into `custom-file'. And to be honest, I hate to allow
;; them every time I restart Emacs.
(when (file-exists-p custom-file)
  (load custom-file nil 'nomessage))

(provide 'init)
;;; init.el ends here
