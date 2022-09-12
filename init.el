;;; init.el --- Gas Init -*- lexical-binding: t -*-

;; Copyright (C) 2001-2022 Gas
;; Timestamp: <>

;; Author: Gas <gas@tuatara.red>
;; Version: 1.0
;; Package-Version: 0.1
;; Created: Sometime during the Covid-19 lockdown
;; Keywords: configuration, emacs
;; URL: https://github.com/frap/emacs.d
;; Package-Requires: ((emacs "27.2"))

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
                   (message "* * %s... terminé!!" ,msg)))))

;; load if-let
(require 'subr-x)
;;; Bootstrap
;;; Set up extra load paths and functionality
;; Add Lisp directory to `load-path'.
;; Add our custom lisp modules to the Emacs load path so they can be discovered.
(push (expand-file-name "lisp/" (file-name-directory user-init-file)) load-path)
;;; ============================================================================
;;; Specify the directory paths
;;; ============================================================================
(require 'config-path)
;; Since we might be running in CI or other environments, stick to
;; XDG_CONFIG_HOME value if possible.

;;; ============================================================================
;;; Set up the package manager
;;; ============================================================================
(require 'init-elpa)

;; Setup `custom-file`.
(setq custom-file (concat *emacs-config/* "custom.el"))

;; load autoloads file
(unless elpa-bootstrap-p
  (unless (file-exists-p *emacs-autoloads-file*)
          (error "Le fichier autoloads n'existe pas, veuillez exécuter '%s'"
                 "eru install emacs"))
  (load *emacs-autoloads-file* nil 'nomessage))

;;; core
;;(require 'init-env)
(require 'init-kbd)
(require 'init-startup)
;;(require 'init-fn-macros)
(require 'init-editor)
;;(require 'init-ui)
;;(require 'init-buffer)
;;(require 'init-window)
;;; utilities
;;(require 'init-selection)
(require 'init-project)
(require 'init-vcs)
;;(require 'enfer-pkg-builtin)

;;; languages
;;(require 'init-ide)
;;(require 'init-lisp)
;;(require 'init-elisp)
;;(require 'init-clisp)
;;(require 'init-clojure)
(require 'init-fennel)
;;(require 'init-ess)
;;(require 'init-utils)

;; Org & Roam
(require 'org-defaults)
;;(require 'init-notes)
(require 'init-file-templates)

;;; user config & some defaults
(require 'init-usersetup)
;;(require 'init-sanity)

;; I don't use `customize' interface, but .dir-locals.el put 'safe'
;; variables into `custom-file'. And to be honest, I hate to allow
;; them every time I restart Emacs.
(when (file-exists-p custom-file)
  (load custom-file nil 'nomessage))

(provide 'init)
;;; init.el ends here
