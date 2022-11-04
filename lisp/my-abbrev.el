;;; my-abbrev --- Abbrev stuff  -*- lexical-binding: t; -*-
;;; Commentary:

;; This adds various abbrevs for various modes.  Abbrevs are useful to
;; avoid typos, for instance.  To prevent the expansion, type ``word
;; C-q SPC'' instead of ``word SPC''.

;;; Code:

(clear-abbrev-table global-abbrev-table)

(define-abbrev-table 'global-abbrev-table
  '(("prots@" "ports@")

    ("supprots" "supports")

    ("het" "the")
    ("teh" "the")

    ("wehn" "when")

    ("perchè" "perché")
    ("perche" "perché")
    ("nonchè" "nonché")
    ("nonche" "nonché")
    ("quetse" "queste")
    ("sovlgimento" "svolgimento")
    ("sovlgere" "svolgere")
    ("sbagilo" "sbaglio")
    ("caffe" "caffè")))

(when (boundp 'text-mode-abbrev-table)
  (clear-abbrev-table text-mode-abbrev-table))

(define-abbrev-table 'text-mode-abbrev-table
  '(("hw" "hardware")
    ("sw" "software")))

(when (boundp 'clojure-mode-abbrev-table)
  (clear-abbrev-table clojure-mode-abbrev-table))

(define-abbrev-table 'clojure-mode-abbrev-table
  '(("erq" "req")))

(when (boundp 'c-mode-abbrev-table)
  (clear-abbrev-table c-mode-abbrev-table))

(define-abbrev-table 'c-mode-abbrev-table
  '(("inculde" "include")
    ("inlcude" "include")))

;; turn on abbrev mode globally
(setq-default abbrev-mode t)

(provide 'my-abbrev)
;;; my-abbrev.el ends here
