;; -*- lexical-binding: t; -*-

(defvar op/mode-line-format-bk mode-line-format
  "Backup of the default `mode-line-format'.")

(setq-default mode-line-format
              '("%e"
                mode-line-front-space
                mode-line-mule-info
                mode-line-client
                mode-line-modified
                mode-line-remote
                mode-line-frame-identification
                mode-line-buffer-identification
                " "
                mode-line-position
                (vc-mode vc-mode)
                " "
                ;; mode-line-modes
                mode-line-misc-info
                mode-line-end-spaces))

(provide 'my-modeline)
;;; my-modeline.el ends here
