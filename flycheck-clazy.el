;;; flycheck-clazy.el --- Flycheck integration for clazy static checking -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'flycheck)


(flycheck-define-checker c/c++-clazy
  "A flycheck backend for the clazy code analyzer"
  :command ("clazy-standalone"
            )
  :modes (c-mode c++-mode)
  :predicate (lambda () buffer-file-name)
  :next-checkers ((error . c/c++-cppcheck)))

;;;###autoload
(defun flycheck-clazy-setup ()
  "Setup flycheck clazy."
  (add-to-list 'flycheck-checkers 'c/c++-clazy t))

(provide 'flycheck-clazy)

;;; flycheck-clazy.el ends here
