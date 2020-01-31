;;; flycheck-clazy.el --- Flycheck integration for clazy static checking -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'flycheck)
(require 'dash)

(flycheck-def-option-var flycheck-clazy-build-path "." c/c++-clazy
  ""
  :type 'directory
  :safe #'stringp)

(flycheck-def-option-var flycheck-clazy-extra-args '() c/c++-clazy
  "Extra arguments to pass to clazy"
  :type (repeat 'list)
  :safe (lambda (x) (-all? 'stringp x)))

(flycheck-def-option-var flycheck-clazy-root nil c/c++-clazy
  ""
  :type 'directory
  :safe #'stringp)

(defun flycheck-clazy--find-project-root (_checker)
  "Determine the project root for CHECKER using in the following order:
`flycheck-clazy-root`, `projectile-root`, location of a `compile_commands.json`, `vc-root` and current directory."
  (let ((project-root flycheck-clazy-root))
    (when (and (not project-root)
               (member 'projectile-mode minor-mode-list))
      (setq project-root (projectile-project-root)))
    (unless project-root
      (setq project-root (locate-dominating-file (buffer-file-name) "compile_commands.json")))
    (unless project-root
      (setq project-root (vc-root-dir)))
    (unless project-root
      (message "Could not determine project root, trying current directory.")
      (setq project-root (flycheck-clazy--current-source-dir)))
    project-root))

(defun flycheck-clazy--current-source-dir ()
  "Directory of current source file."
  (file-name-directory (buffer-file-name)))

(defun flycheck-clazy--verify (_checker)
  "Verifies CHECKER."
  (list (flycheck-verification-result-new
         :label "Project Root"
         :message (format "%s" (flycheck-clazy--find-project-root nil))
         :face (if (file-directory-p (flycheck-clazy--find-project-root nil)) 'success '(bold error)))))

(flycheck-define-checker c/c++-clazy
  "A flycheck backend for the clazy code analyzer"
  :command ("clazy-standalone"
            (option "-p" flycheck-clazy-build-path)
            (eval (concat "-extra-arg=-I" (flycheck-clazy--current-source-dir)))
            (eval (mapconcat 'identity flycheck-clazy-extra-args " "))
            "--ignore-included-files"
            source)
  :error-patterns ((error line-start (file-name) ":" line ":" column ": error: " (message) line-end)
                   (warning line-start (file-name) ":" line ":" column ": warning: " (message) line-end)
                   (info line-start (file-name) ":" line ":" column ": note: " (message) line-end))
  :working-directory flycheck-clazy--find-project-root
  :modes (c-mode c++-mode)
  :verify flycheck-clazy--verify
  :predicate (lambda () buffer-file-name)
  :next-checkers ((error . c/c++-cppcheck)))

;;;###autoload
(defun flycheck-clazy-setup ()
  "Setup flycheck clazy."
  (add-to-list 'flycheck-checkers 'c/c++-clazy t))

(provide 'flycheck-clazy)

;;; flycheck-clazy.el ends here
