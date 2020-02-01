;;; flycheck-clazy.el --- Flycheck integration for clazy static checking -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Björn Larsson

;; Author: Björn Larsson <develop@bjornlarsson.net>
;; Homepage:
;; Version: 1.0.0
;; Package-Requires: ((emacs "25.1")  (dash "2.17.0") (flycheck "32"))
;; Keywords: convenience, languages, tools

;; This file is not part of GNU Emacs.

;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;

;;; Commentary:

;;; Installation:

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
  "Root directory to use as the current working directory for the tool. If not nil it will always be used.
If nil, `flycheck-clazy' will try to automatically determine the root directory using `flycheck-clazy--find-project-root'"
  :type 'directory
  :safe #'stringp)

(defun flycheck-clazy--find-project-root (_checker)
  "Determine the project root for CHECKER using in the following order:
`flycheck-clazy-root', `projectile-root', location of a `compile_commands.json', `vc-root' and current directory."
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
