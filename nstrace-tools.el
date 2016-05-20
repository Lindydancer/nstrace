;;; nstrace-tools.el --- Tools for maintaining module using NSTRACE.

;; Copyright (C) 2015-2016  Anders Lindgren

;; Author: Anders Lindgren
;; URL: https://github.com/Lindydancer/nstrace

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Tools for maintaining module using NSTRACE.

;; See nstrace-mode.el for version number etc.

;;; Code:

(defun nstrace-which-class ()
  "The name of the current Objective-C class."
  (let ((p (point))
        (res nil))
    (save-excursion
      (and (re-search-backward
            "@implementation[ \t]+\\([a-zA-Z_][a-zA-Z0-9_]*\\)\\_>" nil t)
           (progn
             (setq res (match-string-no-properties 1))
             (re-search-forward "@end\\_>" nil t))
           (< p (point))
           res))))


(defun nstrace-which-function ()
  "The name of function.

For an Objective-C method, the fully qualified name is returned,
for example \"[EmacsWindow constrainFrameRect:toScreen:]\"."
  (require 'which-func)
  (let ((name (which-function)))
    (when name
      (when (and (derived-mode-p 'objc-mode)
                 (string-match "^-" name))
        (let ((class (nstrace-which-class)))
          (when class
            (setq name (concat "[" class " " (substring name 1) "]"))))))
    name))


;;;###autoload
(defun nstrace-source-next-entry-macro ()
  "Go to the next NSTRACE entry macro.

Place point at function name argument."
  (interactive)
  (let ((p (point)))
    (forward-line)
    (if (re-search-forward "^[ \t]*NSTRACE\\(_WHEN\\|_UNLESS\\)?[ \t]*(" nil t)
        (progn
          (when (match-beginning 1)
            (re-search-forward ",[ \t]*" nil t))
          t)
      (goto-char p)
      nil)))


;;;###autoload
(defun nstrace-source-next-broken-entry-macro ()
  (interactive)
  (let ((p (point)))
    (while (nstrace-source-next-entry-macro)
      (forward-char)                      ; Skip initial quote.
      (let ((name (nstrace-which-function)))
        (setq name (regexp-quote name))
        (let ((arr (split-string name ":")))
          (setq name (mapconcat 'identity arr ":.*"))
          (unless (looking-at name)
            (user-error "Found broken NSTRACE entry macro")))))
    (goto-char p)
    (message "No more broken NSTRACE macros found.")))

(provide 'nstrace-tools)

;;; nstrace-tools.el ends here
