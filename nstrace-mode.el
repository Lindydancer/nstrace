;;; nstrace-mode.el --- Major mode for NSTRACE log files.

;; Copyright (C) 2015-2016  Anders Lindgren

;; Author: Anders Lindgren
;; Version: 0.0.1
;; URL: https://github.com/Lindydancer/nstrace
;; Package-Requires: ((obj-font-lock "0.0.4"))

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

;; Major mode for NSTRACE logs.
;;
;; When the NS port of Emacs is built with NSTRACE_ENABLED (see
;; nsterm.h for details) a log is produced.
;;
;; This major mode provides naviagtion and syntax highlighting
;; support.

;; See also
;;
;; - objc-font-lock -- This package highlight Objective-C method
;;   calls.  If present, it is used by this package to highlight log
;;   entries originating from Objective-C methods.

;;; Code:

(defvar nstrace-entry-re
  "^\\([a-zA-Z0-9.]+\\)\\s-*:\\s-*\\([0-9]+\\): \\[\\s-*\\([0-9]+\\)]  "
  "Regepx matching a NSTRACE log entry line.")

(defvar nstrace-font-lock-keywords
  `((,(concat nstrace-entry-re "\\(?:| \\)*\\(\\_<[a-zA-Z0-9_]+\\_>\\)?")
     (1 'success)
     (2 'font-lock-keyword-face)
     (3 'font-lock-constant-face)
     (4 'font-lock-function-name-face nil t)))
  "Highlight rules for `nstrace-mode'.")


(defvar nstrace-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'nstrace-goto-entry)
    map)
  "Keymap for NSTRACE log mode.")


(defun nstrace-goto-entry ()
  "Go to the source corresponding to the NSTRACE log entry at point."
  (interactive)
  (beginning-of-line)
  (when (looking-at nstrace-entry-re)
    (let ((file-name (match-string 1))
          (line-number (string-to-number (match-string 2))))
      (let ((candidates (list file-name (concat "src/" file-name)))
            (found nil))
        (while candidates
          (let ((c (pop candidates)))
            (when (file-exists-p c)
              (setq found c)
              (setq candidates nil))))
        (when found
          (forward-line)
          (let ((win (selected-window)))
            (find-file-other-window found)
            (goto-line line-number)
            (select-window win)))))))


;;;###autoload
(define-derived-mode nstrace-mode prog-mode "nstrace"
  (setq font-lock-defaults '(nstrace-font-lock-keywords nil))
  (set (make-local-variable 'truncate-partial-width-windows) t)
  (when (require 'objc-font-lock)
    (objc-font-lock-mode 1)))


;;;###autoload
(defun nstrace-magic-p ()
  "True, when file is a NSTRACE log file."
  (re-search-forward "^nsterm\.m  : *[0-9]+: \\[ +[0-9]+\\]  ns_term_init$"
                     magic-mode-regexp-match-limit t))


;;;###autoload
(add-to-list 'magic-mode-alist
             '(nstrace-magic-p . nstrace-mode))



(provide 'nstrace-mode)

;;; nstrace-mode.el ends here
