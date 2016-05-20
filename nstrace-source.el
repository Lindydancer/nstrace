;;; nstrace-source.el --- Minor mode for modules using NSTRACE

;; Copyright (C) 2015  Anders Lindgren

;; Author: Anders Lindgren

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

;; Minor mode for source codes using NSTRACE.

;;; Code:

(define-minor-mode nstrace-source
  "Minot mode for source code using NSTRACE."
  nil
  nil
  nil)


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



;; ----------------------------------------------------------------------
;; nstrace-log-mode -- major mode for viewing NSTRACE logs
;;

(defvar nstrace-log-re
  "^\\([a-zA-Z0-9.]+\\)\\s-*:\\s-*\\([0-9]+\\): \\[\\s-*\\([0-9]+\\)]  "
  "Regepx matching a NSTRACE log entry line.")

(defvar nstrace-log-font-lock-keywords
  `((,(concat nstrace-log-re "\\(?:| \\)*\\(\\_<[a-zA-Z0-9_]+\\_>\\)?")
     (1 'success)
     (2 'font-lock-keyword-face)
     (3 'font-lock-constant-face)
     (4 'font-lock-function-name-face nil t)))
  "Highlight rules for `nstrace-log-mode'.")


(defvar nstrace-log-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'nstrace-log-goto-entry)
    map)
  "Keymap for NSTRACE log mode.")

(defmacro nstrace-log-define-prog-mode (mode name &rest args)
  "Define a major mode for a programming language.
If `prog-mode' is defined, inherit from it."
  (declare (indent defun))
  `(define-derived-mode
     ,mode ,(and (fboundp 'prog-mode) 'prog-mode)
     ,name ,@args))

(defun nstrace-log-goto-entry ()
  (interactive)
  (beginning-of-line)
  (when (looking-at nstrace-log-re)
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
(nstrace-log-define-prog-mode nstrace-log-mode "nstrace-log"
  (setq font-lock-defaults '(nstrace-log-font-lock-keywords nil))
  (set (make-local-variable 'truncate-partial-width-windows) t)
  (when (require 'objc-font-lock)
    (objc-font-lock-mode 1)))

;;;###autoload
(defun nstrace-log-magic-p ()
  "True, when file is a NSTRACE log file."
  (re-search-forward "^nsterm\.m  : *[0-9]+: \\[ +[0-9]+\\]  ns_term_init$"
                     magic-mode-regexp-match-limit t))

;;;###autoload
(add-to-list 'magic-mode-alist
             '(nstrace-log-magic-p . nstrace-log-mode))



(provide 'nstrace-source)

;;; nstrace-source.el ends here
