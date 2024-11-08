;;; consult-abbrev.el --- preview abbrevs using consult  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Paul D. Nelson

;; Author: Paul D. Nelson <nelson.paul.david@gmail.com>
;; Version: 0.1
;; URL: https://github.com/ultronozm/consult-abbrev.el
;; Package-Requires: ((emacs "29.1") (consult "1.4"))
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides a `consult' interface for viewing abbrevs.
;; It shows both global and mode-specific (local) abbrevs with live preview
;; of their expansions.
;;
;; Installation: download this file, do `M-x package-install-file'.
;;
;; Usage:
;; M-x consult-abbrev

;;; Code:

(require 'consult)

(defgroup consult-abbrev nil
  "Consult interface for abbrevs."
  :group 'consult)

(defvar consult-abbrev--narrow
  '((?g . "Global")
    (?l . "Local"))
  "Abbrev narrowing configuration.")

(defun consult-abbrev--format-expansion (str)
  "Format expansion STR for display in `completing-read'."
  (replace-regexp-in-string "\n" " ⏎ " str))

(defun consult-abbrev--make-candidate (sym table)
  "Create completion candidate from abbrev symbol SYM in TABLE."
  (let* ((name (symbol-name sym))
         (expansion (symbol-value sym))
         (type (if (eq table global-abbrev-table) ?g ?l)))
    (propertize
     (format "%-20s %s"
             (propertize name 'face 'consult-key)
             (propertize (consult-abbrev--format-expansion expansion)
                         'face 'consult-buffer))
     'consult--candidate sym
     'consult--type type)))

(defun consult-abbrev--candidates (tables)
  "Build list of candidates from TABLES."
  (mapcan
   (lambda (table)
     (when-let* ((table-name (abbrev-table-name table)))
       (mapcar (lambda (sym) (consult-abbrev--make-candidate sym table))
               (abbrev--table-symbols table-name t))))
   tables))

;;;###autoload
(defun consult-abbrev ()
  "Browse active abbrevs with live preview."
  (interactive)
  (let* ((tables (abbrev--active-tables))
         (candidates (consult-abbrev--candidates tables)))
    (consult--read
     candidates
     :prompt "Abbrev: "
     :category 'abbrev
     :sort nil
     :require-match t
     :annotate
     (lambda (cand)
       (when-let ((sym (get-text-property 0 'consult--candidate cand)))
         (format " (%d×)" (abbrev-get sym :count))))
     :group
     (lambda (cand transform)
       (if transform cand
         (alist-get (get-text-property 0 'consult--type cand)
                    consult-abbrev--narrow)))
     :narrow
     (list :predicate
           (lambda (cand)
             (= (get-text-property 0 'consult--type cand)
                consult--narrow))
           :keys consult-abbrev--narrow)
     :state
     (lambda (action cand)
       (when (and (eq action 'preview) cand)
         (when-let ((sym (get-text-property 0 'consult--candidate cand)))
           (message "%s" (symbol-value sym)))))
     :history t)))

(provide 'consult-abbrev)
;;; consult-abbrev.el ends here
