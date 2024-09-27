;; jh-mustache-csv.el --- Convert CSV to Text using Mustache templates -*- lexical-binding: t; -*-

;; Author: Julian Hoch (julianhoch@web.de)
;; Version: 1.0
;; Keywords: csv, mustache, text, conversion
;; Package-Requires: ((emacs "24.3") (pcsv "1.4.0") (mustache "0.23"))
;; URL: https://github.com/julian-hoch/emacs-utilities

;;; Commentary:

;; This package provides a function `jh/mustache-csv` that converts CSV data to text
;; using Mustache templates. The function reads a CSV file, parses it, and renders
;; the data using a Mustache template. The result is inserted into the buffer.

;; Usage:

;; - Select the region containing the CSV data.
;; - Call `jh/mustache-csv` interactively.
;; - Provide a Mustache template when prompted.
;; - The selected region is replaced with the rendered text.
;; - The function can also be called programmatically.

;;; Code:

(require 'pcsv)
(require 'mustache)

(defun jh/csv-headers-to-alist (headers row)
  "Map HEADERS to ROW values, returning an alist for mustache."
  (mapcar* #'cons headers row))

(defun jh/csv-to-mustache-data (parsed-csv)
  "Convert PARSED-CSV into a list of alists for mustache rendering."
  (let ((headers (car parsed-csv))  ; Extract headers
        (rows (cdr parsed-csv)))    ; Extract rows (data)
    (mapcar (lambda (row) (jh/csv-headers-to-alist headers row)) rows)))

(defun jh/render-csv-to-org (parsed-csv template)
  "Render PARSED-CSV into an Org-mode string using a Mustache template."
  (let ((mustache-data (jh/csv-to-mustache-data parsed-csv)))
    (mapconcat (lambda (data)
                 (mustache-render template data))
               mustache-data "\n")))

(defun jh/mustache-csv (start end &optional template)
  "Parse the region as CSV, transform it using a Mustache TEMPLATE, and replace the region with the result.
If TEMPLATE is not provided, prompt the user to input it."
  (interactive "r")
  (let* ((parsed-csv (pcsv-parse-region start end))
         (template (or template (read-string "Enter mustache template: ")))
         (rendered-content (jh/render-csv-to-org parsed-csv template)))
    (delete-region start end)
    (insert rendered-content)))

(provide 'jh-mustache-csv)

;; jh-mustache-csv.el ends here
