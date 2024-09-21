;;; jh-decide.el --- Decision-making utility based on probabilities -*- lexical-binding: t; -*-

;; Author: Julian Hoch (julianhoch@web.de)
;; Version: 1.0
;; Keywords: convenience, decision-making
;; Package-Requires: ((emacs "24.3"))
;; URL: TODO

;;; Commentary:

;; This package provides a function `jh/decide-with-probabilities` that allows you to
;; input options with specified probabilities and randomly selects one based on those
;; probabilities. It supports options with explicit percentages and a 'Rest' option
;; that takes up the remaining percentage.

;; Usage:

;; - Call `jh/decide-with-probabilities` interactively.
;; - Provide the options and percentages when prompted, e.g., "Option A/30, Option B/50, Option C".
;; - Alternatively, select a region of text containing the options and percentages and call the function.
;; - The function can also be called programmatically with an input string.

;;; Code:

(defun jh/decide-with-probabilities (&optional input)
  "Select an option based on specified probabilities.

If INPUT is provided, use it as the input string.
If there is an active region, use the selected text as the input string.
Otherwise, prompt via the minibuffer.

Input format:
  Option/Percentage, Option2/Percentage2, Option3
Options without a specified Percentage will be assigned the remaining percentage."
  (interactive)
  (let* ((input-str (jh--get-input-string input))
         (options (jh--parse-options input-str))
         (options (jh--validate-and-handle-rest options))
         (cumulative-options (jh--build-cumulative-ranges options))
         (selected-option (jh--select-option cumulative-options)))
    (if selected-option
        (message "You should choose: %s" selected-option)
      (message "No option found for the generated random number."))))

(defun jh--get-input-string (input)
  "Determine the input string to use for processing.

- If INPUT is non-nil, return INPUT.
- If there is an active region, return the selected text.
- Otherwise, prompt the user via the minibuffer."
  (cond
   (input
    (substring-no-properties input))
   ((use-region-p)
    (buffer-substring-no-properties (region-beginning) (region-end)))
   (t
    (read-string
     "Enter options with percentages (e.g., Option A/30, Option B/50, Option C): "))))

(defun jh--parse-options (input-str)
  "Parse INPUT-STR into a list of options with their percentages.

Returns a list of (OPTION PERCENTAGE) pairs.
PERCENTAGE is either a number or the symbol 'Rest' for options without specified percentages."
  (let ((items (split-string input-str ", *"))
        (options '())
        (rest-option nil))
    (dolist (item items)
      (let* ((parts (split-string item "/"))
             (option (string-trim (nth 0 parts)))
             (percent-str (nth 1 parts))
             (percentage nil))
        (if percent-str
            (if (string-match "\\`[0-9]+\\'" percent-str)
                (setq percentage (string-to-number percent-str))
              (error "Invalid percentage format in '%s'" item))
          ;; No percentage specified, treat as 'Rest' option
          (if rest-option
              (error "Multiple options without specified percentages: '%s' and '%s'"
                     rest-option option)
            (setq percentage 'Rest
                  rest-option option)))
        (push (list option percentage) options)))
    (reverse options)))  ; Return options in the original order

(defun jh--validate-and-handle-rest (options)
  "Validate percentages in OPTIONS and handle the 'Rest' option.

Ensures that the total percentages sum to 100%.
Assigns the remaining percentage to the 'Rest' option, if any."
  (let ((total-percentage 0)
        (rest-option nil))
    ;; Calculate total percentage and identify the 'Rest' option
    (dolist (opt options)
      (let ((percentage (nth 1 opt)))
        (if (eq percentage 'Rest)
            (setq rest-option opt)
          (setq total-percentage (+ total-percentage percentage)))))
    ;; Handle 'Rest' option
    (if rest-option
        (let ((rest-percentage (- 100 total-percentage)))
          (if (< rest-percentage 0)
              (error "Total specified percentages exceed 100%%")
            (setf (nth 1 rest-option) rest-percentage)
            (setq total-percentage (+ total-percentage rest-percentage))))
      ;; No 'Rest' option; percentages must sum to 100%
      (if (/= total-percentage 100)
          (error "Total percentages do not add up to 100%% and no 'Rest' option specified")))
    options))

(defun jh--build-cumulative-ranges (options)
  "Build cumulative percentage ranges for OPTIONS.

Each option becomes a list: (OPTION START END), where START and END define the percentage range."
  (let ((cumulative-percentage 0)
        (cumulative-options '()))
    (dolist (opt options)
      (let* ((option (nth 0 opt))
             (percentage (nth 1 opt))
             (start cumulative-percentage)
             (end nil))
        (setq cumulative-percentage (+ cumulative-percentage percentage))
        (setq end (1- cumulative-percentage))
        (push (list option start end) cumulative-options)))
    (reverse cumulative-options)))  ; Return in original order

(defun jh--select-option (cumulative-options)
  "Select an option from CUMULATIVE-OPTIONS based on a random number.

Uses `jh/generate-random-number` to obtain a random number between 0 and 99.
Returns the selected option's name."
  (let* ((random-number-str (jh/generate-random-number))
         (random-number (string-to-number random-number-str)))
    (catch 'found
      (dolist (opt cumulative-options)
        (let ((option (nth 0 opt))
              (start (nth 1 opt))
              (end (nth 2 opt)))
          (when (and (>= random-number start) (<= random-number end))
            (throw 'found option))))
      nil)))  ; Return nil if no option is found

(defun jh/generate-random-number ()
  "Generates a random number between 0 and 99 as string."
  (interactive)
  (let ((num (string-trim (shell-command-to-string "shuf -i 0-99 -n 1 --random-source=/dev/random"))))
    (message num)))

(provide 'jh-decide)

;;; jh-decide.el ends here
