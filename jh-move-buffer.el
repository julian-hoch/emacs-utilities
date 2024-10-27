(defun jh/move-buffer-to-other-window ()
  "Move the current buffer to the other window."
  (interactive)
  (if (null (next-window))
      (message "No other window.")
    (switch-to-buffer-other-window (current-buffer))
    (previous-buffer)))

(provide 'jh-window)
;;; jh-window.el ends here
