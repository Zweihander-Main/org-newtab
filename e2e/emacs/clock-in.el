(let ((buffer (generate-new-buffer "clock-in")))
  (with-current-buffer buffer
    (insert-file-contents (expand-file-name "e2e/emacs/clock.org" base-dir))
    (org-mode)
    (goto-char (point-min))
    (search-forward "Sample clocked item")
    (beginning-of-line)
    (org-clock-in)))
