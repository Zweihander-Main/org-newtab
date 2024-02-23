(let ((buffer (generate-new-buffer "clock-cancel"))
      (main-file (expand-file-name "e2e/emacs/clock.org" base-dir)))
  (with-current-buffer buffer
    (exec-when-file-isnt-locked main-file
                                (lambda ()
                                  (insert-file-contents main-file)))
    (org-mode)
    (goto-char (point-min))
    (search-forward "Sample clocked item")
    (beginning-of-line)
    (org-clock-in)
    (sleep-for 5)
    (org-clock-cancel)))
