(let ((main-file (expand-file-name "e2e/emacs/agenda.org" base-dir)))
  (with-temp-file
      (expand-file-name (concat "e2e/emacs/change-tags-"
                                (int-to-string org-newtab-ws-port)
                                ".org")
                        base-dir)
    (exec-when-file-isnt-locked main-file
                                (lambda ()
                                  (insert-file-contents main-file)))
    (org-mode)
    (goto-char (point-min))
    (search-forward "Sample tagged item")
    (beginning-of-line)
    (org-set-tags "2#NEWTAG")))
