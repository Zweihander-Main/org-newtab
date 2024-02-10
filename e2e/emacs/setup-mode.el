(defun output-debug-to-console (format-string &rest args)
  "Output debug info to the console."
  (message (apply #'format format-string args)))

(advice-add 'org-newtab--log :before #'output-debug-to-console)

(setq org-agenda-files (list (expand-file-name "e2e/emacs" base-dir)))
(setq org-todo-keywords '((sequence "TODO" "NEXT" "DONE")))
(setq org-tag-faces '(("1#SAMPLETAG" . (:foreground "#42A5F5" :weight bold))
                      ("2#OTHERTAG" . (:foreground "#6A3B9F" :weight bold))
                      ("3#PARENTTAG" . (:foreground "#00FF33" :weight bold))))
(setq org-clock-persist nil)

(org-newtab-mode)

;; Mechanism for reading further code from a file
(setq org-newtab--extra-testing-code-file
      (expand-file-name (concat "e2e/emacs/extra-testing-code-"
                                (int-to-string org-newtab-ws-port)
                                ".el")
                        base-dir))

(let ((start-time (current-time))
      (file-read nil))
  (while (< (time-to-seconds (time-since start-time)) 60)
    (when (and (file-exists-p org-newtab--extra-testing-code-file)
               (not file-read))
      (load org-newtab--extra-testing-code-file)
      (setq file-read t))
    (sit-for 1)))
