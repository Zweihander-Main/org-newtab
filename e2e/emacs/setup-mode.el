(defun output-debug-to-console (format-string &rest args)
  "Output debug info to the console."
  (message (apply #'format format-string args)))

(advice-add 'org-newtab--log :before #'output-debug-to-console)

(setq org-agenda-files (list (expand-file-name "e2e/emacs" base-dir)))
(setq org-todo-keywords '((sequence "TODO" "NEXT" "DONE")))

(org-newtab-mode)

(sleep-for 60)
