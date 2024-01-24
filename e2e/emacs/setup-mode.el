(defun output-debug-to-console (format-string &rest args)
  "Output debug info to the console."
  (message (apply #'format format-string args)))

(advice-add 'org-newtab--log :before #'output-debug-to-console)

(org-newtab-mode)

(sleep-for 60)
