(find-file (expand-file-name "e2e/emacs/agenda.org" base-dir))
(goto-char (point-min))
(search-forward "Sample clocked item")
(beginning-of-line)
(org-clock-in)
;; Shouldn't modify logbook as long as op is under a minute
