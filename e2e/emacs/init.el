(setq base-dir default-directory)
(setq default-directory (expand-file-name "lisp" base-dir))
(setq org-agenda-files (list (expand-file-name "e2e/emacs" base-dir)))
(add-to-list 'load-path default-directory)

(require 'package)
(package-initialize)
(unless package-archive-contents                            ; unless packages are not available locally, dont refresh package archives
  (package-refresh-contents))
(package-install-file (expand-file-name "org-newtab.el" default-directory))

(load "org-newtab.el")
