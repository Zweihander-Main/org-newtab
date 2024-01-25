(setq base-dir default-directory)
(setq default-directory (expand-file-name "lisp" base-dir))
(add-to-list 'load-path default-directory)

(defun exec-when-file-isnt-locked (file fn)
  (if (and (file-exists-p file) (file-readable-p file) (not (file-locked-p file)))
      (condition-case nil
          (progn
            (funcall fn))
        (error (exec-when-file-isnt-locked file fn)))
    ;; Recurse this function until the file is unlocked
    (run-with-idle-timer 0.1 nil
                         (lambda (file fn)
                           (exec-when-file-isnt-locked file fn))
                         file fn)))

(require 'package)
(package-initialize)
(unless package-archive-contents                            ; unless packages are not available locally, dont refresh package archives
  (package-refresh-contents))
(let ((main-file (expand-file-name "org-newtab.el" default-directory)))
  (exec-when-file-isnt-locked main-file
                              (lambda ()
                                (package-install-file main-file)))
  (exec-when-file-isnt-locked main-file
                              (lambda ()
                                (load main-file))))
