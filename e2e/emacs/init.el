(setq base-dir default-directory)
(setq default-directory (expand-file-name "lisp" base-dir))
(add-to-list 'load-path default-directory)

(setq exec-recurse-count 0)
(defun exec-when-file-isnt-locked (file fn)
  "Executes the function FN when the file FILE is not locked."
  (setq exec-recurse-count (1+ exec-recurse-count))
  (if (> exec-recurse-count 100)
      (error "File %s is locked or inaccessible" file)
    (if (and (file-exists-p file) (file-readable-p file) (not (file-locked-p file)))
        (condition-case nil
            (funcall fn)
          (error (exec-when-file-isnt-locked file fn)))
      ;; Recurse this function until the file is unlocked
      (exec-when-file-isnt-locked file fn))))

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
