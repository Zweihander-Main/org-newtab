;;; org-newtab.el --- WIP -*-lexical-binding:t-*-

;; Copyright (C) 2023, Zweihänder <zweidev@zweihander.me>
;;
;; Author: Zweihänder <zweidev@zweihander.me>
;; Keywords: outlines
;; Homepage: https://github.com/Zweihander-Main/org-newtab
;; Version: 0.0.1
;; Package-Requires: ((emacs "27.1") (websocket "1.7") (async "1.9.4"))

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published
;; by the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.
;;
;; You should have received a copy of the GNU Affero General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; WIP
;;
;;; Code:

(require 'cl-lib)

(defgroup org-newtab nil
  "A browser new tab page linked to `org-agenda'."
  :group 'org-newtab
  :prefix "org-newtab-"
  :link `(url-link :tag "Github" "https://github.com/Zweihander-Main/org-newtab"))

(defcustom org-newtab-ws-port
  35942
  "Port to server websocket server on."
  :type 'integer
  :group 'org-newtab)

(defun org-newtab--log (format-string &rest args)
  "Log FORMAT-STRING and ARGS to `org-newtab-log-buffer'."
  (with-current-buffer (get-buffer-create "*org-newtab-log*")
    (goto-char (point-max))
    (insert (apply #'format format-string args))
    (insert "\n")))

(provide 'org-newtab)

(cl-eval-when (load eval)
  (let ((dir (expand-file-name default-directory)))
    (if (not (memq dir load-path))
        (add-to-list 'load-path dir))) ; TODO: Remove for packaging?
  (require 'org-newtab-mode))

;; Local Variables:
;; coding: utf-8
;; flycheck-enabled-checkers: (emacs-lisp-package emacs-lisp-checkdoc elisp-eldev)
;; flycheck-disabled-checkers: nil
;; End:

;;; org-newtab.el ends here
