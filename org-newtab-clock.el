;;; org-newtab-clock.el --- WIP -*-lexical-binding:t-*-

;; Copyright (C) 2023, Zweihänder <zweidev@zweihander.me>
;;
;; Author: Zweihänder
;; Keywords: outlines
;; Homepage: https://github.com/Zweihander-Main/org-newtab
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1") (websocket "1.7") (async "1.9.4"))

;; This file is not part of GNU Emacs.

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

(require 'org)
(require 'org-clock)

(defun org-newtab--get-clocked-in-item ()
  "Retrieve the currently clocked-in item in the background with temporary buffers."
  (when (org-clocking-p)
    (let* ((marker org-clock-hd-marker)
           (buffer (marker-buffer marker)))
      (with-current-buffer buffer
        (save-excursion
          (goto-char (marker-position marker))
          (org-newtab--process-clock-item))))))

(defun org-newtab--process-clock-item ()
  "Get an org clock marker and return a JSON string of its properties."
  (let* ((props (org-entry-properties))
         (json-null json-false))
    (setq props (append props
                        `(("CLOCKED_MINUTES" . ,(org-clock-get-clocked-time)))))
    (json-encode props)))

(provide 'org-newtab-clock)

;; Local Variables:
;; coding: utf-8
;; End:

;;; org-newtab-clock.el ends here
