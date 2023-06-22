;;; org-newtab-agenda.el --- WIP -*-lexical-binding:t-*-

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
;; This file contains the code for interacting with `org-agenda'. It's
;; deliberately separated from the main file for usage in async processes.
;; Because of this, file should be kept fairly dependency-free.
;;
;; Assumes`org-agenda-files' has been set.
;;
;;; Code:

(require 'org)
(require 'org-clock)
(require 'json)

(defun org-newtab--get-one-agenda-item (filter)
  "Return first item from agenda using FILTER."
  (let* ((entries (org-map-entries #'org-newtab--process-org-marker
                                   filter 'agenda))
         (first-entry (car entries))
         (json-entry (json-encode first-entry)))
    json-entry))

(defun org-newtab--get-clocked-in-item ()
  "Retrieve the currently clocked-in item in the background with temporary buffers."
  (let* ((marker org-clock-hd-marker)
         (buffer (marker-buffer marker)))
    (with-current-buffer buffer
      (save-excursion
        (goto-char (marker-position marker))
        (json-encode (org-newtab--process-org-marker t))))))

(defun org-newtab--process-org-marker (&optional clocked)
  "Get an org marker and return an JSONable form of its properties.
Add CLOKED minutes if CLOCKED is non-nil."
  (let* ((props (org-entry-properties))
         (json-null json-false))
    (message "props: %s" props)
    (when clocked
      (setq props
            (append props
                    `(("CLOCKED_MINUTES" . ,(org-clock-get-clocked-time))))))
    props))

(provide 'org-newtab-agenda)

;; Local Variables:
;; coding: utf-8
;; End:

;;; org-newtab-agenda.el ends here
