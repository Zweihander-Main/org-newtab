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

(require 'cl-macs)
(require 'org)
(require 'org-element)
(require 'org-duration)
(require 'org-clock)
(require 'color)
(require 'json)

(defun org-newtab--get-one-agenda-item (filter)
  "Return first item from agenda using FILTER in JSONable form."
  (let* ((entries (org-map-entries #'org-newtab--process-org-marker
                                   filter 'agenda))
         (first-entry (car entries)))
    first-entry))

(defun org-newtab--get-clocked-in-item ()
  "Retrieve the currently clocked-in item in JSONable form."
  (let* ((marker org-clock-hd-marker)
         (buffer (marker-buffer marker)))
    (with-current-buffer buffer
      (save-excursion
        (goto-char (marker-position marker))
        (org-newtab--process-org-marker t)))))

(defun org-newtab--process-org-marker (&optional clocked)
  "Get an org marker and return a JSONable form of its properties.
Add CLOKED minutes if CLOCKED is non-nil."
  (let ((props (org-entry-properties))
        (json-null json-false))
    (when clocked
      (setq props
            (append props
                    `(("EFFORT_MINUTES" .
                       ,(org-duration-to-minutes
                         (org-entry-get (point) org-effort-property)))
                      ("PREVIOUSLY_CLOCKED_MINUTES" .
                       ,(or org-clock-total-time 0))
                      ("CURRENT_CLOCK_START_TIMESTAMP" .
                       ,(* (time-convert org-clock-start-time 'integer) 1000))))))
    props))

(defun org-newtab--string-color-to-hex (string)
  "Convert STRING to hex value if it's a color."
  (cond ((not string) nil)
        ((string-prefix-p "#" string) string)
        (t (cl-destructuring-bind (red green blue)
               (color-name-to-rgb string)
             (color-rgb-to-hex red green blue 2)))))

(defun org-newtab--get-tag-faces ()
  "Retrieve `org-tag-faces' variable in JSON.

Note that this function will not work in a terminal/async context as converting
from color names to hex will use the terminal color codes eg goldenrod=yellow1."
  (let ((json-null json-false))
    (mapcar
     (lambda (tag-cons)
       (let ((tag (car tag-cons))
             (foreground-data (cdr tag-cons)))
         (cond ((stringp foreground-data)
                (cons tag (org-newtab--string-color-to-hex
                           foreground-data)))
               ((facep foreground-data)
                (cons tag (org-newtab--string-color-to-hex
                           (face-foreground foreground-data))))
               ((listp foreground-data)
                (cons tag (org-newtab--string-color-to-hex
                           (plist-get foreground-data :foreground)))))))
     org-tag-faces)))

(provide 'org-newtab-agenda)

;; Local Variables:
;; coding: utf-8
;; End:

;;; org-newtab-agenda.el ends here
