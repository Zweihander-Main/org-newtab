;;; org-newtab-mode.el --- WIP -*-lexical-binding:t-*-

;; Copyright (C) 2023, Zweihänder <zweidev@zweihander.me>
;;
;; Author: Zweihänder <zweidev@zweihander.me>

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

(eval-when-compile
  (cl-pushnew (expand-file-name default-directory) load-path))

(require 'org-newtab-server)

(defun org-newtab--save-all-agenda-buffers ()
  "Save all Org agenda buffers without user confirmation.
Necessary to allow for async queries to use fresh data."
  (save-some-buffers t (lambda () (org-agenda-file-p))))

(defun org-newtab--send-new-match-query ()
  "Send new item to client using last recorded match query."
  (org-newtab--save-all-agenda-buffers)
  (cond ((org-clocking-p)
         (org-newtab--on-msg-send-clocked-in))
        (t
         (org-newtab--on-msg-send-match-query org-newtab--last-match-query))))

(defun org-newtab--items-modified (&optional change-data)
  "From `org-trigger-hook', send new query if CHANGE-DATA changed."
  (when change-data
    (let ((to (substring-no-properties (plist-get change-data :to)))
          (from (substring-no-properties (plist-get change-data :from))))
      (unless (string-match-p from to)
        (org-newtab--send-new-match-query)))))

;; TODO: rename items-modified
;; TODO: DRY send-new-match-query
;; TODO: Append hook to edit-headline function
;; TODO: Append hook to priority shift functions
;; TODO: Append hook to edit effort functions
;; TODO: Let client know async function is running (send resid)

(defvar org-newtab--hook-assocs
  '((org-clock-in-hook . org-newtab--on-msg-send-clocked-in)
    (org-clock-out-hook . org-newtab--send-new-match-query)
    (org-clock-cancel-hook . org-newtab--send-new-match-query)
    (org-trigger-hook . org-newtab--items-modified)
    (org-after-tags-change-hook . org-newtab--send-new-match-query)
    (org-after-refile-insert-hook . org-newtab--send-new-match-query))
  "Association list of hooks and functions to append to them.")

;;;###autoload
(define-minor-mode
  org-newtab-mode
  "Enable `org-newtab'.
Start the websocket server and add hooks in."
  :lighter " org-newtab"
  :global t
  :group 'org-newtab
  :init-value nil
  (cond
   (org-newtab-mode
    (org-newtab--start-server)
    (dolist (assoc org-newtab--hook-assocs)
      (add-hook (car assoc) (cdr assoc))))
   (t
    (org-newtab--close-server)
    (dolist (assoc org-newtab--hook-assocs)
      (remove-hook (car assoc) (cdr assoc))))))

(provide 'org-newtab-mode)

;; Local Variables:
;; coding: utf-8
;; End:

;;; org-newtab-mode.el ends here
