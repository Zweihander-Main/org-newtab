;;; org-newtab-mode.el --- WIP -*-lexical-binding:t-*-

;; Copyright (C) 2023, Zweihänder <zweidev@zweihander.me>
;;
;; Author: Zweihänder
;; Keywords: outlines
;; Homepage: https://github.com/Zweihander-Main/org-newtab
;; Version: 0.0.1
;; Package-Requires: ((emacs "27.1") (websocket "1.7") (async "1.9.4"))

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

(require 'org-newtab-server)

(defun org-newtab--save-all-agenda-buffers ()
  "Save all Org agenda buffers without user confirmation.
Necessary to allow for async queries to use fresh data."
  (save-some-buffers t (lambda () (org-agenda-file-p))))

(defun org-newtab--send-new-match-query ()
  "Send new item to client using last recorded match query."
  (org--newtab--save-all-agenda-buffers)
  (cond ((org-clocking-p)
         (org-newtab--on-msg-send-clocked-in))
        (t
         (org-newtab--on-msg-send-match-query org-newtab--last-match-query))))

(defun org-newtab--items-modified (&optional change-data)
  "From `org-trigger-hook', check CHANGE-DATA state change, send new query if changed."
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
    (add-hook 'org-clock-in-hook #'org-newtab--on-msg-send-clocked-in)
    (add-hook 'org-clock-out-hook #'org-newtab--send-new-match-query)
    (add-hook 'org-clock-cancel-hook #'org-newtab--send-new-match-query)
    (add-hook 'org-trigger-hook #'org-newtab--items-modified)
    (add-hook 'org-after-tags-change-hook #'org-newtab--send-new-match-query)
    (add-hook 'org-after-refile-insert-hook #'org-newtab--send-new-match-query))
   (t
    (org-newtab--close-server)
    (remove-hook 'org-clock-in-hook #'org-newtab--on-msg-send-clocked-in)
    (remove-hook 'org-clock-out-hook #'org-newtab--send-new-match-query)
    (remove-hook 'org-clock-cancel-hook #'org-newtab--send-new-match-query)
    (remove-hook 'org-trigger-hook #'org-newtab--items-modified)
    (remove-hook 'org-after-tags-change-hook #'org-newtab--send-new-match-query)
    (remove-hook 'org-after-refile-insert-hook #'org-newtab--send-new-match-query))))

(provide 'org-newtab-mode)

;; Local Variables:
;; coding: utf-8
;; End:

;;; org-newtab-mode.el ends here
