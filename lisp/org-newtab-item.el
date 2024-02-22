;;; org-newtab-item.el --- Toggle WebSocket server and hooks -*-lexical-binding:t-*-

;; Copyright (C) 2023-2024, Zweihänder <zweidev@zweihander.me>
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

;; This file deals with the 'business logic' around fetching an item based
;; on clocked status and given match query.

;;; Code:

(require 'org-newtab)
(require 'org-newtab-server)
(require 'org-newtab-agenda)
(require 'org-newtab-store)
(require 'async)

(defun org-newtab--send-tag-faces ()
  "Send the tag faces to the client."
  (let* ((tags (org-newtab--get-tag-faces))
         (data-packet (list :type "TAGS" :data tags)))
    (org-newtab--send-data (json-encode data-packet))))

(defun org-newtab--send-match (query &optional resid)
  "Send the current match for query QUERY to the client -- with RESID if provided."
  (org-newtab--dispatch 'find-match `(:resid ,resid))
  (let ((own-task (org-newtab--selected-async-priority-task)))
    (unless resid ; Not a response to extension, coming from hook/emacs side
      (org-newtab--send-data (json-encode `(:type "FINDING" :data ,own-task))))
    (async-start
     `(lambda ()
        ,(async-inject-variables "\\`load-path\\'")
        ,(async-inject-variables "\\`org-agenda-files\\'")
        ,(async-inject-variables "\\`org-todo-keywords\\'")
        (let ((inhibit-message t)) ; TODO: freezes if prompted for input -- test further
          (require 'org-newtab-agenda)
          (org-newtab--get-one-agenda-item ',query)))
     `(lambda (result)
        (let ((data-packet (list :type "ITEM" :data result)))
          (when ,resid
            (setq data-packet (plist-put data-packet :resid ,resid)))
          (if (equal ,own-task (org-newtab--selected-async-priority-task))
              (progn (org-newtab--send-data (json-encode data-packet))
                     (org-newtab--dispatch 'send-item))
            (org-newtab--log
             "[Item] %s" "Async task priority changed, older request dropped")))))))

(defun org-newtab--send-clkd-item (&optional resid)
  "Send the current clocked-in item to the client -- with RESID if provided."
  (org-newtab--dispatch 'send-item)
  (let* ((item (org-newtab--get-clocked-in-item))
         (data-packet (list :type "ITEM" :data item)))
    (when resid
      (setq data-packet (plist-put data-packet :resid resid)))
    (org-newtab--send-data (json-encode data-packet))))

(defun org-newtab--get-item (&optional payload)
  "Send an item to the extension based on :query and :resid in PAYLOAD.
If QUERY is nil, use `org-newtab--last-match-query'. If RESID is nil, ignore."
  (let ((query (or (plist-get payload :query)
                   (org-newtab--selected-last-match-query)))
        (resid (plist-get payload :resid)))
    (cond ((org-clocking-p)
           (org-newtab--send-clkd-item resid))
          (t
           (org-newtab--send-match query resid)))))

(defun org-newtab--save-and-get-item (&rest _)
  "Send new item to client using last recorded match query."
  (org-newtab--save-all-agenda-buffers)
  (org-newtab--get-item))

(provide 'org-newtab-item)

;; Local Variables:
;; coding: utf-8
;; End:

;;; org-newtab-item.el ends here
