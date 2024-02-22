;;; org-newtab-mode.el --- Toggle WebSocket server and hooks -*-lexical-binding:t-*-

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

;; This file provides a minor mode to toggle the WebSocket server and org hooks.
;; Hook related code primarily goes here.

;;; Code:

;; TODO: ping the client on async to let it know data is coming
;; TODO: Let client know async function is running (send resid)
;; TODO: on clock out, let client know clock out occured if async needed

(eval-when-compile
  (cl-pushnew (expand-file-name default-directory) load-path))

(require 'org-newtab-server)
(require 'org-newtab-store)
(require 'org-newtab-agenda)
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
             "[Server] %s" "Async task priority changed, older request dropped")))))))

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

(defun org-newtab--save-all-agenda-buffers ()
  "Save all Org agenda buffers without user confirmation.
Necessary to allow for async queries to use fresh data."
  (save-some-buffers t (lambda () (org-agenda-file-p))))

(defun org-newtab--save-and-get-item (&rest _)
  "Send new item to client using last recorded match query."
  (org-newtab--save-all-agenda-buffers)
  (org-newtab--get-item))

(defun org-newtab--on-hook-clock-in (&optional _)
  "From `org-clock-in-hook', send new item to client."
  (org-newtab--dispatch 'hk-clk-in))

(defun org-newtab--on-hook-clock-out (&optional _)
  "From `org-clock-out-hook', send new item to client."
  (org-newtab--dispatch 'hk-clk-out))

(defun org-newtab--on-hook-clock-cancel (&optional _)
  "From `org-clock-cancel-hook', send new item to client."
  (org-newtab--dispatch 'hk-clk-cancel))

(defun org-newtab--on-hook-todo-change (&optional change-data)
  "From `org-trigger-hook', send new query if CHANGE-DATA changed (todo change)."
  (when change-data
    (let ((to (substring-no-properties (plist-get change-data :to)))
          (from (substring-no-properties (plist-get change-data :from))))
      (unless (string-match-p from to)
        (org-newtab--dispatch 'hk-todo-chg)))))

(defun org-newtab--on-hook-after-tags-change (&optional _)
  "From `org-after-tags-change-hook', send new item to client."
  (org-newtab--dispatch 'hk-tags-chg))

(defun org-newtab--on-hook-after-refile-insert (&optional _)
  "From `org-after-refile-insert-hook', send new item to client."
  (org-newtab--dispatch 'hk-refile))

(defun org-newtab--on-adv-edit-headline (&optional _)
  "From `org-edit-headline', send new item to client."
  (org-newtab--dispatch 'adv-edit-hl))

(defun org-newtab--on-adv-priority (&optional _)
  "From `org-priority', send new item to client."
  (org-newtab--dispatch 'adv-pri-chg))

(defun org-newtab--on-adv-set-effort (&optional _)
  "From `org-set-effort', send new item to client."
  (org-newtab--dispatch 'adv-effort-chg))

(defconst org-newtab--hook-assocs
  '((org-clock-in-hook . org-newtab--on-hook-clock-in)
    (org-clock-out-hook . org-newtab--on-hook-clock-out)
    (org-clock-cancel-hook . org-newtab--on-hook-clock-cancel)
    (org-trigger-hook . org-newtab--on-hook-todo-change)
    (org-after-tags-change-hook . org-newtab--on-hook-after-tags-change)
    (org-after-refile-insert-hook . org-newtab--on-hook-after-refile-insert))
  "Association list of hooks and functions to append to them.")

;; TODO: can determine if the client todo is the headline being edited
;; - Note that using the match query method, it should never change the item
;; sent as you can't match on headline
(defconst org-newtab--advice-assocs
  '((org-edit-headline . org-newtab--on-adv-edit-headline)
    (org-priority . org-newtab--on-adv-priority)
    (org-set-effort . org-newtab--on-adv-set-effort))
  "Association list of functions and advice to append to them.")

(defconst org-newtab--sub-assocs
  '((ext-get-item . org-newtab--get-item)
    (ext-open . org-newtab--send-tag-faces)
    (hk-clk-in . org-newtab--send-clkd-item)
    (hk-clk-out . org-newtab--save-and-get-item)
    (hk-clk-cancel . org-newtab--save-and-get-item)
    (hk-todo-chg . org-newtab--save-and-get-item)
    (hk-tags-chg . org-newtab--save-and-get-item)
    (hk-refile . org-newtab--save-and-get-item)
    (adv-edit-hl . org-newtab--save-and-get-item)
    (adv-pri-chg . org-newtab--save-and-get-item)
    (adv-effort-chg . org-newtab--save-and-get-item))
  "Association list of action types and subscriber functions to them.")

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
    (dolist (assoc org-newtab--sub-assocs)
      (org-newtab--subscribe (car assoc) (cdr assoc)))
    (dolist (assoc org-newtab--hook-assocs)
      (add-hook (car assoc) (cdr assoc)))
    (dolist (assoc org-newtab--advice-assocs)
      (advice-add (car assoc) :after (cdr assoc))))
   (t
    (org-newtab--close-server)
    (org-newtab--clear-subscribers)
    (dolist (assoc org-newtab--hook-assocs)
      (remove-hook (car assoc) (cdr assoc)))
    (dolist (assoc org-newtab--advice-assocs)
      (advice-remove (car assoc) (cdr assoc))))))

(provide 'org-newtab-mode)

;; Local Variables:
;; coding: utf-8
;; End:

;;; org-newtab-mode.el ends here
