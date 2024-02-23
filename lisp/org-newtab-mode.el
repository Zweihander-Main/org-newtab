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

(eval-when-compile
  (cl-pushnew (expand-file-name default-directory) load-path))

(require 'org-newtab-store)
(require 'org-newtab-server)
(require 'org-newtab-item)

(defun org-newtab--on-hook-clock-in (&rest _)
  "From `org-clock-in-hook', send new item to client."
  (org-newtab--dispatch 'hk-clk-in))

(defun org-newtab--on-hook-clock-out (&rest _)
  "From `org-clock-out-hook', send new item to client."
  (org-newtab--dispatch 'hk-clk-out))

(defun org-newtab--on-hook-clock-cancel (&rest _)
  "From `org-clock-cancel-hook', send new item to client."
  (org-newtab--dispatch 'hk-clk-cancel))

(defun org-newtab--on-hook-todo-change (&optional change-data)
  "From `org-trigger-hook', send new query if CHANGE-DATA changed (todo change)."
  (when change-data
    (let ((to (substring-no-properties (plist-get change-data :to)))
          (from (substring-no-properties (plist-get change-data :from))))
      (unless (string-match-p from to)
        (org-newtab--dispatch 'hk-todo-chg)))))

(defun org-newtab--on-hook-after-tags-change (&rest _)
  "From `org-after-tags-change-hook', send new item to client."
  (org-newtab--dispatch 'hk-tags-chg))

(defun org-newtab--on-hook-after-refile-insert (&rest _)
  "From `org-after-refile-insert-hook', send new item to client."
  (org-newtab--dispatch 'hk-refile))

(defun org-newtab--on-adv-edit-headline (&rest _)
  "From `org-edit-headline', send new item to client."
  (org-newtab--dispatch 'adv-edit-hl))

(defun org-newtab--on-adv-priority (&rest _)
  "From `org-priority', send new item to client."
  (org-newtab--dispatch 'adv-pri-chg))

(defun org-newtab--on-adv-set-effort (&rest _)
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
