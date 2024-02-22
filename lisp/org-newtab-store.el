;;; org-newtab-store.el --- Keep track of app state -*-lexical-binding:t-*-

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

;; This file deals with the application state. It can be thought of as a simpler
;; Lisp version of the Flux (Redux) architecture used on the client side
;; extension.

;;; Code:

(require 'org-newtab)

(defvar org-newtab--state
  '(:last-match-query nil ;str -- The last match query received from the ext.
    :async-priority-task nil) ;int -- The async task which currently has priority.
  "The application state.")

(defvar org-newtab--action-subscribers
  '(())
  "List of actions with corresponding list of subscribers.")

(defun org-newtab--reducer (state type &optional payload)
  "Take STATE, apply action TYPE with opt PAYLOAD to state, return state.
Avoid side effects and mutations."
  (pcase type
    ;; Always capture the match query in case it's needed later
    ;; (for example, being able to send back data after clock out without
    ;; having to ask the extension for the query again)
    ('ext-get-item (plist-put state :last-match-query (plist-get payload :query)))
    ;; Set a new priority task that will take precedence over any previous
    ;; async queries. Use resid if provided, otherwise a random num.
    ('find-match   (plist-put state :async-priority-task
                              (or (plist-get payload :resid) (random t))))
    ;; Async task completed and task has priority so it can be cleared
    ;; Alternatively, clocked in item has been sent and should clear all other
    ;; async tasks
    ('send-item    (plist-put state :async-priority-task nil))
    ;; Don't save priority tasks between websocket connections
    ('ext-close    (plist-put state :async-priority-task nil))
    (_ state)))

(defun org-newtab--dispatch (type &optional payload)
  "Run state reducer and subscriptions on action TYPE with optional PAYLOAD."
  (org-newtab--log "[Store] Action dispatched: %s >> %s" type payload)
  (setq org-newtab--state
        (org-newtab--reducer org-newtab--state type payload))
  (let ((subs (alist-get type org-newtab--action-subscribers)))
    (when subs
      (dolist (func subs)
        (if payload (funcall func payload) (funcall func))))))

(defun org-newtab--clear-subscribers ()
  "Clear all subscribers."
  (setq org-newtab--action-subscribers '(())))

(defun org-newtab--subscribe (type func)
  "Subscribe FUNC to the action TYPE."
  (let ((action-subs (assoc type org-newtab--action-subscribers)))
    (if action-subs
        (unless (member func (cdr action-subs))
          (setcdr action-subs (cons func (cdr action-subs))))
      (setq org-newtab--action-subscribers
            (cons (cons type (list func)) org-newtab--action-subscribers)))))

(defun org-newtab--unsubscribe (type func)
  "Unsubscribe FUNC from the action TYPE."
  (let ((action-subs (assoc type org-newtab--action-subscribers)))
    (when action-subs
      (setcdr action-subs (remove func (cdr action-subs)))
      (when (null (cdr action-subs))
        (setq org-newtab--action-subscribers
              (delq action-subs org-newtab--action-subscribers))))))

(defun org-newtab--selected-last-match-query ()
  "Return the last match query."
  (plist-get org-newtab--state :last-match-query))

(defun org-newtab--selected-async-priority-task ()
  "Return async task which currently has priority."
  (plist-get org-newtab--state :async-priority-task))

(provide 'org-newtab-store)

;; Local Variables:
;; coding: utf-8
;; End:

;;; org-newtab-store.el ends here
