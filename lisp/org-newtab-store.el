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

(defun org-newtab--reducer (state action)
  "Take STATE, apply ACTION to state based on :type, return state.
Avoid side effects and mutations."
  (let ((type (plist-get action :type))
        (payload (plist-get action :payload)))
    (setq org-newtab--state
          (pcase type
            ;; Always capture the match query in case it's needed later
            ;; (for example, being able to send back data after clock out without
            ;; having to ask the extension for the query again)
            ('ext-get-item (plist-put state :last-match-query (plist-get payload :query)))
            (_ state)))))

(defun org-newtab--dispatch (action)
  "Dispatch ACTION to the reducer and set the new state. Then alert listeners."
  (org-newtab--log "[Store] Action dispatched: %s" action)
  (setq org-newtab--state
        (org-newtab--reducer org-newtab--state action))
  (let ((subs (alist-get (plist-get action :type)
                         org-newtab--action-subscribers)))
    (when subs
      (dolist (func subs)
        (funcall func (plist-get action :payload))))))

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

(provide 'org-newtab-store)

;; Local Variables:
;; coding: utf-8
;; End:

;;; org-newtab-store.el ends here
