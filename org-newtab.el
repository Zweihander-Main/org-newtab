;;; org-newtab.el --- WIP -*-lexical-binding:t-*-

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

(require 'cl-lib)
(require 'org)
(require 'websocket)

(defgroup org-newtab nil
  "A browser new tab page linked to `org-agenda'."
  :group 'org-newtab
  :prefix "org-newtab-"
  :link `(url-link :tag "Github" "https://github.com/Zweihander-Main/org-newtab"))

(defvar org-newtab--ws-socket nil
  "The websocket for `org-newtab'.")

(defvar org-newtab--ws-server nil
  "The websocket server for `org-newtab'.")

(defcustom org-newtab-ws-port
  35942
  "Port to server websocket server on."
  :type 'integer
  :group 'org-newtab)

(defcustom org-newtab-agenda-filter "TODO=\"TODO\""
  "Default filter for agenda entries. Takes the form of agenda match query."
  :type 'string
  :group 'org-newtab)

(defun org-newtab--determine-action-from-message (recv-json)
  "Determine what action to take from RECV-JSON."
  (pcase (plist-get recv-json :action)
    ("changeFilter" (org-newtab--action-change-filter
		     (plist-get recv-json :data)))
    (_ (message "[Server] Unknown action"))))

(defun org-newtab--action-change-filter (filter)
  "Change the filter to FILTER and send the agenda."
  (setq org-newtab-agenda-filter filter)
  (org-newtab--get-one-agenda-item))

(defun org-newtab--process-agenda-item ()
  "Get an org agenda event and transform it into a form that is easily JSONable."
  (let* ((props (org-entry-properties))
         (json-null json-false))
    props))

(defun org-newtab--get-one-agenda-item ()
  "Return first item from agenda using the current `org-newtab-agenda-filter'."
  (let* ((entries (org-map-entries #'org-newtab--process-agenda-item
				   org-newtab-agenda-filter 'agenda))
	 (first-entry (car entries))
	 (json-entry (json-encode first-entry)))
    json-entry))

(provide 'org-newtab)

(cl-eval-when (load eval)
  (require 'org-newtab-server))

;; Local Variables:
;; coding: utf-8
;; End:

;;; org-newtab.el ends here
