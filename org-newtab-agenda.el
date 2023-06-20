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
;; WIP
;;
;;; Code:

(require 'org)
(require 'json)

;; (defun org-newtab--determine-action-from-message (recv-json)
;;   "Determine what action to take from RECV-JSON."
;;   (pcase (plist-get recv-json :action)
;;     ("changeFilter" (org-newtab--action-change-filter
;; 		     (plist-get recv-json :data)))
;;     (_ (message "[Server] Unknown action"))))

(defun org-newtab--process-agenda-item ()
  "Get an org agenda event and transform it into a form that is easily JSONable."
  (let* ((props (org-entry-properties))
         (json-null json-false))
    props))

(defun org-newtab--get-one-agenda-item (filter)
  "Return first item from agenda using FILTER."
  (let* ((entries (org-map-entries #'org-newtab--process-agenda-item
				   filter 'agenda))
	 (first-entry (car entries))
	 (json-entry (json-encode first-entry)))
    json-entry))

(provide 'org-newtab-agenda)

;; Local Variables:
;; coding: utf-8
;; End:

;;; org-newtab-agenda.el ends here