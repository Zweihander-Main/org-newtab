;;; org-newtab.el --- WIP -*-lexical-binding:t-*-

;; Copyright (C) 2023, Zweihänder <zweidev@zweihander.me>
;;
;; Author: Zweihänder
;; Keywords: outlines
;; Homepage: https://github.com/Zweihander-Main/org-newtab
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.1") (websocket "1.7"))

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
(require 'websocket)

(setq websocket-debug t)

(defvar org-newtab-ws-socket nil
  "The websocket for `org-newtab'.")

(defvar org-newtab-ws-server nil
  "The websocket server for `org-newtab'.")

(defcustom org-newtab-ws-port
  35942
  "Port to server websocket server on."
  :type 'integer
  :group 'org-newtab)

(defcustom org-newtab-agenda-filter "SCHEDULED>=\"<2023-06-01>\""
  "Filter to generate a list of agenda entries to show in the calendar."
  :type 'string
  :group 'org-newtab)

;;;###autoload
(define-minor-mode
  org-newtab-mode
  "Enable `org-newtab'.
This serves the web-build and API over HTTP."
  :lighter " org-newtab"
  :global t
  :group 'org-newtab
  :init-value nil
  (cond
   (org-newtab-mode
    (setq org-newtab-ws-server
          (websocket-server
           org-newtab-ws-port
           :host 'local
           :on-open #'org-newtab--ws-on-open
           :on-message #'org-newtab--ws-on-message
           :on-close #'org-newtab--ws-on-close
           :on-error #'org-newtab--ws-on-error)))
   (t
    (websocket-server-close org-newtab-ws-server))))

(defun org-newtab--ws-on-open (ws)
  "Open the websocket WS and send initial data."
  (setq org-newtab-ws-socket ws)
  (message "[Server] on-open"))

(defun org-newtab--ws-on-message (ws frame)
  "Take WS and FRAME as arguments when message received."
  (message "[Server] on-message")
  (message "[Server] Received %S from client" (websocket-frame-text frame))
  (message "[Server] Sending %S to client" (upcase (websocket-frame-text frame)))
  (websocket-send-text ws (upcase (websocket-frame-text frame))))


(defun org-newtab--ws-on-close (ws)
  "Perform when WS is closed."
  (setq org-newtab-ws-socket nil)
  (message "[Server] on-close"))

(defun org-newtab--ws-on-error (ws type error)
  "Handle ERROR of TYPE from WS."
  (concat "[Server] Error: " (prin1 type) ": " (prin1 error)))

(defun org-newtab--send-text (text)
  "Send TEXT to socket."
  (websocket-send-text org-newtab-ws-socket text))

(defun org-newtab--get-agenda ()
  "Get an org agenda event and transform it into a form that is easily JSONable."
  (let* ((props (org-entry-properties))
         (json-null json-false))
    props))

(defun org-newtab--get-calendar-entries (scope)
  "Get all agenda entries using our filter and `org-mode' SCOPE.
Return a structure that is JSONable."
  (org-map-entries #'org-newtab--get-agenda org-newtab-agenda-filter scope))

(defun org-newtab--encode-agenda ()
  "Encode our agenda to JSON."
  ;; want json-encode-array here in case we get an empty list. then we want "[]"
  (json-encode-array (org-newtab--get-calendar-entries 'agenda)))

(defun org-newtab--send-agenda ()
  "Get the agenda and send it through to the client."
  (let* ((encoded-agenda (org-newtab--encode-agenda)))
    (org-newtab--send-text encoded-agenda)))

(provide 'org-newtab)

;; Local Variables:
;; coding: utf-8
;; flycheck-disabled-checkers: 'emacs-lisp-elsa
;; End:

;;; org-newtab.el ends here
