;;; org-newtab-server.el --- WIP -*-lexical-binding:t-*-

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

(require 'org-newtab)
(require 'async)
(require 'websocket) ;; TODO into other file?

(defvar org-newtab--ws-socket nil
  "The websocket for `org-newtab'.")

(defvar org-newtab--ws-server nil
  "The websocket server for `org-newtab'.")

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
    (setq org-newtab--ws-server
          (websocket-server
           org-newtab-ws-port
           :host 'local
           :on-open #'org-newtab--ws-on-open
           :on-message #'org-newtab--ws-on-message
           :on-close #'org-newtab--ws-on-close
           :on-error #'org-newtab--ws-on-error)))
   (t
    (websocket-server-close org-newtab--ws-server))))

(defun org-newtab--ws-on-open (ws)
  "Open the websocket WS and send initial data."
  (setq org-newtab--ws-socket ws)
  (message "[Server] on-open"))

(defun org-newtab--ws-on-message (_ws frame)
  "Take WS and FRAME as arguments when message received."
  (message "[Server] on-message")
  (let* ((frame-text (websocket-frame-text frame))
	 (json-data (org-newtab--decipher-message-from-frame-text frame-text))
         (agenda-filter (plist-get json-data :data)))
    (message "[Server] Received %S from client" json-data)
    (async-start
     `(lambda ()
        ,(async-inject-variables "\\`\\(org-agenda-files\\)\\'") ;; TODO: if it becomes interactive, it freezes
	(load-file ,(concat (file-name-directory (or load-file-name buffer-file-name)) "org-newtab-agenda.el"))
		  (require 'org-newtab-agenda)
	(org-newtab--get-one-agenda-item ,agenda-filter))
     (lambda (result)
       (message "[Server] Sending %S to client" result)
       (org-newtab--send-data result))))) ;; TODO cannot send message to closed websocket
;; TODO Sending null to client

(defun org-newtab--ws-on-close (_ws)
  "Perform when WS is closed."
  (setq org-newtab--ws-socket nil)
  (message "[Server] on-close"))

(defun org-newtab--ws-on-error (_ws type error)
  "Handle ERROR of TYPE from WS."
  (concat "[Server] Error: " (prin1 type) ": " (prin1 error)))

(defun org-newtab--send-data (data)
  "Send DATA to socket."
  (websocket-send-text org-newtab--ws-socket data))

(defun org-newtab--decipher-message-from-frame-text (frame-text)
  "Decipher FRAME-TEXT and return the message."
  (let* ((json-object-type 'plist)
	 (json-array-type 'list)
	 (json (json-read-from-string frame-text)))
    json))

(provide 'org-newtab-server)

;; Local Variables:
;; coding: utf-8
;; flycheck-disabled-checkers: (emacs-lisp-package)
;; End:

;;; org-newtab-server.el ends here