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
(require 'org-newtab-agenda)
(require 'org-clock)
(require 'async)
(require 'websocket)

(defvar org-newtab--ws-socket nil
  "The WebSocket for `org-newtab'.")

(defvar org-newtab--ws-server nil
  "The WebSocket server for `org-newtab'.")

(defvar org-newtab--debug-mode nil
  "Whether or not to turn on every debugging tool.")

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

(defun org-newtab--debug-mode ()
  "Turn on every debug setting."
  (setq org-newtab--debug-mode (not org-newtab--debug-mode))
  (setq debug-on-error org-newtab--debug-mode
        websocket-callback-debug-on-error org-newtab--debug-mode
        async-debug org-newtab--debug-mode))

(defun org-newtab--ws-on-open (ws)
  "Open the WebSocket WS and send initial data."
  (setq org-newtab--ws-socket ws)
  (org-newtab--log "[Server] %s" "on-open")
  (org-newtab--on-opn-send-tag-faces))

(defun org-newtab--ws-on-message (_ws frame)
  "Take WS and FRAME as arguments when message received."
  (org-newtab--log "[Server] %s" "on-message")
  (let* ((frame-text (websocket-frame-text frame))
         (json-data (org-newtab--decipher-message-from-frame-text frame-text)))
    (org-newtab--log "[Server] Received %S from client" json-data)
    (let ((command (plist-get json-data :command))
          (resid (plist-get json-data :resid)))
      (cond ((org-clocking-p)
             (org-newtab--on-msg-send-clocked-in resid))
            ((string= command "getItem")
             (org-newtab--on-msg-send-match-query (plist-get json-data :data) resid))
            (t
             (org-newtab--log "[Server] %s" "Unknown command from client"))))))

(defun org-newtab--on-opn-send-tag-faces ()
  "Send the tag faces to the client."
  (let* ((tags (org-newtab--get-tag-faces))
         (data-packet (list :type "TAGS" :data tags)))
    (org-newtab--send-data (json-encode data-packet))))

(defun org-newtab--on-msg-send-clocked-in (&optional resid)
  "Send the current clocked-in item to the client -- with RESID if provided."
  (let* ((item (org-newtab--get-clocked-in-item))
         (data-packet (list :type "ITEM" :data item)))
    (when resid
      (setq data-packet (plist-put data-packet :resid resid)))
    (org-newtab--send-data (json-encode data-packet))))

(defun org-newtab--on-msg-send-match-query (data &optional resid)
  "Send the current match for query DATA to the client -- with RESID if provided."
  (async-start
   `(lambda () ; TODO: if it becomes interactive (asks for prompt), it freezes
      ,(async-inject-variables "\\`load-path\\'") ;  TODO: Reliant on load path being set to agenda dir
      ,(async-inject-variables "\\`org-agenda-files\\'")
      ,(async-inject-variables "\\`org-todo-keywords\\'")
      (require 'org-newtab-agenda)
      (org-newtab--get-one-agenda-item ',data))
   `(lambda (result)
      (let ((data-packet (list :type "ITEM" :data result)))
        (when ,resid
          (setq data-packet (plist-put data-packet :resid ,resid)))
        (org-newtab--send-data (json-encode data-packet))))))

(defun org-newtab--ws-on-close (_ws)
  "Perform when WS is closed."
  (setq org-newtab--ws-socket nil)
  (org-newtab--log "[Server] %s" "on-close"))

(defun org-newtab--ws-on-error (_ws type error)
  "Handle ERROR of TYPE from WS."
  (org-newtab--log "[Server] Error: %S : %S" (prin1 type) (prin1 error)))

(defun org-newtab--send-data (data)
  "Send DATA to socket. If socket is nil, drop the data and do nothing."
  (when org-newtab--ws-socket
    (org-newtab--log "[Server] Sending %S to client" data)
    (condition-case err
        (websocket-send-text org-newtab--ws-socket data)
      (error (org-newtab--log "[Server] Error sending data to client: %S" err)))))

(defun org-newtab--decipher-message-from-frame-text (frame-text)
  "Decipher FRAME-TEXT and return the message."
  (let* ((json-object-type 'plist)
         (json-array-type 'list)
         (json (json-read-from-string frame-text))) ;; TODO: error handling for "\261R\30\7", missing data, command, etc.
    json))

(provide 'org-newtab-server)

;; Local Variables:
;; coding: utf-8
;; End:

;;; org-newtab-server.el ends here
