;;; zweigtd-newtab.el --- WIP -*-lexical-binding:t-*-

;; Copyright (C) 2021, Zweihänder <zweidev@zweihander.me>
;;
;; Author: Zweihänder
;; Keywords: outlines
;; Homepage: https://github.com/Zweihander-Main/zweigtd-newtab
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.1"))

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

(require 'websocket)

(setq websocket-debug t)

(defvar zweigtd-newtab-ws-socket nil
  "The websocket for `zweigtd-newtab'.")

(defvar zweigtd-newtab-ws-server nil
  "The websocket server for `zweigtd-newtab'.")

(defcustom zweigtd-newtab-ws-port
  35942
  "Port to server websocket server on."
  :type 'integer
  :group 'zweigtd-newtab)

;;;###autoload
(define-minor-mode
  zweigtd-newtab-mode
  "Enable `zweigtd-newtab'
This serves the web-build and API over HTTP."
  :lighter " zweigtd-newtab"
  :global t
  :group 'zweigtd-newtab
  :init-value nil
  (cond
   (zweigtd-newtab-mode
    (setq zweigtd-newtab-ws-server
          (websocket-server
           zweigtd-newtab-ws-port
           :host 'local
           :on-open #'zweigtd-newtab--ws-on-open
           :on-message #'zweigtd-newtab--ws-on-message
           :on-close #'zweigtd-newtab--ws-on-close
           :on-error #'zweigtd-newtab--ws-on-error)))
   (t
    (websocket-server-close zweigtd-newtab-ws-server))))

(defun zweigtd-newtab--ws-on-open (ws)
  "Open the websocket WS and send initial data."
  (setq zweigtd-newtab-ws-socket ws)
  (message "Open"))

(defun zweigtd-newtab--ws-on-message (ws frame)
  "Take WS and FRAME as arguments when message received."
  (prin1 frame))

(defun zweigtd-newtab--ws-on-close (ws)
  "Perform when WS is closed."
  (setq zweigtd-newtab-ws-socket nil)
  (message "Closed"))

(defun zweigtd-newtab--ws-on-error (ws type error)
  "Handle ERROR of TYPE from WS."
  (concat (prin1 type) ": " (prin1 error)))

(defun zweigtd-newtab--send-text (text)
  "Send TEXT to socket."
  (websocket-send-text zweigtd-newtab-ws-socket text))

(defun zweigtd-newtab--send-agenda ()
	""
	)

(provide 'zweigtd-newtab)

;; Local Variables:
;; coding: utf-8
;; flycheck-disabled-checkers: 'emacs-lisp-elsa
;; End:

;;; zweigtd-newtab.el ends here
