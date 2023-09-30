;;; org-newtab.el --- Supercharge your browser's new tab page -*-lexical-binding:t-*-

;; Copyright (C) 2023, Zweihänder <zweidev@zweihander.me>
;;
;; Author: Zweihänder <zweidev@zweihander.me>
;; Keywords: outlines
;; Homepage: https://github.com/Zweihander-Main/org-newtab
;; Version: 0.0.1
;; Package-Requires: ((emacs "27.1") (websocket "1.7") (async "1.9.4"))

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

;; Org-NewTab is a browser extension which sets the org-agenda task you should
;; be working on as your new tab page. It's comprised of two parts: a browser
;; extension and an Emacs package. The Emacs package runs a WebSocket server
;; which the browser extension talks to.

;; The Emacs side is invoked with `org-newtab-mode'. This starts a WebSocket
;; server which listens on `org-newtab-ws-port'. The browser extension connects
;; in and will send an org-agenda match query which is then used by Emacs to
;; find the top task to work on. On connection, the Emacs side will send in
;; data for `org-tag-faces' used to color the task background in the browser
;; extension. The Emacs side will send data over when it's either requested
;; by the browser extension (when it first connects) or when a task is changed
;; (clocked in/out, marked as done, etc).

;;; Code:

(require 'cl-lib)

(defgroup org-newtab nil
  "A browser new tab page linked to `org-agenda'."
  :group 'org-newtab
  :prefix "org-newtab-"
  :link `(url-link :tag "Github" "https://github.com/Zweihander-Main/org-newtab"))

(defcustom org-newtab-ws-port
  35942
  "Port to server WebSocket server on."
  :type 'integer
  :group 'org-newtab)

(defun org-newtab--log (format-string &rest args)
  "Log FORMAT-STRING and ARGS to `org-newtab-log-buffer'."
  (with-current-buffer (get-buffer-create "*org-newtab-log*")
    (goto-char (point-max))
    (insert (apply #'format format-string args))
    (insert "\n")))

(provide 'org-newtab)

(cl-eval-when (load eval)
  (cl-pushnew (expand-file-name default-directory) load-path)
  (require 'org-newtab-mode))

;; Local Variables:
;; coding: utf-8
;; End:

;;; org-newtab.el ends here
