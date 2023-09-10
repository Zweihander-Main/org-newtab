;;; org-newtab-mode.el --- WIP -*-lexical-binding:t-*-

;; Copyright (C) 2023, Zweihänder <zweidev@zweihander.me>
;;
;; Author: Zweihänder
;; Keywords: outlines
;; Homepage: https://github.com/Zweihander-Main/org-newtab
;; Version: 0.0.1
;; Package-Requires: ((emacs "27.1") (websocket "1.7") (async "1.9.4"))

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

(require 'org-newtab-server)

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
    (add-hook 'org-clock-in-hook #'org-newtab--on-msg-send-clocked-in))
   (t
    (org-newtab--close-server)
    (remove-hook 'org-clock-in-hook #'org-newtab--on-msg-send-clocked-in))))

(provide 'org-newtab-mode)

;; Local Variables:
;; coding: utf-8
;; End:

;;; org-newtab-mode.el ends here
