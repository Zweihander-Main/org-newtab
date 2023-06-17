;;; org-newtab-test.el --- Tests for org-newtab-*-lexical-binding:t-*-

;; Copyright (C) 2021, Zweihänder <zweidev@zweihander.me>
;;
;; Author: Zweihänder
;; Keywords: org-mode
;; Homepage: https://github.com/Zweihander-Main/org-newtab
;; Version: 0.0.1

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

;; Tests for org-newtab

;;; Code:

(require 'buttercup)
(require 'org-newtab)

(describe "Testing"
  (it "works."
    (expect t :to-equal t)))

;; Local Variables:
;; coding: utf-8
;; flycheck-disabled-checkers: 'emacs-lisp-elsa
;; End:

;;; org-newtab-test.el ends here
