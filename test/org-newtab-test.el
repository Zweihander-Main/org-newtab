;;; org-newtab-test.el --- Tests for org-newtab-*-lexical-binding:t-*-

;; Copyright (C) 2023-2024, Zweihänder <zweidev@zweihander.me>
;;
;; Author: Zweihänder <zweidev@zweihander.me>
;; Keywords: outlines
;; Homepage: https://github.com/Zweihander-Main/org-newtab
;; Version: 0.0.3

;; SPDX-License-Identifier: AGPL-3.0-or-later

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

(describe "Converting colors"
  (it "works with hex provided colors"
    (expect (org-newtab--string-color-to-hex "#0000FF") :to-equal "#0000FF"))
  (it "works with named provided colors"
    (expect (org-newtab--string-color-to-hex "blue") :to-equal "#0000ff"))
  (it "works with nil"
    (expect (org-newtab--string-color-to-hex nil) :to-equal nil)))

(describe "Fetching tags"
  (it "works with default faces"
    (let ((org-tag-faces '(("1#A" . diary)
                           ("2#B" . org-warning))))
      (expect (org-newtab--get-tag-faces) :to-equal
              '(("1#A") ;; Face-foreground has term frame in buttercup test
                ("2#B")))))

  (it "works with hex provided colors"
    (let ((org-tag-faces '(("1#A" . "#0000FF")
                           ("2#B" . "#f0c674"))))
      (expect (org-newtab--get-tag-faces) :to-equal
              '(("1#A" . "#0000FF")
                ("2#B" . "#f0c674")))))

  (it "works with named provided colors"
    (let ((org-tag-faces '(("1#A" . "blue")
                           ("2#B" . "yellow1"))))
      (expect (org-newtab--get-tag-faces) :to-equal
              '(("1#A" . "#0000ff")
                ("2#B" . "#ffff00")))))

  (it "works with hex foreground faces"
    (let ((org-tag-faces '(("1#A" :foreground "#42A5F5" :weight bold :italic t)
                           ("2#B" :foreground "#CC2200" :weight bold :underline t))))
      (expect (org-newtab--get-tag-faces) :to-equal
              '(("1#A" . "#42A5F5")
                ("2#B" . "#CC2200")))))

  (it "works with named provided colors as foreground faces"
    (let ((org-tag-faces '(("1#A" :foreground "yellow1" :weight bold)
                           ("2#B" :foreground "green yellow" :weight bold)
                           ("3#C" :foreground "blue" :weight bold :underline t))))
      (expect (org-newtab--get-tag-faces) :to-equal
              '(("1#A" . "#ffff00")
                ("2#B" . "#ffff00") ;; Running in term
                ("3#C" . "#0000ff")))))

  (it "doesn't break on empty foreground face"
    (let ((org-tag-faces `(("1#A" :weight bold))))
      (expect (org-newtab--get-tag-faces) :to-equal '(("1#A")))))

  (it "doesn't break on nil"
    (let ((org-tag-faces nil))
      (expect (org-newtab--get-tag-faces) :to-equal nil))))

(provide 'org-newtab-test)

;; Local Variables:
;; coding: utf-8
;; End:

;;; org-newtab-test.el ends here
