;;; helm-bible.el --- bible search and other tools in helm

;; Author: Andy Holt <andrew.holt@hotmail.co.uk>
;; Version: 0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A bible search tool for Emacs, based on Helm.

;;; Code:

(require 'helm)

(defun helm-bible-text-to-assoc-list (line)
    "Take the LINE from text file and return assoc list of data."
    (string-match "\\([A-Za-z0-9 ]+\\) \\([0-9]+\\):\\([0-9]+\\): \\(.*\\)" line)
    (list (cons 'name (substring line (match-beginning 1) (match-end 3)))
          (cons 'book (match-string 1 line))
          (cons 'chapter (match-string 2 line))
          (cons 'verse (match-string 3 line))
          (cons 'esv-text (match-string 4 line))))

(defun helm-bible-get-bible-verses ()
  "Get a list of assoc lists of all bible verses."
  (mapcar 'helm-bible-text-to-assoc-list
          (with-temp-buffer
            (insert-file-contents "~/Bible/helm-bible/esv-bible-text.txt")
            (split-string (buffer-string) "\n" t))))


(defun helm-bible-format-verse-for-display (verse)
  "Format VERSE for display in helm selection.

Take the association list that describes the verse (VERSE) and return the string
that will be displayed in the helm interface.

TODO: Make display more useful, include the text of the verse in
the display.  And prettify the formatting so that the columns of
book name, chapter number, verse number etc line up."
  (concat (cdr (assoc 'name verse)) " " (cdr (assoc 'esv-text verse))))


(defun helm-bible-search ()
    "Create candidates list for helm-bible.

Returns a list of pairs, with the car of each as the display, and
the cdr as the whole verse data structure.
This function is used by in defining `helm-source-bible' to provide the list of
candidates."
    (mapcar (lambda (verse)
              (cons (helm-bible-format-verse-for-display verse)
                    verse))
            (helm-bible-get-bible-verses)))

(defun helm-bible-action-display-verse (verse)
  "An action to display the text of VERSE in the mini-buffer."
  (message (mapconcat 'identity (mapcar (lambda (this-verse)
                                          (cdr (assoc 'esv-text this-verse)))
                                        (helm-marked-candidates))
                      " ")))

(defun helm-bible-action-insert-verse-text (verse)
  "Insert the text of the selected Bible verse at point."
  (insert (mapconcat 'identity (mapcar (lambda (this-verse)
                                         (cdr (assoc 'esv-text this-verse)))
                                       (helm-marked-candidates))
                     " ")))

;; [todo] - In order to correctly format the reference, will need to come up
;; with a clever way of getting all the verses and turning the appropriate one
;; into a range, rather than listing every verse. But needs to deal with the
;; cases where verses go over a chapter break or where verses from different
;; places are given.
(defun helm-bible-action-insert-reference (verse)
  "Insert the reference of the selected Bible verse at point."
  (insert (cdr (assoc 'name verse))))

(defun helm-bible-action-insert-verse-with-reference (verse)
  "Insert the text of the selected Bible verse at point, with a reference."
  (insert (concat (cdr (assoc 'esv-text verse))
                  " ("
                  (cdr (assoc 'name verse))
                  ")")))

(defun helm-bible-action-goto-notes (verse)
  "Go to notes file and location for notes on that verse."
  (find-file (format "~/Documents/BibleNotes/%s.org"
                     (replace-regexp-in-string " " ""
                                               (cdr (assoc 'book verse)))))
  (goto-char (point-min))
  (re-search-forward (format "^\* %s %s"
                             (cdr (assoc 'book verse))
                             (cdr (assoc 'chapter verse)))))

(defun helm-bible-action-create-buffer (verse)
  "Make a notes buffer (org-mode) with the selected candidates."
  (switch-to-buffer "bible-search.org")
  (insert (mapconcat 'identity
                     (mapcar (lambda (this-verse)
                               ;; insert here code to produce an org-element
                               ;; with the appropriate content for each marked candidate
                               (concat "** "
                                       (cdr (assoc 'name this-verse))
                                       "\n"
                                       (cdr (assoc 'esv-text this-verse))
                                       "\n"))
                             (helm-marked-candidates))
                     "\n")))

(defvar helm-bible-actions
  (helm-make-actions
   "Display verse" 'helm-bible-action-display-verse
   "Insert verse (text only)" 'helm-bible-action-insert-verse-text
   "Insert verse reference" 'helm-bible-action-insert-reference
   "Insert verse with reference" 'helm-bible-action-insert-verse-with-reference
   "Goto notes" 'helm-bible-action-goto-notes
   "List selected verses in buffer" 'helm-bible-action-create-buffer)
  "Create the actions for helm-bible.")

(defvar helm-source-bible
      (helm-build-sync-source "Bible"
        :candidates (helm-bible-search)
        :action helm-bible-actions)
      "Create the primary bible search source.")

(defun helm-bible ()
  "Search the Bible."
  (interactive)
  (helm :sources '(helm-source-bible)
        :buffer "*helm bible*"))

(provide 'helm-bible)
;;; helm-bible ends here
