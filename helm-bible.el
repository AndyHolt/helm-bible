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

;; Adjacent references should be grouped together into a single reference of the
;; range. Multiple references from the same chapter and book should be
;; consolidated so that the book name and chapter number are only given once.
(defun helm-bible-consolidate-refs (verse1 verse2)
    "If verse2 immediately follows verse1, return a single reference which is
amalgamated of the two. Otherwise, return the two verses unchanged."
    (if ;; same bible book
        (and (equal (car verse1) (car verse2))
             ;; chapter numbers same and verse no. successive
             ;; OR last verse of chapter and first verse of next
             (or (and (= (car (last (nth 1 verse1))) (car (nth 1 verse2)))
                      (= (1+ (car (last (nth 2 verse1))))
                          (car (nth 2 verse2))))
                 ;; A speed up: don't look up last verse of this chapter unless
                 ;; the second verse is first chapter
                 (and (= (car (nth 2 verse2)) 1)
                      (= (1+ (car (last (nth 1 verse1))))
                          (car (nth 1 verse2)))
                      (equal (format "%s" (car (last (nth 2 verse1))))
                          (helm-bible-get-last-verse-of-chap verse1)))))
        ;; successive, join together
        (list
         (car verse1)
         (if (= (car (nth 1 verse1)) (car (last (nth 1 verse2))))
             (nth 1 verse1)
           (list (car (nth 1 verse1)) (car (last (nth 1 verse2)))))
         (list (car (nth 2 verse1)) (car (last (nth 2 verse2)))))
        ;; non-successive, return separately
        (list verse1 verse2)))

(defun helm-bible-last-verse-to-cons (ref)
    "Take `ref' and turn it into a cons for lookup in an assoc list"
    (string-match "\\([A-Za-z0-9 ]+\\) \\([0-9]+\\):\\([0-9]+\\)" ref)
    (cons (concat (match-string 1 ref) " " (match-string 2 ref))
          (match-string 3 ref)))

(defun helm-bible-get-last-verse-numbers ()
    "Get list of last verse numbers for each chapter of Bible"
    (mapcar 'helm-bible-last-verse-to-cons
            (with-temp-buffer
              (insert-file-contents "~/Bible/helm-bible/LastVerse.txt")
              (split-string (buffer-string) "\n" t))))
(defun helm-bible-get-last-verse-of-chap (verse)
    "Given a reference `verse', return the verse number of the final verse of
the chapter"
    (cdr (assoc
          (concat (nth 0 verse) " " (format "%s" (car (last (nth 1 verse)))))
          (helm-bible-get-last-verse-numbers))))

(defun helm-bible-consolidate-references-list (references-list)
    "Take a list of single verse references and return a consolidated list"
    (setq index 0)
    (while (< index (length references-list))
      (let ((consolidated-elem
             (helm-bible-consolidate-refs (nth index references-list)
                                          (nth (1+ index) references-list))))
        (if (eq (length consolidated-elem) 3)
            (progn
              (setq references-list (-replace-at index consolidated-elem
                                                 references-list))
              (setq references-list (-remove-at (1+ index) references-list)))
          (setq index (1+ index)))))
    references-list)

(defun helm-bible-verse-to-reference (verse)
    "Take a `verse' assoc list, as selected by helm-bible, and return a
reference structure for use in referencing code."
    (list (cdr (assoc 'book verse))
          (list (string-to-number (cdr (assoc 'chapter verse))))
          (list (string-to-number (cdr (assoc 'verse verse))))))

(defun helm-bible-group-by-book (the-list)
    "Take a consolidated verse list and return a list grouped together by book,
    i.e. the car of the sub-lists, with each subsequent element being
the cons of one of the original lists."
    (let ((grouped-list (-group-by 'car the-list)))
      (-map #'(lambda (grouped-elem)
                (cons (car grouped-elem)
                      (-map 'cdr (cdr grouped-elem))))
            grouped-list)))

(defun helm-bible-group-by-chap (list-in-book)
  "Take the list of references for a specific Bible book and group them together
  according to chapter number."
  (let ((grouped-list (-group-by 'car list-in-book)))
    (-map #'(lambda (grouped-elem)
              (cons (car grouped-elem)
                    (-map 'cadr
                          (cdr grouped-elem))))
          grouped-list)))

(defun helm-bible-group-similar-references (verse-list)
    "Take a list of consolidated bible references (adjacent references joined
together) and return a list reorganised into a list with each element one bible
    book, taking a list of all references from that Bible book."
    (-map #'(lambda (book-group)
              (cons (car book-group)
                    (helm-bible-group-by-chap (cdr book-group))))
          (helm-bible-group-by-book verse-list)))

(defun helm-bible-format-verse-reference (verse-ref)
    "Take a inidividual verse reference, of either one or two verses and return
as a string of either a single verse or a range (3 -> 3; 3 5 -> 3-5)"
    (if (eq (length verse-ref) 1)
        (format "%s" (car verse-ref))
      (format "%s-%s"
              (car verse-ref)
              (cadr verse-ref))))

(defun helm-bible-format-chapter-references (chap-references)
    "Take the list of references for a particular chapter and return a formatted
string."
    ;; reference(s) within a single chapter
    (if (eq (length (car chap-references)) 1)
        (format "%s:%s"
                (car (car chap-references))
                (mapconcat 'helm-bible-format-verse-reference
                           (cdr chap-references)
                           ", "))
      ;; reference crossing chapters, e.g. 3:15-4:2
      (format "%s:%s-%s:%s"
              (nth 0 (nth 0 chap-references))
              (nth 0 (nth 1 chap-references))
              (nth 1 (nth 0 chap-references))
              (nth 1 (nth 1 chap-references)))))

(defun helm-bible-format-book-reference (book-list)
    "Take an organised list defining the references of a particular bible book
and return a properly formatted text reference"
    (format "%s %s"
            (car book-list)
            (mapconcat 'helm-bible-format-chapter-references
                       (cdr book-list)
                       "; ")))

(defun helm-bible-format-reference (verse-list)
    "Take a list of verses from helm selection and return a formatted reference string"
    (mapconcat 'helm-bible-format-book-reference
               (helm-bible-group-similar-references
                (helm-bible-consolidate-references-list
                 (-map 'helm-bible-verse-to-reference
                         verse-list)))
               "; "))

;; Action for inserting references
(defun helm-bible-action-insert-reference (verse-list)
  "Insert the reference(s) of the selected Bible verse(s) at point."
  (insert (helm-bible-format-reference (helm-marked-candidates))))

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

(defun helm-bible-action-selected-candidates (verse)
  "Insert a list of the selected candidates for use in testing and developing."
  (insert (mapconcat 'string (helm-marked-candidates) "\n")))

(defvar helm-bible-actions
  (helm-make-actions
   "Display verse" 'helm-bible-action-display-verse
   "Insert verse (text only)" 'helm-bible-action-insert-verse-text
   "Insert verse reference" 'helm-bible-action-insert-reference
   "Insert verse with reference" 'helm-bible-action-insert-verse-with-reference
   "Goto notes" 'helm-bible-action-goto-notes
   "List selected verses in buffer" 'helm-bible-action-create-buffer
   "Get selected candidates for testing" 'helm-bible-action-selected-candidates)
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
        :buffer "*helm bible*"
        :candidate-number-limit nil))

(provide 'helm-bible)
;;; helm-bible ends here
