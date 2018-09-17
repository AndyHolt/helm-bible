;; could produce json version of bible text, which would allow use of the json
;; library, might be quite a neat way to do it
;;
;; json data structure like
;; book:
;; chapter:
;; verse:
;; esv-text:
;; niv-text:
;; sbl-text:
;; nlt-text:
;; etc

;; helm-pattern is the current entry to the helm search box, entered by the
;; user. Dynamic variable.

;; function to format the track for display, which sets the name of the
;; candidate for display in helm, can be made up of various elements
;; (format "%s %s")


;;; code:

(require 'helm)

;; set up some dummy data
;; need to find a way to get the real data in
(defvar bible-verses
  '(((name . "Genesis 1:1")
     (book . "Genesis")
     (chapter . "1")
     (verse . "1")
     (esv-text . "In the beginning, God created the heavens and the earth."))
    ((name . "Genesis 1:2")
     (book . "Genesis")
     (chapter . "1")
     (verse . "2")
     (esv-text . "The earth was without form and void, and darkness was over the face of the deep"))))

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
            bible-verses))

(defun helm-bible-display-verse (verse)
  "An action to display the text of VERSE in the mini-buffer."
  (message (cdr (assoc 'esv-text verse))))

(defvar helm-bible-actions
  (helm-make-actions
   "Display verse" 'helm-bible-display-verse)
  "Create the actions for helm-bible.")

(defvar helm-source-bible
      (helm-build-sync-source "Bible"
        :candidates (helm-bible-search)
        :action helm-bible-actions)
      "Create the primary bible search source.")

(helm :sources '(helm-source-bible)
      :buffer "*helm bible*")

(provide 'helm-bible)
;;; helm-bible ends here
