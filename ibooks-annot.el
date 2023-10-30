;;; ibooks-annot.el --- Extract annotations from Apple books -*- lexical-binding: t -*-

;; Author: Jousimies <duan_n@outlook.com>
;; Maintainer: Jousimies <duan_n@outlook.com>
;; URL: https://github.com/Jousimies/ibooks-annot.el
;; Package-Requires: ((emacs "28.1") (denote "2.0.0") (org "9.6.10") (json "1.5"))
;; Copyright (C) 2023, Jousimies, all rights reserved.
;; Created: 2023-10-27
;; Last-Updated: 2023-10-28T22:47:35+08:00

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'denote nil t)
(require 'cl-seq nil t)
(require 'org nil t)
(require 'json nil t)

(defun ibooks-db/get-sqlite (directory sub-dir pattern)
  "Get a list of database in SUB-DIR of DIRECTORY matching the PATTERN."
  (let ((file-names '()))
    (dolist (file (directory-files (expand-file-name sub-dir directory) nil pattern))
      (push (expand-file-name (concat directory sub-dir file)) file-names))
    file-names))

(defconst IBOOKS-DATA "~/Library/Containers/com.apple.iBooksX/Data/Documents/")
(defconst ibooks-db/ANNOTATIONS-QUERY
  "SELECT ZANNOTATIONASSETID AS assetId,
          ZANNOTATIONSELECTEDTEXT AS quote,
          ZANNOTATIONNOTE AS comment,
          ZFUTUREPROOFING5 AS chapter,
          ZANNOTATIONSTYLE AS colorCode,
          ZANNOTATIONMODIFICATIONDATE AS modifiedAt,
          ZANNOTATIONCREATIONDATE AS createdAt
   FROM ZAEANNOTATION
   WHERE ZANNOTATIONDELETED = 0
     AND ZANNOTATIONSELECTEDTEXT IS NOT NULL
     AND ZANNOTATIONSELECTEDTEXT <> ''
   ORDER BY ZANNOTATIONASSETID, ZPLLOCATIONRANGESTART;")

(defconst ibooks-db/BOOKS-QUERY
  "SELECT ZASSETID AS id, ZTITLE AS title, ZAUTHOR AS author
   FROM ZBKLIBRARYASSET")

(defvar *ibooks-db* (make-hash-table :test 'equal)
  "A hash table to store iBooks database information.")

(defvar ibooks-annot/book-alist nil
  "A list to store iBooks name and ID extract from database.")

(defvar ibooks-annot/books-location (expand-file-name "~/Library/Mobile Documents/iCloud~com~apple~iBooks/Documents/")
  "The location of iBooks documents system.")

(defvar ibooks-annot/book-note-highlights-heading "Annotations Extracted"
  "The heading used for extracted annotations book note.")

(defvar ibooks-annot/book-note-directory "~/org"
  "The directory where book note are stored.")

(defvar pdfannots-script nil
  "A variable to store pdfannot shell command, if applicable.")

(defvar ibooks-annot/python-command "/usr/bin/python3"
  "The path to the Python 3 executable on system.")

(defvar ibooks-annot/BKLibrary-DB (ibooks-db/get-sqlite IBOOKS-DATA "BKLibrary/" "BKLibrary.*\\.sqlite$")
  "The database for iBooks library information.")

(defvar ibooks-annot/AEAnnotation-DB (ibooks-db/get-sqlite IBOOKS-DATA "AEAnnotation/" "AEAnnotation.*\\.sqlite$")
  "The database for iBooks annotations information.")

(defvar ibooks-annot/highlights-color-list '((1 . "#7cc867")
                                             (2 . "#69aff0")
                                             (3 . "#f9cd59")
                                             (4 . "#fb5b89"))
  "This list specifies four different colors for highlighting various types of content.
Use the associated numbers for different purposes:
- 1: Normal knowledge
- 2: Original sources
- 3: Questions
- 4: Imported knowledge.

The colors were extract from pdf by pdfannots. Please adjust it according to you content.
Execute shell command `python3 pdfannot.py -f json xxx.pdf',
check the output json file to find color value.")

(defun ibooks-db/open (dbname)
  (or (gethash dbname *ibooks-db*)
      (setf (gethash dbname *ibooks-db*) (sqlite-open dbname))))

(defun ibooks-db/close (dbname)
  (when-let ((db (gethash dbname *ibooks-db*)))
    (sqlite-close db)
    (remhash dbname *ibooks-db*)))

(defun ibooks-db/fetch-book-from-db (library)
  (let ((db (ibooks-db/open library)))
    (sqlite-execute db ibooks-db/BOOKS-QUERY)))

(defun ibooks-db/get-all-books ()
  (mapcan 'ibooks-db/fetch-book-from-db ibooks-annot/BKLibrary-DB))

(defun ibooks-db/fetch-highlights-from-db (annotation)
  (let ((db (ibooks-db/open annotation)))
    (sqlite-execute db ibooks-db/ANNOTATIONS-QUERY)))

(defun ibooks-db/get-all-highlights ()
  (mapcan 'ibooks-db/fetch-highlights-from-db ibooks-annot/AEAnnotation-DB))

(defun ibooks-annot/get-book-highlights (book-id)
  (cl-remove-if-not (lambda (annotation)
                      (string= (car annotation) book-id))
                    (ibooks-db/get-all-highlights)))

(defun ibooks-annot/book-highlights-count (book-id)
  "Get the number of annotations for a book with BOOK-ID."
  (length (ibooks-annot/get-book-highlights book-id)))

(defun ibooks-annot/code-to-symbol (code)
  (cond
   ((= code 0) "_")
   ((= code 1) "~")
   ((= code 2) "=")
   ((= code 3) "/")
   ((= code 4) "*")
   (t "")))

(defun ibooks-annot/color-to-symbol (color)
  (let ((code (car (rassoc color ibooks-annot/highlights-color-list))))
    (ibooks-annot/code-to-symbol code)))

(defun ibooks-annot/highlights-color (colorcode)
    "Return a character corresponding to `COLORCODE'.

If COLORCODE  is a number (1-4), the function returns the character
'~', '=', '/', or '*'.

If COLORCODE is a color string, the function returns the
   corresponding character from the `custom-highlights-list'.

If neither condition is met, an empty string is returned."
  (if (numberp colorcode)
      (ibooks-annot/code-to-symbol colorcode)
    (ibooks-annot/color-to-symbol colorcode)))

(defun ibooks-annot/book-note-exist-p (title)
  "Check if a book note with TITLE exists in the denote directory."
  (let ((note nil))
    (require 'denote nil t)
    (dolist (file (denote-directory-files))
      (if (string= title (denote-retrieve-title-value file 'org))
          (setq note file)))
    note))

(defun ibooks-annot/update-book-alist ()
  "Update `ibooks-annot/book-alist' with book titles and IDs from the database."
  (setq ibooks-annot/book-alist
        (mapcar (lambda (book)
                  (cons (cadr book) (car book)))
                (ibooks-db/get-all-books))))

(defun ibooks-annot/choose-book ()
  "Choose a book and return its ID."
  (let* ((book-alist (ibooks-annot/update-book-alist))
         (book-title (completing-read "Choose a book: " book-alist)))
    (ibooks-db/close (car ibooks-annot/BKLibrary-DB)) ;close BKLIBRARY-DB after update book alist
    (cdr (assoc book-title book-alist))))

(defun ibooks-annot/remove-heading-in-note (heading note-path)
  "Remove existing annotations section from NOTE-PATH."
  (with-current-buffer (find-file-noselect note-path)
    (goto-char (point-min))
    (while (re-search-forward heading nil t)
      (org-mark-subtree)
      (delete-region (region-beginning) (region-end))
      (save-buffer))))

(defun ibooks-annot/denote-temp-buffer (title directory)
  (when-let ((text (buffer-substring-no-properties (region-beginning) (region-end))))
    (denote title (denote-keywords-prompt) nil directory)
    (push-mark (point))
    (insert text)))

(defun ibooks-annot/get-bookname-extension (directory)
  "Get a list of cons cells containing file names and extensions in `DIRECTORY'."
  (let ((files (directory-files directory nil "^[^.].*")))
    (mapcar (lambda (file)
              (cons (file-name-sans-versions file) (file-name-extension file)))
            files)))

(defun ibooks-annot/bookname-partial-match (str1 str2)
  "Check if `STR1' is a partial match of `STR2'."
  (string-match-p (regexp-quote str1) str2))

(defun ibooks-annot/get-book-extension (book-title)
  (let* ((book-and-ext (ibooks-annot/get-bookname-extension ibooks-annot/books-location))
         (ext (cl-assoc book-title book-and-ext :test #'ibooks-annot/bookname-partial-match)))
    (cdr ext)))

(defun ibooks-annot/get-book-path-in-cloud (book-title)
  (let* ((book-and-ext (ibooks-annot/get-bookname-extension ibooks-annot/books-location))
         (ext (cl-assoc book-title book-and-ext :test #'ibooks-annot/bookname-partial-match)))
    (expand-file-name (car ext) ibooks-annot/books-location)))

(defun ibooks-annot/extract-epub-highlights (book-id book-title book-note)
  (let* ((highlights (ibooks-annot/get-book-highlights book-id)))
    (ibooks-db/close (car ibooks-annot/AEAnnotation-DB)) ;close AEANNOTATION-DB after extract highlights
    (with-temp-buffer
      (when highlights
        (insert (format "* %s\n" ibooks-annot/book-note-highlights-heading))
        (dolist (annot highlights)
          (let ((symbol (ibooks-annot/highlights-color (nth 4 annot)))
                (highlight (cadr annot))
                (comment (caddr annot)))
            (insert (format "%s%s%s\n" symbol highlight symbol))
            (when comment
              (insert (format "# %s%s%s\n" symbol comment symbol)))))
        (ibooks-annot/write-to-note book-title book-note)))))

(defun ibooks-annot/extract-pdf-highlights (book-title book-note)
  "Extract PDF highlight for the given `BOOK-TITLE' and write to `BOOK-NOTE'."
  (let* ((book-path (ibooks-annot/get-book-path-in-cloud book-title))
         (parsed-json (shell-command-to-string
                       (format "%s %s %s" ibooks-annot/python-command pdfannots-script (shell-quote-argument book-path))))
         (highlights (ibooks-annot/parse-pdf-highlights parsed-json)))
    (ibooks-annot/write-highlights-to-note book-title book-note highlights)))

(defun ibooks-annot/parse-pdf-highlights (json-data)
  "Parse `JSON-DATA' and return a list of highlight."
  (let ((highlights '()))
    (with-temp-buffer
      (let ((json-object-type 'hash-table))
        (mapc (lambda (entry)
                (message "%s" (cons (ibooks-annot/format-highlight entry) highlights))
                (setq highlights (cons (ibooks-annot/format-highlight entry) highlights)))
              (json-read-from-string json-data)))
      (nreverse highlights))))

(defun ibooks-annot/format-highlight (entry)
  "Format a single highlight `ENTRY'."
  (let ((color (ibooks-annot/highlights-color (gethash "color" entry)))
        (text (gethash "text" entry))
        (contents (gethash "contents" entry)))
    (if contents
        (format "%s%s%s\n# %s\n" color text color contents)
      (format "%s%s%s\n" color text color))))

(defun ibooks-annot/write-highlights-to-note (book-title book-note highlights)
  "Write `Highlight' to `BOOK-NOTE' for the given `BOOK-TITLE'."
  (with-temp-buffer
    (insert (format "* %s\n" ibooks-annot/book-note-highlights-heading))
    (dolist (highlight highlights)
      (insert highlight))
    (ibooks-annot/write-to-note book-title book-note)))

(defun ibooks-annot/write-to-note (book-title book-note)
  "Write the buffer content to `BOOK-NOTE'.
Create a new note with `BOOK-TITLE' if needed."
  (if book-note
      (progn
        (ibooks-annot/remove-heading-in-note ibooks-annot/book-note-highlights-heading book-note)
        (append-to-file (point-min) (point-max) book-note))
    (when-let ((text (buffer-substring-no-properties (point-min) (point-max))))
      (denote book-title (denote-keywords-prompt) nil ibooks-annot/book-note-directory)
      (push-mark (point))
      (insert text))))

;;;###autoload
(defun ibooks-annot/extract-highlights-to-note ()
  "Choose a book and save its annotations to NOTE-PATH."
  (interactive)
  (let* ((book-id (ibooks-annot/choose-book))
         (book-title (car (rassoc book-id ibooks-annot/book-alist)))
         (book-note (ibooks-annot/book-note-exist-p book-title))
         (book-ext (ibooks-annot/get-book-extension book-title)))
    (when book-id
      (cond ((string= book-ext "epub") (ibooks-annot/extract-epub-highlights book-id book-title book-note))
            ((string= book-ext "pdf") (if pdfannots-script
                                          (ibooks-annot/extract-pdf-highlights book-title book-note)
                                        (message "Install pdfannots first!")))))))

(provide 'ibooks-annot)

;;; ibooks-annot.el ends here
