(require 'denote nil t)

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

(defvar *ibooks-db* (make-hash-table :test 'equal))

(defun ibooks-db/get-sqlite (directory sub-dir pattern)
  "Get a list of database file names in DIRECTORY matching the given PATTERN."
  (let ((file-names '()))
    (dolist (file (directory-files (expand-file-name sub-dir directory) nil pattern))
      (push (expand-file-name (concat directory sub-dir file)) file-names))
    file-names))

(defun ibooks-db/open (dbname)
  (or (gethash dbname *ibooks-db*)
      (setf (gethash dbname *ibooks-db*) (sqlite-open dbname))))

(defun ibooks-db/close (dbname)
  (when-let ((db (gethash dbname *ibooks-db*)))
    (sqlite-close db)
    (remhash dbname *ibooks-db*)))

(defun ibooks-db/book-from-db (BKLibrary-db)
  (let ((db (ibooks-db/open BKLibrary-db)))
    (sqlite-execute db ibooks-db/BOOKS-QUERY)))

(defun ibooks-db/books (BKLibrary-db)
  (mapcan 'ibooks-db/book-from-db BKLibrary-db))

(defun ibooks-db/annotations-from-db (dbname)
  (let ((db (ibooks-db/open dbname)))
    (sqlite-execute db ibooks-db/ANNOTATIONS-QUERY)))

(defun ibooks-db/annotations (AEAnnotation-DB)
  (mapcan 'ibooks-db/annotations-from-db AEAnnotation-DB))

(defun ibooks-annot/highlights-for-book (book-id AEAnnotation-DB)
  (cl-remove-if-not (lambda (annotation)
                      (string= (car annotation) book-id))
                    (ibooks-db/annotations AEAnnotation-DB)))

(defun ibooks-annot/highlights-count (book-id AEAnnotation-DB)
  "Get the number of annotations for a book with BOOK-ID."
  (length (ibooks-annot/highlights-for-book book-id AEAnnotation-DB)))

(defun ibooks-annot/highlights-color (color)
  (cond
   ((= color 1) "*")
   ((= color 2) "=")
   ((= color 3) "/")
   ((= color 4) "*")
   (t "")))

(defun ibooks-annot/book-note-exist-p (title)
  "Check if a book note with TITLE exists in the denote directory."
  (let ((note nil))
    (require 'denote nil t)
    (dolist (file (denote-directory-files))
      (if (string= title (denote-retrieve-title-value file 'org))
          (setq note file)))
    note))

(defvar ibooks-annot/book-note-hook nil)

(defvar *ibooks-annot/book-alist* nil)

(defun ibooks-annot/choose-book (BKLibrary-db AEAnnotation-DB)
  "Choose a book and return its ID."
  (let* ((books (ibooks-db/books BKLibrary-db))
         (book-alist (mapcar (lambda (book)
                               (let* ((book-id (car book))
                                      (book-title (cadr book)))
                                 (cons book-title book-id)))
                             books))
         (book-title (completing-read "Choose a book: " book-alist)))
    (setq *ibooks-annot/book-alist* book-alist)
    (ibooks-db/close (car BKLibrary-db))
    (cdr (assoc book-title book-alist))))

(defvar ibooks-annot/book-note-highlights-heading "Annotations Extracted")

(defun ibooks-annot/remove-heading-in-note (heading note-path)
  "Remove existing annotations section from NOTE-PATH."
  (with-current-buffer (find-file-noselect note-path)
    (goto-char (point-min))
    (while (re-search-forward heading nil t)
      (org-mark-subtree)
      (delete-region (region-beginning) (region-end))
      (save-buffer))))

(defvar ibooks-annot/book-note-directory "~/org")

(defun ibooks-annot/denote-temp-buffer (title directory)
  (when-let ((text (buffer-substring-no-properties (region-beginning) (region-end))))
    (denote title (denote-keywords-prompt) nil directory)
    (push-mark (point))
    (insert text)))

(defun ibooks-annot/write-highlights-to-note (book-id AEAnnotation-DB)
  "Write ANNOTATIONS to a temporary buffer and return the buffer."
  (let* ((book-title (car (rassoc book-id *ibooks-annot/book-alist*)))
         (highlights (ibooks-annot/highlights-for-book book-id AEAnnotation-DB))
         (book-note-path (ibooks-annot/book-note-exist-p book-title))
         (heading ibooks-annot/book-note-highlights-heading))
    (with-temp-buffer
      (when highlights
        (insert (format "* %s\n" heading))
        (dolist (annot highlights)
          (let ((symbol (ibooks-annot/highlights-color (nth 4 annot)))
                (text (cadr annot)))
            (insert (format "%s%s%s\n" symbol text symbol))))
        (if book-note-path
            (progn
              (ibooks-annot/remove-heading-in-note heading book-note-path)
              (append-to-file (point-min) (point-max) book-note-path)
              (ibooks-db/close (car AEAnnotation-DB)))
          (when-let ((text (buffer-substring-no-properties (point-min) (point-max))))
            (denote book-title (denote-keywords-prompt) nil ibooks-annot/book-note-directory)
            (push-mark (point))
            (insert text)))))))

;;;###autoload
(defun ibooks-annot/choose-book-and-save-to-file ()
  "Choose a book and save its annotations to NOTE-PATH."
  (interactive)
  (let* ((AEAnnotation-DB (ibooks-db/get-sqlite IBOOKS-DATA "AEAnnotation/" "AEAnnotation.*\\.sqlite$"))
         (BKLibrary-DB (ibooks-db/get-sqlite IBOOKS-DATA "BKLibrary/" "BKLibrary.*\\.sqlite$"))
         (book-id (ibooks-annot/choose-book BKLibrary-DB AEAnnotation-DB)))
    (when book-id
      (ibooks-annot/write-highlights-to-note book-id AEAnnotation-DB))))

(provide 'ibooks-annot)
