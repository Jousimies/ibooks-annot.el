(require 'denote nil t)

(defconst IBOOKS-DATA "~/Library/Containers/com.apple.iBooksX/Data/Documents/")
(defvar *ibooks-db* (make-hash-table :test 'equal))
(defvar ibooks-annot/book-alist nil)
(defvar ibooks-annot/books-location (expand-file-name "~/Library/Mobile Documents/iCloud~com~apple~iBooks/Documents/"))
(defvar ibooks-annot/book-note-highlights-heading "Annotations Extracted")
(defvar ibooks-annot/book-note-directory "~/org")
(defvar pdfannots "~/.emacs.d/packages/pdfannots/pdfannots.py -f json")
(defvar ibooks-annot/python-command "/usr/bin/python3")
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

(defun ibooks-db/get-sqlite (directory sub-dir pattern)
  "Get a list of database file names in DIRECTORY matching the given PATTERN."
  (let ((file-names '()))
    (dolist (file (directory-files (expand-file-name sub-dir directory) nil pattern))
      (push (expand-file-name (concat directory sub-dir file)) file-names))
    file-names))

(defvar ibooks-annot/BKLibrary-DB (ibooks-db/get-sqlite IBOOKS-DATA "BKLibrary/" "BKLibrary.*\\.sqlite$"))
(defvar ibooks-annot/AEAnnotation-DB (ibooks-db/get-sqlite IBOOKS-DATA "AEAnnotation/" "AEAnnotation.*\\.sqlite$"))

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

(defun ibooks-annot/book-highlights-color (num)
  (cond
   ((= num 1) "~")
   ((= num 2) "=")
   ((= num 3) "/")
   ((= num 4) "*")
   (t "")))

(defun ibooks-annot/pdf-highlights-color (num)
  (cond
   ((string= num "#fb5b89") "*")
   ((string= num "#69aff0") "=")
   ((string= num "#7cc867") "~")
   ((string= num "#f9cd59") "/")
   (t "")))

(defun ibooks-annot/book-note-exist-p (title)
  "Check if a book note with TITLE exists in the denote directory."
  (let ((note nil))
    (require 'denote nil t)
    (dolist (file (denote-directory-files))
      (if (string= title (denote-retrieve-title-value file 'org))
          (setq note file)))
    note))

(defun ibooks-annot/update-book-alist ()
  "Update ibooks-annot/book-alist with book titles and IDs from the database."
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
  "Get a list of cons cells containing file names and their extensions in the given DIRECTORY."
  (let ((files (directory-files directory nil "^[^.].*")))
    (mapcar (lambda (file)
              (cons (file-name-sans-versions file) (file-name-extension file)))
            files)))

(defun ibooks-annot/bookname-partial-match (str1 str2)
  "Check if str1 is a partial match of str2."
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
          (let ((symbol (ibooks-annot/book-highlights-color (nth 4 annot)))
                (text (cadr annot)))
            (insert (format "%s%s%s\n" symbol text symbol))))
        (ibooks-annot/write-to-note book-title book-note)))))

(defun ibooks-annot/extract-pdf-highlights (book-title book-note)
  "Extract PDF highlights for the given BOOK-TITLE."
  (let* ((book-path (ibooks-annot/get-book-path-in-cloud book-title))
         (parsed-json (shell-command-to-string
                       (format "%s %s %s" ibooks-annot/python-command pdfannots (shell-quote-argument book-path))))
         (highlights (ibooks-annot/parse-pdf-highlights parsed-json)))
    (ibooks-annot/write-highlights-to-note book-title book-note highlights)))

(defun ibooks-annot/parse-pdf-highlights (json-data)
  "Parse JSON data and return a list of highlights."
  (let ((highlights '()))
    (with-temp-buffer
      (let ((json-object-type 'hash-table))
        (mapc (lambda (entry)
                (setq highlights (cons (ibooks-annot/format-highlight entry) highlights)))
              (json-read-from-string json-data)))
      (nreverse highlights))))

(defun ibooks-annot/format-highlight (entry)
  "Format a single highlight entry."
  (let ((color (ibooks-annot/pdf-highlights-color (gethash "color" entry)))
        (text (gethash "text" entry)))
    (format "%s%s%s\n" color text color)))

(defun ibooks-annot/write-highlights-to-note (book-title book-note highlights)
  "Write highlights to the note for the given BOOK-TITLE."
  (with-temp-buffer
    (insert (format "* %s\n" ibooks-annot/book-note-highlights-heading))
    (dolist (highlight highlights)
      (insert highlight))
    (ibooks-annot/write-to-note book-title book-note)))

(defun ibooks-annot/write-to-note (book-title book-note)
  "Write the buffer content to a note or create a new one if needed."
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
            ((string= book-ext "pdf") (ibooks-annot/extract-pdf-highlights book-title book-note))))))

(provide 'ibooks-annot)
