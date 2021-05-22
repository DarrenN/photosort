(import (chicken condition)
        (chicken file)
        (chicken format)
        (chicken pathname)
        (chicken platform)
        (chicken process-context)
        exif
        image-dimensions
        list-utils
        simple-sha1
        sql-de-lite
        srfi-1
        srfi-13
        srfi-14
        srfi-69)

;; 1. pull filenames from Camera Uploads dir
;; 2. use simple-sha1, exif and image-dimensions to get the sha1sum,
;;    date-time and dimensions
;; 3. split the datetime into '(year month day)
;; 4. if TARGET/<year> folder not exists, make it
;; 5. if TARGET/<year>/<month> folder not exists, make it
;; 6. Copy file into TARGET/<year>/<month> as is
;; 7. Add row in sqlite DB with: sha, filename, new path, datetime,
;;    width, height, type

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Logging

(define (%log p fmt r)
  (let ((out (if (port? p) p (current-error-port))))
    (apply fprintf p fmt r)
    (apply fprintf p "\n" '())))

(define (log-err fmt #!rest r #!key (p (current-error-port)))
  (%log p (string-append "ERR: " fmt) r))

(define (log-debug fmt #!rest r #!key (p (current-output-port)))
  (%log p (string-append "DEBUG: " fmt) r))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cache filepaths

(define CACHE-DIR-TOP (system-cache-directory))
(define CACHE-DIR-SUB
  (normalize-pathname (string-append CACHE-DIR-TOP "/" "photosort")))
(define CACHE-DB
  (normalize-pathname (string-append CACHE-DIR-SUB "/" "photos.db")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SQLite

(define CREATE-TABLE-PHOTOS #<<SQL
  CREATE TABLE IF NOT EXISTS photos
  (hash TEXT NOT NULL PRIMARY KEY,
        filename TEXT NOT NULL,
        original_path TEXT NOT NULL,
        new_path TEXT NOT NULL,
        filesize INTEGER NOT NULL,
        width INTEGER,
        height INTEGER,
        type TEXT,
        make TEXT,
        model TEXT,
        datetime TEXT NOT NULL)
SQL
)

(define (ensure-db)
  (let* ((db (open-database CACHE-DB))
         (stmt (prepare db CREATE-TABLE-PHOTOS)))
    (step stmt)))

(define (get-db-handle)
  (open-database CACHE-DB))

(define (guard-false v)
  (if (equal? v #f) "" v))

;; If the hash is present, then we update, else insert new row
(define (persist-photo db row)
  (if (not
       (null? (query fetch (sql db "SELECT hash FROM photos WHERE hash = ?;")
                     (hash-table-ref/default row 'hash ""))))
      (update-photo db row)
      (insert-photo db row)))

(define (insert-photo db row)
  ;; '(info path target-path filename bytes-copied exif-tags)
  ;; (png 10 20 0) type w h rot
  (let* ((tags (hash-table-ref row 'exif-tags))
         (make (guard-false (cdr (assq 'make tags))))
         (model (guard-false (cdr (assq 'model tags))))
         (datetime (cdr (assq 'date-time tags)))
         (filename (hash-table-ref row 'filename))
         (info (hash-table-ref row 'info)))

    (exec (sql db "INSERT OR IGNORE INTO photos(hash, filename,
original_path, new_path, filesize, width, height, type, make, model,
datetime) values(?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?);")
          (hash-table-ref row 'hash)
          filename
          (hash-table-ref row 'path)
          (hash-table-ref row 'target-path)
          (hash-table-ref/default row 'bytes-copied 0)
          (cadr info)
          (caddr info)
          (symbol->string (car info))
          make
          model
          (get-iso8601-datetime filename datetime))))

;; If we're copying the same file somewhere, then just update the paths
;; The sha1 hash would be the same if the file is unmodified.
(define (update-photo db row)
    (exec (sql db "UPDATE photos SET filename = ?, original_path = ?,
new_path = ? WHERE hash = ?;")
          (hash-table-ref row 'filename)
          (hash-table-ref row 'path)
          (hash-table-ref row 'target-path)
          (hash-table-ref row 'hash)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Filesystem Operations

;; Create a location for the SQLite DB
(define (ensure-cache-dir)
  (when (not (directory-exists? CACHE-DIR-TOP))
    (create-directory CACHE-DIR-TOP))
  (when (not (directory-exists? CACHE-DIR-SUB))
    (create-directory CACHE-DIR-SUB)))

(define dircache (make-hash-table))

;; Instead of checking directory-exists? for every image store
;; directory paths in a lookup table. Caches on the next check
;; after a directory is created.
(define (cached-create-directory path)
  (let ((dir? (hash-table-ref/default dircache path #f)))
    (when (not dir?)
      (if (directory-exists? path)
          (hash-table-set! dircache path #t)
          (create-directory path)))
    path))

;; Ensure we have the correct directories setup
(define (ensure-dir path date)
  (let* ((year (car date))
         (month (cadr date))
         (year-dir (normalize-pathname (string-append path "/" year)))
         (month-dir (normalize-pathname (string-append year-dir "/" month))))
    (cached-create-directory year-dir)
    (cached-create-directory month-dir)
    month-dir))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Datetime utils

(define (get-datetime filename dt)
  (if dt
      (datetime->list dt)
      (datetime->list filename)))

;; Convert "YYYY:MM:DD HH:MM:SS" to '("YYYY" "MM" "DD")
(define (datetime->list dt)
  (if (not dt)
      (datetime->list "1970:01:01 00:00:00")
      (string-tokenize (car (string-tokenize dt)) char-set:digit)))

(define (get-iso8601-datetime filename dt)
  (if (not dt)
      (filename->iso8601 filename)
      (datetime->iso8601 dt)))

;; Convert "YYYY-MM-DD HH.MM.SS-x-x.jpg" to "YYYY-MM-DD HH:MM:SS.000"
(define (filename->iso8601 filename)
  (let* ((tokens (string-tokenize filename))
         (date (string-tokenize (car tokens) char-set:digit))
         (time (take (string-tokenize (cadr tokens) char-set:digit) 3)))
    (string-append (string-join date "-") " " (string-join time ":") ".000")))

;; 2021:02:13 16:18:41
;; Convert "YYYY:MM:DD HH:MM:SS" to "YYYY-MM-DD HH:MM:SS.000"
(define (datetime->iso8601 dt)
  (let* ((tokens (string-tokenize dt))
         (date (string-tokenize (car tokens) char-set:digit)))
    (string-append (string-join date "-") " " (cadr tokens) ".000")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Image file handling

;; Wraps copy-file with an exception handler to cover an already
;; existing file (we don't clobber)
(define (copy-image path target-path)
  (handle-exceptions exn
      (begin
	(log-err ((condition-property-accessor 'exn 'message) exn))
        #f)
    (copy-file path target-path)))

;; Get a SHA1 hash of the copied file and insert metadata into DB
(define (create-entry db opts)
  (let ((sha (sha1sum (hash-table-ref opts 'target-path))))
    (hash-table-set! opts 'hash sha)
    (persist-photo db opts)))

;; Pulls EXIF data from image and if present attempts to copy to a
;; directory tree in the target-dir shaped like:
;;
;; <YEAR>
;; └── <MONTH>
;;     └── <FILENAME>
;;     └── ...
;;
;; Then insert into the SQLite table keye on SHA1 hash.
;;
;; If the file can't be copied send a message to stderr.
;;
(define (process-image db path out)
  (let ((tags (tag-alist-from-file path '(model make date-time))))
    (if tags
        (let* ((filename (pathname-strip-directory path))
               (info (call-with-input-file path image-info))
               (date (get-datetime filename (cdr (assq 'date-time tags))))
               (dir (ensure-dir out date))
               (target-path (normalize-pathname
                             (string-append dir "/" filename)))
               (exif-tags tags)
               (bytes-copied (copy-image path target-path))
               (opts (make-hash-table)))

          (hash-table-set! opts 'path path)
          (hash-table-set! opts 'target-path target-path)
          (hash-table-set! opts 'filename filename)
          (hash-table-set! opts 'bytes-copied bytes-copied)
          (hash-table-set! opts 'exif-tags tags)
          (hash-table-set! opts 'info info)

          (if bytes-copied              
              (create-entry db opts)
              (log-err "couldn't copy ~A" filename))))
        (log-err "no exif for ~A" path))
    #t)

(define ((process-dir db in out) path prev)
  (if (file-exists? path)
      (process-image db path out)
      (begin
        (log-err "file doesn't exist: ~A" path)
        #f)))

;; We're only interested in certain types of files
;; primarily JPGs Maybe mp4 / mov?
(define (is-jpeg? path)
  (let ((ext (pathname-extension path)))
    (if (and ext
             (or (string= ext "jpg")
                 (string= ext "jpeg")))
        #t
        #f)))

;; Accepts two command line args:
;;
;; input-dir - directory to scan for files
;; output-dir - directory to copy new file structure to
;;
(define (main args)
  (define-values
      (input-dir output-dir)
      (values
       (car (command-line-arguments))
       (cadr (command-line-arguments))))
  (if (and (directory-exists? input-dir)
           (directory-exists? output-dir))
      (begin
        (ensure-cache-dir)
        (ensure-db)
        (let ((db (get-db-handle)))
          (find-files input-dir
                      #:test is-jpeg?
                      #:action (process-dir db input-dir output-dir))
          (close-database db)))
      (log-err "input: ~A output: ~A" input-dir output-dir)))


