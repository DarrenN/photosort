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
  (hash TEXT NOT NULL UNIQUE,
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

;; Convert "YYYY:MM:DD HH:MM:SS" to '("YYYY" "MM" "DD")
(define (datetime->list dt)
  (if (not dt)
      (datetime->list "1970:01:01 00:00:00")
      (string-tokenize (car (string-tokenize dt)) char-set:digit)))

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
(define (create-entry opts)
  (let* ((target-path (alist-ref 'target-path opts))
         (bytes-copied (alist-ref 'bytes-copied opts))
         (info (alist-ref 'info opts))
         (path (alist-ref 'path opts))
         (filename (alist-ref 'filename opts))
         (sha (sha1sum target-path)))
    (log-debug "sha1: ~A" sha)
    (log-debug "copied-path: ~A bytes: ~A" target-path bytes-copied)))

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
(define (process-image path out)
  (let ((tags (tag-alist-from-file path '(model make date-time))))
    (if tags
        (let* ((info (call-with-input-file path image-info))
               (date (datetime->list (cdr (assq 'date-time tags))))
               (dir (ensure-dir out date))
               (filename (pathname-strip-directory path))
               (target-path (normalize-pathname
                             (string-append dir "/" filename)))
               (exif-tags tags)
               (bytes-copied (copy-image path target-path)))

          (if bytes-copied
              (create-entry
               (zip-alist
                '(info path target-path filename bytes-copied exif-tags)
                `(,info ,path ,target-path ,filename ,bytes-copied ,exif-tags)))
              (log-err "couldn't copy ~A" filename)))
        (log-err "no exif for ~A" path))
    #t))

(define ((process-dir in out) path prev)
  (if (file-exists? path)
      (process-image path out)
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
        (find-files input-dir
                    #:test is-jpeg?
                    #:action (process-dir input-dir output-dir)))
      (log-err "input: ~A output: ~A" input-dir output-dir)))


