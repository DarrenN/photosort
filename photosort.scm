(import (chicken condition)
        (chicken file)
        (chicken file posix)
        (chicken format)
        (chicken io)
        (chicken irregex)
        (chicken pathname)
        (chicken platform)
        (chicken process-context)
        (chicken string)
        (chicken syntax)
        (clojurian syntax)
        defstruct
        exif
        image-dimensions
        json
        list-utils
        simple-sha1
        sql-de-lite
        srfi-1
        srfi-13
        srfi-14
        srfi-19-core
        srfi-19-io
        srfi-69)

(import-for-syntax (chicken string)
                   srfi-13)

;; 1. pull filenames from Camera Uploads dir
;; 2. use simple-sha1, exif and image-dimensions to get the sha1sum,
;;    date-time and dimensions
;; 3. split the datetime into '(year month day)
;; 4. if TARGET/<year> folder not exists, make it
;; 5. if TARGET/<year>/<month> folder not exists, make it
;; 6. Copy file into TARGET/<year>/<month> as is
;; 7. Add row in sqlite DB with: sha, filename, new path, datetime,
;;    width, height, type

(cond-expand
  (compiling
   (define (compiled?) #t))
  (else
   (define (compiled?) #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Logging

(define TRACE "TRACE")
(define DEBUG "DEBUG")
(define INFO  "INFO")
(define WARN  "WARN")
(define ERROR "ERROR")
(define FATAL "FATAL")

(define *default-log-level* (make-parameter INFO))
(define *default-log-stream* (make-parameter (current-error-port)))

(define (%json-log p h)
  (let ((out (if (port? p) p (*default-log-stream*)))
        (o (if (hash-table? h) h (make-hash-table eq? symbol-hash))))
    (hash-table-set! o 'timestamp (timestamp))
    (hash-table-update!/default o 'level identity (*default-log-level*))
    (json-write o out)
    (write-line "" out)))

(define (->symbol x)
  (cond [(symbol? x) x]
        [(string? x) (string->symbol x)]
        [(number? x) (string->symbol (number->string x))]
        [else (gensym "invalid-field")]))

;; TODO: add checks for proper length
(define (list->hash l)
  (define (inner ps l)
    (if (null? l)
        (alist->hash-table ps #:test eq? #:hash symbol-hash)
        (inner
         (append ps (list (cons (->symbol (car l)) (cadr l)))) (drop l 2))))
  (inner '() l))

(define-syntax define-logger
  (er-macro-transformer
   (lambda (exp rename compare)
     (let* ((level-id (cadr exp))
            (level-label (string-upcase (symbol->string level-id)))
            (proc-id (string->symbol
                      (conc "log-" (symbol->string level-id))))
            (%define (rename 'define))
            (%let (rename 'let))
            (%hash-table-set! (rename 'hash-table-set!))
            (%json-log (rename '%json-log))
            (%list->hash (rename 'list->hash)))
       `(,%define (,proc-id . kvs)
                  (,%let ((h (,%list->hash kvs)))
                         (,%hash-table-set! h 'level ,level-label)
                         (,%json-log '() h)))))))

(define-logger trace)
(define-logger info)
(define-logger debug)
(define-logger warn)
(define-logger error)
(define-logger fatal)

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

(define (step6-persist-to-db photo)
  (if (not
       (null?
        (query fetch
               (sql (photo-db photo) "SELECT hash FROM photos WHERE hash = ?;")
               (photo-sha1 photo))))
      (update-photo photo)
      (insert-photo photo)))

(define (insert-photo photo)
  (let* ((tags (photo-exif-tags photo))
         (make (guard-false (aget 'make tags)))
         (model (guard-false (aget 'model tags)))
         (datetime (aget 'date-time tags))
         (info (photo-info photo))
         (width (if info (cadr info) 0))
         (height (if info (caddr info) 0))
         (type (if info (symbol->string (car info)) "")))

    (exec (sql (photo-db photo) "INSERT OR IGNORE INTO photos(hash, filename,
original_path, new_path, filesize, width, height, type, make, model,
datetime) values(?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?);")
          (photo-sha1 photo)        ; hash
          (photo-filename photo)    ; filename
          (photo-input-dir photo)   ; original_path
          (photo-output-path photo) ; new_path
          (photo-bytes photo)       ; filesize
          width
          height
          type
          make
          model
          (photo-date photo))       ; datetime (ISO8601)
    photo))

;; If we're copying the same file somewhere, then just update the paths
;; The sha1 hash would be the same if the file is unmodified.
(define (update-photo photo)
  (exec (sql (photo-db photo)
             "UPDATE photos SET filename = ?, original_path = ?,
new_path = ? WHERE hash = ?;")
        (photo-filename photo)
        (photo-input-dir photo)
        (photo-output-path photo)
        (photo-sha1 photo))
  photo)

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

;; Convert "YYYY:MM:DD HH:MM:SS" to '("YYYY" "MM" "DD" ...)
;; for use in ensure-dir
(define (date-string->list dt)
  (if (not dt)
      (datetime->list "1970:01:01 00:00:00")
      (string-tokenize (car (string-tokenize dt)) char-set:digit)))

;; Convert "YYYY:MM:DD HH:MM:SS" to ISO 8601 string
(define (exif-datetime->iso8601 dt)
  (let* ((tokens (string-tokenize dt char-set:digit))
         (date (take tokens 3))
         (time (drop tokens 3)))
    (string-append (string-join date "-") "T" (string-join time ":") ".000Z")))

(define (get-iso8601-datetime filename dt)
  (if (not dt)
      (filename->iso8601 filename)
      (datestring->iso8601 dt)))

;; Convert "YYYY-MM-DD HH.MM.SS-x-x.jpg" to "YYYY-MM-DD HH:MM:SS.000"
(define (filename->iso8601 filename)
  (let* ((tokens (string-tokenize filename))
         (date (string-tokenize (car tokens) char-set:digit))
         (time (take (string-tokenize (cadr tokens) char-set:digit) 3)))
    (string-append (string-join date "-") "T " (string-join time ":") ".000Z")))

;; 2021:02:13 16:18:41
;; Convert "YYYY:MM:DD HH:MM:SS" to "YYYY-MM-DD HH:MM:SS.000"
(define (datestring->iso8601 dt)
  (let* ((tokens (string-tokenize dt))
         (date (string-tokenize (car tokens) char-set:digit)))
    (string-append (string-join date "-") " " (cadr tokens) ".000")))

;; Converts epoch seconss to iso8601
(define (seconds->iso8601 n)
  (date->string
   (seconds->date n #f) "~Y-~m-~dT~H:~M:~S.000Z"))

;; ISO 8601 of the current UTC date/time w nanoseconds
(define (timestamp)
  (date->string
   (current-date (utc-timezone-locale)) "~Y-~m-~dT~H:~M:~S.~N~z"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Image file handling

;; Wraps copy-file with an exception handler to cover an already
;; existing file (we don't clobber). If the file exists at target-path
;; then we log a warning and return the file-size of image at
;; target-path. We do this so we can update any existing records in
;; the DB based on the sha1 of the file.
;;
;; If file-size throws an exception then we log that
;; warning and return #f, which will short circuit the pipeline.
(define (copy-image path target-path)
  (handle-exceptions exn
      (let ((msg ((condition-property-accessor 'exn 'message) exn)))
        (if (equal? "newfile exists but clobber is false" msg)
            (begin
	      (log-warn 'proc "copy-image"
                        'path path
                        'targetPath target-path
                        'type "exception"
                        'msg msg)
              (file-size target-path))
            (begin (log-warn 'proc "file-size"
                        'path path
                        'targetPath target-path
                        'type "exception"
                        'msg msg)
                   #f)))
    (copy-file path target-path)))

(define (aget k a)
  (if (or (not a) (null? a))
      #f
      (let ((v (assq k a)))
        (if (null? v)
            #f
            (cdr v)))))

(define (extract-tags photo)
  (let ((tags (tag-alist-from-file (photo-filename photo)
                                   '(model make date-time))))
    (when tags
      (photo-exif-tags-set! photo tags))
    photo))

;; image-info will throw unexpected EOF errors which we need to handle
(define (get-image-info photo)
  (handle-exceptions
      exn
      (begin
        (log-warn 'proc "get-image-info"
                  'msg
                  (format "Couldn't get image-info for ~A ~A"
                          (photo-filename photo)
                          ((condition-property-accessor 'exn 'message) exn)))
        photo)
    (let ((info (call-with-input-file (photo-filename photo) image-info)))
      (when info
        (photo-info-set! photo info))
      photo)))

;; file-modification-time will also throw if it can't find the file, which we
;; should have caught already, so let it bubble up
(define (get-image-mtime photo)
  (let ((mtime (file-modification-time (photo-filename photo))))
      (when mtime
        (photo-mtime-set! photo mtime))
      photo))

(define (step2-get-metadata photo)
  (get-image-mtime (get-image-info (extract-tags photo))))

(define re-filename-date
  (sre->irregex
   '(seq (= 4 numeric) "-" (= 2 numeric) "-" (= 2 numeric) space
         (= 2 numeric) (or "." ":") (= 2 numeric) (or "." ":") (= 2 numeric)
         "." (or "jpg" "jpeg"))))

;; Inspect the file to determine the best possible date
(define (step3-get-filedate photo)
  (let ((og-filename (pathname-strip-directory (photo-filename photo)))
        (date-time (aget 'date-time (photo-exif-tags photo)))
        (mtime (photo-mtime photo)))

    ;; Now we need to work our way through the following:
    ;; - if DT then make an ISO8601 filename
    ;; - else if we can parse a date from filename, use that
    ;; - else convert mtime seconds to ISO8601 filename
    (define filedate
      (cond [date-time (exif-datetime->iso8601 date-time)]
            [(irregex-match re-filename-date og-filename)
             (filename->iso8601 og-filename)]
            [mtime (seconds->iso8601 mtime)]
            [else "1970-01-01T00:00:00.000Z"]))
    (photo-date-set! photo filedate)
    photo))

(define (step4-copy-photo photo)
  (let* ((og-filename (pathname-strip-directory (photo-filename photo)))
         (target-dir
          (ensure-dir
           (photo-output-dir photo) (date-string->list (photo-date photo))))
         (target-path (normalize-pathname
                       (string-append target-dir "/" og-filename)))
         (bytes-copied (copy-image (photo-filename photo) target-path)))
    (photo-output-path-set! photo target-path)
    (if bytes-copied
        (begin
          (photo-bytes-set! photo bytes-copied)
          photo)
        (log-error-and-false "step4-copy-photo"
                             (format "Couldn't copy ~A" target-path)))))


;; Calculate the SHA1 of the copied image file. We use this as a unique
;; id in the SQLite DB
(define (step5-get-hash photo)
  (let ((sha (sha1sum (photo-output-path photo))))
    (photo-sha1-set! photo sha)
    photo))

;; We're only interested in certain types of files
;; primarily JPGs Maybe mp4 / mov?
(define (is-jpeg? path)
  (let ((ext (pathname-extension path)))
    (if (and ext
             (or (string= ext "jpg")
                 (string= ext "jpeg")))
        #t
        #f)))

(define (step1-check-file-path p)
  (let ((f (photo-filename p)))
    (if (and f (file-exists? f))
        p
        (log-error-and-false "step1-check-file-path"
                             (format "~A is not a valid file" f)))))

(define (log-error-and-false proc msg)
  (log-error 'proc proc
             'msg msg)
  #f)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Structs

(defstruct photo
  db filename input-dir output-dir output-path exif-tags info mtime date bytes
  sha1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Processing Pipeline

(define verbose? #f)

;; Creates a handler function with db in out bound to the returned closure.
;; This is specifically to be passed to the find-files #:action argument
;; which passes in a path string (path) and a previous value (prev).
;; The closure creates a photo? record and hands off to process-photo pipeline.
(define ((handle-photo db in out) path prev)
  (process-photo
   (make-photo db: db
               input-dir: in
               output-dir: out
               filename: path)))

;; Guard functions passed to and->
;; Ensures first value input to fn is pred? or returns #f
;; Handles exception by logging a warning and returns #f
(define-syntax guard/and->
  (syntax-rules ()
    ((_ val pred? fn args ...)
     (handle-exceptions exn
         (begin
	   (log-warn 'type "exception"
                     'guardedProc (format "~A" fn)
                     'value (format "~A" val)
                     'proc "guard/and->"
                     'msg ((condition-property-accessor 'exn 'message) exn))
           #f)
         (if (pred? val)
             (apply fn (append (list val) (list args ...)))
             #f)))))

;; Passes photo? through an and-> pipeline of guarded functions
(define (process-photo photo)
  (when verbose?
    (log-debug 'proc "process-photo"
               'msg (format "received ~A" photo)))
  (let ((result (and-> photo
                       (guard/and-> photo? step1-check-file-path)
                       (guard/and-> photo? step2-get-metadata)
                       (guard/and-> photo? step3-get-filedate)
                       (guard/and-> photo? step4-copy-photo)
                       (guard/and-> photo? step5-get-hash)
                       (guard/and-> photo? step6-persist-to-db))))
    (unless result
      (log-warn 'proc "process-photo"
                'msg (format "Couldn't process ~A" (photo-filename photo))))))

;; Accepts two command line args:
;;
;; input-dir - directory to scan for files
;; output-dir - directory to copy new file structure to
;;
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
(define (main args)
  (define-values
      (input-dir output-dir)
      (values
       (car args)
       (cadr args)))
  (if (and (directory-exists? input-dir)
           (directory-exists? output-dir))
      (begin
        (ensure-cache-dir)
        (ensure-db)
        (let* ((db (get-db-handle))
               (handler (handle-photo db input-dir output-dir)))
          (find-files input-dir
                      #:test is-jpeg?
                      #:action handler)
          (close-database db)))
      (log-error 'msg (format "input: ~A output: ~A" input-dir output-dir))))

;; We need to explicitly call main when this has been compiled
(when (compiled?)
  (main (command-line-arguments)))
