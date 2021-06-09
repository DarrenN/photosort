(import (chicken condition)
        (chicken format)
        (chicken io)
        (chicken process-context)
        (chicken string)
        (chicken syntax)
        defstruct
        json
        matchable
        srfi-1
        srfi-19-core
        srfi-19-io
        srfi-69)

(import-for-syntax (chicken string)
                   srfi-13)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Logging

(define TRACE "TRACE")
(define DEBUG "DEBUG")
(define INFO  "INFO")
(define WARN  "WARN")
(define ERROR "ERROR")
(define FATAL "FATAL")

(define *default-log-level* (make-parameter INFO))
(define *default-log-stream* (make-parameter (current-output-port)))

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

(define (%log p fmt r)
  (let ((out (if (port? p) p (current-error-port))))
    (apply fprintf p fmt r)
    (apply fprintf p "\n" '())))

;; ISO 8601 of the current date/time
(define (timestamp)
  (date->string (current-date) "~4"))

(defstruct photo filename)

(define verbose? #f)

(define-syntax wrap-debug
  (syntax-rules ()
    ((_ fn var)
     (begin
       (when verbose?
         (log-debug 'msg (format "Proc ~A called with ~A" 'fn var)))
       (fn var)))))

(define-syntax define-dispatch
  (er-macro-transformer
   (lambda (exp rename compare)
     (let* ((dispatch-id (cadr exp))
            (new-id (string->symbol
                     (conc "dispatch-" (symbol->string dispatch-id))))
            (%define (rename 'define))
            (%dispatch (rename 'dispatch)))
       `(,%define (,new-id p) (,%dispatch (list ',dispatch-id p)))))))

(define-dispatch begin)
(define-dispatch success)
(define-dispatch error)

(define (stub p)
  (log-debug 'msg (format "Stub called with ~A" p)))

(define (dispatch photo)
  (when verbose?
    (log-debug 'msg (format "Dispatch (dispatch ~A)" photo)))
  (handle-exceptions
      exn
      (dispatch
       (list 'error
             ((condition-property-accessor 'exn 'message) exn)))
    (match photo
      [('begin (? string? f)) (wrap-debug stub f)]
      [('success (? photo? p)) (wrap-debug stub p)]
      [('error . xs) (wrap-debug stub xs)]
      [_ (log-error 'msg (format "No match for ~A" photo))])))

(log-fatal 'msg "shit went down!" 12 "number" "foo" "bar" '() '(1 2 3))

(define p (make-photo "filename.jpg"))
(dispatch-begin "filename.jpg")
(dispatch-error "oops had an error")
(dispatch-success p)
(dispatch-begin 112)

;; (fprintf (current-error-port) "~A" (list->alist '(1 2 3 4 5 "a" "b" "c")))
