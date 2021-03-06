(import (chicken condition)
        (chicken format)
        (chicken io)
        (chicken process-context)
        (chicken string)
        (chicken syntax)
        (clojurian syntax)
        defstruct
        json
        matchable
        srfi-1
        srfi-19-core
        srfi-19-io
        srfi-69)

(import-for-syntax (chicken string)
                   srfi-13)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;; ISO 8601 of the current UTC date/time w nanoseconds
(define (timestamp)
  (date->string
   (current-date (utc-timezone-locale)) "~Y-~m-~dT~H:~M:~S.~N~z"))

(defstruct photo filename)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dispatching

(define verbose? #t)

(define-syntax wrap-debug
  (syntax-rules ()
    ((_ fn var)
     (begin
       (when verbose?
         (log-debug 'proc 'fn 'msg (format "args: ~A" var)))
       (fn var)))))

  (handle-exceptions exn
      (begin
	(log-warn 'msg ((condition-property-accessor 'exn 'message) exn))
        #f)
    (copy-file path target-path))

;; Guard functions passed to and->
;; Ensures first value input to fn is pred? or returns #f
;; Handles exception by logging a warning and returns #f
(define-syntax guard/and->
  (syntax-rules ()
    ((_ val pred? fn args ...)
     (handle-exceptions exn
         (begin
	   (log-warn 'type "exception"
                     'msg ((condition-property-accessor 'exn 'message) exn))
           #f)
         (if (pred? val)
             (apply fn (append (list val) (list args ...)))
             #f)))))

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

(define (stub p . args)
  (log-debug 'msg (format "Stub called with ~A" p) 'args args)
  p)

(define (raise p . args)
  (signal (make-property-condition 'exn 'message "Ahhh")))

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

;; (log-fatal 'msg "shit went down!" 12 "number" "foo" "bar" '() '(1 2 3))

;; (define p (make-photo "filename.jpg"))
;; (dispatch-begin "filename.jpg")
;; (dispatch-error "oops had an error")
;; (dispatch-success p)
;; (dispatch-begin 112)

(print
 (and-> (make-photo "ANS->.jpg")
        (guard/and-> photo? stub 1 2)
        (guard/and-> photo? stub)
        (guard/and-> photo? raise 3 4)))

;; (fprintf (current-error-port) "~A" (list->alist '(1 2 3 4 5 "a" "b" "c")))
