(import (chicken condition)
        (chicken format)
        (chicken process-context)
        (chicken string)
        (chicken syntax)
        defstruct
        json
        matchable)

(import-for-syntax (chicken string))

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


(defstruct photo filename)

(define verbose? #f)

(define-syntax wrap-debug
  (syntax-rules ()
    ((_ fn var)
     (begin
       (when verbose?
         (log-debug "Proc ~A called with ~A" 'fn var))
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
  (log-debug "Stub called with ~A" p))

(define (dispatch photo)
  (when verbose?
    (log-debug "Dispatch (dispatch ~A)" photo))
  (handle-exceptions
      exn
      (dispatch
       (list 'error
             ((condition-property-accessor 'exn 'message) exn)))
    (match photo
      [('begin (? string? f)) (wrap-debug stub f)]
      [('success (? photo? p)) (wrap-debug stub p)]
      [('error . xs) (wrap-debug stub xs)]
      [_ (log-err "No match for ~A" photo)])))

(define p (make-photo "filename.jpg"))
(dispatch-begin "filename.jpg")
(dispatch-error "oops had an error")
(dispatch-success p)
(dispatch-begin 112)

