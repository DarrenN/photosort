# Photosort

Very simple [Chicken Scheme](https://call-cc.org) program that copies jpeg images from one directory into a new directory with a date structure.

Only JPEGs will be moved, and only those with Exif data. This was written to bring some sense of order to my Dropbox "Camera Uploads" folder.

## Example

`$ photosort /tmp/directory-a /tmp/directory-b`

**Directory A**:

```
file1.jpg
file2.jpg
file3.jpg
```

**Directory B**:

Files will be checked for date information and moved into a structure like:

```
<YEAR>
 └── <MONTH>
     └── <FILENAME>
     └── ...
```

`YEAR` and `MONTH` are extracted from Exif tags on the photo. If none are present then we try to grab them from the filename, hoping that your filenames are something like `2021-05-01 10:45:21.jpg`.

### SQLite

This program also stores information about the moved photos in a SQLite DB within your `~/.cache` directory. This might be useful if you want a record of what moved where, etc.

### Refactor notes

Have a single dispatch function that manages a photo object, wrapping it in a tagged list

example:

```scheme
(define (dispatch (photo)
  (match photo
    [('BEGIN . xs) (dispatch (get-filename xs))]
    [('COPY . xs) (dispatch (copy-photo xs))]
    [('METADATA . xs) (dispatch (persist-metadata xs))]
    [('DONE . xs) (log-success xs)]
    [('ERROR . xs) (log-error xs)]
    [_ (log-fail photo)])))
```

`xs` should probably be a hash or a record (SRFI-9)?

Maybe a macro?

```scheme
(define-dispatch photo
  [begin -> get-filename]
  [copy -> copy-photo]
  [metadata -> persist-metadata]
  [success -> log-success]
  [error -> log-error]
  [else #f])

;; expands to:

(define (dispatch-photo-begin xs) (dispatch-photo (list 'begin xs)))
(define (dispatch-photo-copy xs) (dispatch-photo (list 'copy xs)))
(define (dispatch-photo-metadata xs) (dispatch-photo (list 'metadata xs)))
(define (dispatch-photo-success xs) (dispatch-photo (list 'success xs)))
(define (dispatch-photo-error xs) (dispatch-photo (list 'error xs)))

;; should we explicitly handle exceptions or should individual handlers?
(define (dispatch-photo (photo)
  (handle-exceptions exn
    (dispatch-photo (list 'exception ((condition-property-accessor 'exn 'message) exn)))
    (match photo
      [('begin . xs) (get-filename xs)]
      [('copy . xs) (copy-photo xs)]
      [('metadata . xs) (persist-metadata xs)]
      [('success . xs) (log-success xs)]
      [('error . xs) (log-error xs)]
      [_ ((lambda (x) #f) photo)]))))
```
