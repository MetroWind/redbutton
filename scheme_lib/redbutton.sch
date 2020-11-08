(define (path-join . segs)
  (if (null? (cdr segs))
      (car segs)
      (path-join2 (car segs) (apply path-join (cdr segs)))))

(define (walk-dir dir process)
  (map (lambda (f)
         (let* ((path (path-join dir f))
                (type (file-type path)))
           (cond ((= type 'file) (process dir f))
                 ((= type 'dir) (walk-dir path process)))))
       (list-dir dir)))

;; (list-split-common (list 0 1 2 3) (list 0 1 3))
;; ==> '((0 1) (2 3) . (3))
(define (list-split-common xs ys)
  (define (list-split-common-inner common xs ys)
    (if (or (null? xs) (null? ys))
        (cons (reverse common) (cons xs ys))
        (if (= (car xs) (car ys))
            (list-split-common-inner (cons (car xs) common) (cdr xs) (cdr ys))
            (cons (reverse common) (cons xs ys)))))
  (list-split-common-inner '() xs ys))

(define (path-replace-prefix path from to)
  (apply path-join
   (append (path-split to)
           (list-tail (path-split path) (length (path-split from))))))

;; This always replaces dest.
(define (link-files dir-from dir-to)
  (define dir-from-abs (path-canonicalize dir-from))
  (walk-dir
   dir-from-abs
   (lambda (dir file)
     (let* ((src-file (path-join dir file))
            (dest-dir (path-replace-prefix dir dir-from-abs dir-to))
            (dest-file (path-join dest-dir file)))
       (if (file-exists? dest-file)
           (file-remove dest-file))
       (if (not (file-exists? dest-dir))
           (mkdir dest-dir))
       ;; (display (format "%{src} --> %{dest}\n"
       ;;                  (cons "src" src-file) (cons "dest" dest-file)))
       (link-file src-file dest-file)))))

(define (append-git-conf filename section . key-values)
  (file-append filename (format "[%{sec}]\n" (cons "sec" section)))
  (for-each key-values
            (lambda (kv)
              (file-append filename (format "%{key} = %{value}\n"
                                            (cons "key" (car kv))
                                            (cons "value" (cdr kv)))))))
