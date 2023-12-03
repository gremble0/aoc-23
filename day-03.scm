(define day-03-input (with-input-from-file "day-03.in"
  (lambda ()
    (let loop ((lines '())
               (next-line (read-line)))
       (if (eof-object? next-line)
           (reverse lines)
           (loop (cons next-line lines)
                 (read-line)))))))

(define (schematic-symbol? char)
  (not (or (eq? char #\.)
           (char-numeric? char))))

(define (neighbouring-schematic-symbol? line-x line-y schematic)
  (let ((rows (length schematic))
        (cols (string-length (car schematic))))
    (display rows) (newline) (display cols) (newline)
    (or (and (> line-x 0)
             (> line-y 0)
             (schematic-symbol? (string-ref (list-ref schematic (- line-x 1)) (- line-y 1)))) ;; TODO: out of bounds
        (and (> line-y 0)
             (schematic-symbol? (string-ref (list-ref schematic line-x) (- line-y 1))))
        (and (< (+ line-x 1) rows)
             (> line-y 0)
             (schematic-symbol? (string-ref (list-ref schematic (+ line-x 1)) (- line-y 1))))
        (and (> line-x 0)
             (schematic-symbol? (string-ref (list-ref schematic (- line-x 1)) line-y)))
        (and (< (+ line-x 1) rows)
             (schematic-symbol? (string-ref (list-ref schematic (+ line-x 1)) line-y)))
        (and (< (+ line-x 1) rows)
             (< (+ line-y 1) cols)
             (schematic-symbol? (string-ref (list-ref schematic (+ line-x 1)) (+ line-y 1))))
        (and (< (+ line-y 1) cols)
             (schematic-symbol? (string-ref (list-ref schematic line-x) (+ line-y 1))))
        (and (< (+ line-x 1) rows)
             (< (+ line-y 1) cols)
             (schematic-symbol? (string-ref (list-ref schematic (- line-x 1)) (+ line-y 1)))))))

(neighbouring-schematic-symbol? 2 8 day-03-input)
(string-ref (list-ref day-03-input 2) 8)
(string-length (car day-03-input))
(length day-03-input)

(schematic-symbol? (string-ref (list-ref schematic line-y) (+ line-x 1)))

(define (filter-numbers schematic)
  (define (filter-numbers-line line)
    (define (filter-numbers-line-impl line line-x line-y cur-num include-cur-num? acc)
      (display cur-num) (newline)
      (cond ((null? line) acc)
            ((char-numeric? (car line))
             ;; TODO: check if curnum already is tagged to optimize
             (if (neighbouring-schematic-symbol? line-x line-y schematic)
               (filter-numbers-line-impl (cdr line) (+ line-x 1) line-y (string cur-num (car line)) #t acc)
               (filter-numbers-line-impl (cdr line) (+ line-x 1) line-y (string cur-num (car line)) include-cur-num? acc)))
            (else
              (if (or (null? cur-num)
                      (not include-cur-num?))
               (filter-numbers-line-impl (cdr line) (+ line-x 1) line-y '() #f acc)
               (filter-numbers-line-impl (cdr line) (+ line-x 1) line-y '() #f (cons (string->number cur-num) acc))))))
    (filter-numbers-line-impl (string->list line) 1 2 '() #f '()));; todo out of bounds
  (map filter-numbers-line schematic))

(filter-numbers day-03-input)
