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

(schematic-symbol? #\=)

(define (neighbouring-schematic-symbol? line-x line-y schematic)
  (let ((rows (length schematic))
        (cols (string-length (car schematic))))
    (or (and (> line-x 0)
             (> line-y 0)
             (schematic-symbol? (string-ref (list-ref schematic (- line-x 1)) (- line-y 1))))
        (and (> line-y 0)
             (schematic-symbol? (string-ref (list-ref schematic line-x) (- line-y 1))))
        (and (< (+ line-x 1) cols)
             (> line-y 0)
             (schematic-symbol? (string-ref (list-ref schematic (+ line-x 1)) (- line-y 1))))
        (and (> line-x 0)
             (schematic-symbol? (string-ref (list-ref schematic (- line-x 1)) line-y)))
        (and (< (+ line-x 1) cols)
             (schematic-symbol? (string-ref (list-ref schematic (+ line-x 1)) line-y)))
        (and (> line-x 0)
             (< (+ line-y 1) rows)
             (schematic-symbol? (string-ref (list-ref schematic (- line-x 1)) (+ line-y 1))))
        (and (< (+ line-y 1) rows)
             (schematic-symbol? (string-ref (list-ref schematic line-x) (+ line-y 1))))
        (and (< (+ line-x 1) cols)
             (< (+ line-y 1) rows)
             (schematic-symbol? (string-ref (list-ref schematic (+ line-x 1)) (+ line-y 1)))))))

(define (filter-numbers schematic)
  (define (filter-numbers-line line line-x)
    (define (filter-numbers-line-impl line line-y cur-num include-cur-num? acc)
      (cond ((null? line) acc)
            ((char-numeric? (car line))
             ;; TODO: check if curnum already is tagged to optimize
             (if (neighbouring-schematic-symbol? line-x line-y schematic)
               (if (null? (cdr line))
                 (+ (string->number (string cur-num (car line))) acc)
                 (filter-numbers-line-impl (cdr line) (+ line-y 1) (string cur-num (car line)) #t acc))
               (filter-numbers-line-impl (cdr line) (+ line-y 1) (string cur-num (car line)) include-cur-num? acc)))
            (else
              (if (or (string-null? cur-num)
                      (not include-cur-num?))
               (filter-numbers-line-impl (cdr line) (+ line-y 1) "" #f acc)
               (filter-numbers-line-impl (cdr line) (+ line-y 1) "" #f (+ (string->number cur-num) acc))))))
    (filter-numbers-line-impl (string->list line) 0 "" #f 0))
  (let loop ((line-x 0)
             (rest-lines schematic)
             (numbers-acc 0))
    (if (>= line-x (length schematic))
      numbers-acc
      (loop (+ line-x 1)
            (cdr rest-lines)
            (+ numbers-acc (filter-numbers-line (car rest-lines) line-x))))))

(filter-numbers day-03-input)
