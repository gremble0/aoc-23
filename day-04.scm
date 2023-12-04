(define day-04-input (with-input-from-file "day-04.in"
  (lambda ()
    (let loop ((lines '())
               (next-line (read-line)))
       (if (eof-object? next-line)
           (reverse lines)
           (loop (cons next-line lines)
                 (read-line)))))))

(define (parse-card card)
  (define (parse-num-sequence rest cur-num acc)
    (cond ((null? rest)
           (if (not (string-null? cur-num))
             (cons (string->number cur-num) acc)
             acc))
          ((char-numeric? (car rest))
           (parse-num-sequence (cdr rest) (string cur-num (car rest)) acc))
          (else
            (parse-num-sequence (cdr rest) "" (cons (string->number cur-num) acc)))))
  (let* ((card-no-intro (cadr ((string-splitter 'delimiter #\:) card)))
         (card-nums ((string-splitter 'delimiter #\|) card-no-intro))
         (card-winning-nums (parse-num-sequence (string->list (car card-nums)) "" '()))
         (card-my-nums (parse-num-sequence (string->list (cadr card-nums)) "" '())))
       (length (filter (lambda (x)
                     (and x (memq x card-winning-nums))) card-my-nums))))

(define (count-points card-wins)
  (if (= card-wins 0)
    0
    (let loop ((wins-iter card-wins)
               (points 1))
      (if (= wins-iter 1)
        points
        (loop (- wins-iter 1)
              (* points 2))))))

(reduce + 0 (map count-points (map parse-card day-04-input)))
