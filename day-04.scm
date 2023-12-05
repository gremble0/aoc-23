(define day-04-input (with-input-from-file "day-04.in"
  (lambda ()
    (let loop ((lines '())
               (next-line (read-line)))
       (if (eof-object? next-line)
           (reverse lines)
           (loop (cons next-line lines)
                 (read-line)))))))

;; part 1:
(define (parse-num-sequence rest cur-num acc)
  (cond ((null? rest)
         (if (not (string-null? cur-num))
           (cons (string->number cur-num) acc)
           acc))
        ((char-numeric? (car rest))
         (parse-num-sequence (cdr rest) (string cur-num (car rest)) acc))
        (else
          (parse-num-sequence (cdr rest) "" (cons (string->number cur-num) acc)))))

(define (count-wins card)
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

(reduce + 0 (map count-points (map count-wins day-04-input)))

;; part 2:
(define scratchcards-count 0)

(define (count-scratchcards scratchcards)
  (define (count-wins card)
    (let* ((card-split ((string-splitter 'delimiter #\:) card))
           (card-row (string->number (list->string (filter char-numeric? (string->list (car card-split)))))) ;; eww
           (card-nums ((string-splitter 'delimiter #\|) (cadr card-split)))
           (card-winning-nums (parse-num-sequence (string->list (car card-nums)) "" '()))
           (card-my-nums (parse-num-sequence (string->list (cadr card-nums)) "" '())))
      (let ((wins (length (filter (lambda (x)
                       (and x (memq x card-winning-nums))) card-my-nums))))
        (set! scratchcards-count (+ scratchcards-count 1))
        (for-each (lambda (row)
                    (count-wins (list-ref scratchcards row))) (iota wins card-row)))))
  (for-each count-wins scratchcards))

scratchcards-count
(count-scratchcards day-04-input)
