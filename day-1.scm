(define p-input '("1abc2" "pqr3stu8vwx" "a1b2c3d4e5f" "treb7uchet"))

(define (decode-lines lines)
  (define (decode-line line)
    (define (get-number nums first last)
      (cond ((null? nums)
             (string->number (list->string (list first last))))
            ((null? first)
             (get-number (cdr nums) (car nums) (car nums)))
            (else
              (get-number (cdr nums) first (car nums)))))
    (get-number (filter char-numeric? (string->list line)) '() '()))
  (reduce + 0 (map decode-line lines)))

(decode-lines p-input) ;; -> 142
