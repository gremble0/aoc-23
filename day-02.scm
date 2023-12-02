(define day-02-input (with-input-from-file "day-02.in"
  (lambda ()
    (let loop ((lines '())
               (next-line (read-line)))
       (if (eof-object? next-line)
           (reverse lines)
           (loop (cons next-line lines)
                 (read-line)))))))

;; part 1:
(define max-r 12)
(define max-g 13)
(define max-b 14)

(define (count-round round)
  (define (count-round-impl rest r g b)
    (if (null? rest)
      (list r g b)
      (let ((count (string->number (list->string (filter char-numeric? (string->list (car rest)))))))
        (cond ((string-suffix? "red" (car rest))
               (count-round-impl (cdr rest) (+ r count) g b))
              ((string-suffix? "green" (car rest))
               (count-round-impl (cdr rest) r (+ g count) b))
              ((string-suffix? "blue" (car rest))
               (count-round-impl (cdr rest) r g (+ b count)))))))
  (count-round-impl ((string-splitter 'delimiter #\,) round) 0 0 0))

(define (count-game game)
  (let* ((split-game ((string-splitter 'delimiter #\;) game))
         (game-counted (map count-round split-game)))
    (if (= (length (filter (lambda (rgbs)
                             (and (<= (car rgbs) max-r)
                                  (<= (cadr rgbs) max-g)
                                  (<= (caddr rgbs) max-b)))
                           game-counted))
           (length game-counted))
      #t
      #f)))

(define (count-games games)
  (reduce + 0
    (filter (lambda (x) x) ;; remove #f's
      (map (lambda (game)
             (let ((game-split ((string-splitter 'delimiter #\:) game)))
               (if (count-game (cadr game-split))
                 (string->number (list->string (filter char-numeric? (string->list (car game-split)))))
                 #f)))
           games))))

;; part 2:
(define (count-game game)
  (let* ((split-game ((string-splitter 'delimiter #\;) game))
         (game-counted (map count-round split-game)))
    game-counted))

(define (dot-max lsts)
  (apply map max lsts))

(define (count-games games)
  (reduce + 0
    (map (lambda (game)
           (let ((freqs (count-game (cadr ((string-splitter 'delimiter #\:) game)))))
             (reduce * 1 (dot-max freqs)))) games)))

(count-games day-02-input)
