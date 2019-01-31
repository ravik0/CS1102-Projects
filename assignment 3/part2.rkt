;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname part2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;Ryley Wheelock and Ravi Kirschner
(define-struct widget(name quantity time price parts))
;; a widget is a (make-widget String Natural Natural Number ListOfWidget)

(define Wire (make-widget "Wire" 3 5 5 empty))
(define Cord (make-widget "Cord" 7 5 5 (list Wire)))
(define Numbers (make-widget "Numbers" 9 5 5 empty))
(define Buttons (make-widget "Buttons" 8 5 5 (list Numbers)))
(define Receiver (make-widget "Receiver" 10 5 7 empty))
(define Telephone (make-widget "Telephone" 5 20 15 (list Receiver Buttons Cord)))

(define Glass (make-widget "Glass" 6 9 4 empty))
(define Beads (make-widget "Beads" 25 12 7 (list Glass)))
(define Bracelet (make-widget "Bracelet" 5 3 5 (list Beads)))
(define Chain (make-widget "Chain" 7 2 1 empty))
(define Pendant (make-widget "Pendant" 4 3 1 empty))
(define Necklace (make-widget "Necklace" 10 7 3 (list Chain Pendant)))
(define Rings (make-widget "Rings" 15 8 11 empty))
(define Jewelry (make-widget "Jewelry set" 4 17 30 (list Rings Necklace Bracelet)))

;;Part 1 Templates for history of code
#;
(define (fn-for-widget w)
  (... (widget-name w)
       (widget-quantity w)
       (widget-time w)
       (widget-price w)
       (fn-for-low (widget-parts w))))
(define (fn-for-low low)
  (cond [(empty? low) ...]
        [else (... (fn-for-widget (first low))
                   (fn-for-low (rest low)))]))

#;
(define (fn-for-widget w)
  (local [(define (fn-for-widget-top w)
            (... (widget-name w)
                 (widget-quantity w)
                 (widget-time w)
                 (widget-price w)
                 (fn-for-low (widget-parts w))))
          (define (fn-for-low-bot low)
            (cond [(empty? low) ...]
                  [else (... (fn-for-widget (first low))
                             (fn-for-low (rest low)))]))]
    (fn-for-widget-top w)))

;(widget -> Boolean) widget -> (listof Widget)
;operates on widget based on the function passed into it
(define (find-widget fntop w)
  (local [(define (fn-for-widget-top w)
            (cond [(fntop w) (cons w (fn-for-low-bot (widget-parts w)))]
                  [else (fn-for-low-bot (widget-parts w))]))
          (define (fn-for-low-bot low)
            (cond [(empty? low) empty]
                  [else (append (fn-for-widget-top (first low))
                                (fn-for-low-bot (rest low)))]))]
    (fn-for-widget-top w)))

;;find-widget-name-longer-than: widget natural -> (listof widget)
;;return a list of widgets that have a name that is longer than the provided length (simply)
(check-expect (find-widget-name-longer-than Buttons 6) (list Buttons Numbers))
(check-expect (find-widget-name-longer-than Buttons 8) empty)
(check-expect (find-widget-name-longer-than Jewelry 9) (list Jewelry))
(check-expect (find-widget-name-longer-than Jewelry 6) (list Jewelry Necklace Pendant Bracelet))

(define (find-widget-name-longer-than w l)
  (local [(define (isLonger w)
            (> (string-length (widget-name w)) l))]
    (find-widget isLonger w)))

;;find-widget-quantity-over: widget natural -> (listof widget)
;;returns widgets with a quantity (the amount in stock) over the given value (simply)
(check-expect (find-widget-quantity-over Buttons 8) (list Numbers))
(check-expect (find-widget-quantity-over Telephone 10) empty)
(check-expect (find-widget-quantity-over Jewelry 14) (list Rings Beads))
(check-expect (find-widget-quantity-over Jewelry 3) (list Jewelry Rings Necklace Chain Pendant Bracelet Beads Glass))

(define (find-widget-quantity-over w q)
  (local [(define (quantity-over w)
            (> (widget-quantity w) q))]
    (find-widget quantity-over w)))

;;find-widgets-cheaper-than: widget natural -> (listof widget)
;;returns widgets with a quantity (the amount in stock) over the given value (simply)
(check-expect (find-widgets-cheaper-than Buttons 4) empty)
(check-expect (find-widgets-cheaper-than Telephone 10) (list Receiver Buttons Numbers Cord Wire))
(check-expect (find-widgets-cheaper-than Jewelry 31) (list Jewelry Rings Necklace Chain Pendant Bracelet Beads Glass))
(check-expect (find-widgets-cheaper-than Jewelry 3) (list Chain Pendant))

(define (find-widgets-cheaper-than w p)
  (local [(define (cheaper-than w)
            (< (widget-price w) p))]
    (find-widget cheaper-than w)))


;find-widget-hard-make: widget natural number -> (listof widget)
;find widgets whose quantity is less than natural or cost is greater than number (simply)
(check-expect (find-widget-hard-make Buttons 9 10) (list Buttons))
(check-expect (find-widget-hard-make Buttons 9 4) (list Buttons Numbers))
(check-expect (find-widget-hard-make Buttons 1 10) empty)
(check-expect (find-widget-hard-make Telephone 6 6) (list Telephone Receiver Wire))

(define (find-widget-hard-make w q p)
  (local [(define (hard-make w)
            (or (< (widget-quantity w) q) (> (widget-price w) p)))]
    (find-widget hard-make w)))


;;sort-widgets: (widget widget->boolean) widget -> (listof widget)
;;sorts a widget and its parts based on the given function
(check-expect (sort-widgets (lambda (w1 w2) (> (string-length (widget-name w1)) (string-length (widget-name w2)))) Bracelet) (list Bracelet Beads Glass))
(check-expect (sort-widgets (lambda (w1 w2) (> (string-length (widget-name w1)) (string-length (widget-name w2)))) Necklace) (list Necklace Pendant Chain))
(check-expect (sort-widgets (lambda (w1 w2) (< (string-length (widget-name w1)) (string-length (widget-name w2)))) Bracelet) (list Beads Glass Bracelet))
(check-expect (sort-widgets (lambda (w1 w2) (< (string-length (widget-name w1)) (string-length (widget-name w2)))) Cord) (list Cord Wire))

(define (sort-widgets fn w)
  (local [(define LOW (find-widget widget? w))
          (define (qsort low)
            (cond
              [(empty? low) empty]
              [else
               (local
                 [(define pivot (first low))]
                 (append
                  (qsort (filter (lambda (i) (fn i pivot)) (rest low)))
                  (list pivot)
                  (qsort (filter (lambda (i) (not (fn i pivot))) (rest low)))))]))]
    (qsort LOW)))

;Widget -> Widget
;produces longest name widget out of itself and its parts
(check-expect (longest-name-widget Jewelry) Jewelry)
(check-expect (longest-name-widget Bracelet) Bracelet)
(check-expect (longest-name-widget Necklace) Necklace)
(check-expect (longest-name-widget Beads) Beads)
(define (longest-name-widget w)
  (local [(define (longer-than w1 w2)
            (> (string-length (widget-name w1)) (string-length (widget-name w2))))]
    (first (sort-widgets longer-than w))))


