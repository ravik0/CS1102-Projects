;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname part1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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
#;
(define (fn-for-widget w)
  (... (widget-name w)
       (widget-quantity w)
       (widget-time w)
       (widget-price w)
       (fn-for-low (widget-parts w))))
#;
(define (fn-for-low low)
  (cond [(empty? low) ...]
        [else (... (fn-for-widget (first low))
                   (fn-for-low (rest low)))]))

;;find-widget-name-longer-than: widget natural -> (listof widget)
;;return a list of widgets that have a name that is longer than the provided length
(check-expect (find-widget-name-longer-than Buttons 6) (list Buttons Numbers))
(check-expect (find-widget-name-longer-than Buttons 8) empty)
(check-expect (find-widget-name-longer-than Jewelry 9) (list Jewelry))
(check-expect (find-widget-name-longer-than Jewelry 6) (list Jewelry Necklace Pendant Bracelet))
(check-expect (longer-than-helper (widget-parts Rings) 6) empty)
(check-expect (longer-than-helper (widget-parts Necklace) 5) (list Pendant))
(check-expect (longer-than-helper (widget-parts Telephone) 6) (list Receiver Buttons Numbers))

(define (find-widget-name-longer-than w l)
  (cond [(>(string-length (widget-name w)) l) (cons w (longer-than-helper (widget-parts w) l))]
        [else (longer-than-helper (widget-parts w) l)]))

;;longer-than-helper: (listof widget) natural -> (listof widget)
;;longer-than-helper is the assitance that helps with the mutual recusion of find-widget-name-longer-than by appending the lists

(define (longer-than-helper low l)
  (cond [(empty? low) empty]
        [else (append (find-widget-name-longer-than (first low) l)
                   (longer-than-helper (rest low) l))]))

;;find-widget-quantity-over: widget natural -> (listof widget)
;;returns widgets with a quantity (the amount in stock) over the given value
(check-expect (find-widget-quantity-over Buttons 8) (list Numbers))
(check-expect (find-widget-quantity-over Telephone 10) empty)
(check-expect (find-widget-quantity-over Jewelry 14) (list Rings Beads))
(check-expect (find-widget-quantity-over Jewelry 3) (list Jewelry Rings Necklace Chain Pendant Bracelet Beads Glass))
(check-expect (quantity-over-helper (widget-parts Cord) 5) empty)
(check-expect (quantity-over-helper (widget-parts Bracelet) 4) (list Beads Glass))
(check-expect (quantity-over-helper (widget-parts Necklace) 5) (list Chain))

(define (find-widget-quantity-over w q)
  (cond [(> (widget-quantity w) q) (cons w (quantity-over-helper (widget-parts w) q))]
        [else (quantity-over-helper (widget-parts w) q)]))

;;quantity-over-helper: (listof widget) natural -> (listof widget)
;;quantity-over-helper is the assitance that helps with the mutual recusion of find-widget-quantity-over by appending the lists

(define (quantity-over-helper low q)
  (cond [(empty? low) empty]
        [else (append (find-widget-quantity-over (first low) q)
                   (quantity-over-helper (rest low) q))]))

;;find-widgets-cheaper-than: widget natural -> (listof widget)
;;returns widgets with a quantity (the amount in stock) over the given value
(check-expect (find-widgets-cheaper-than Buttons 4) empty)
(check-expect (find-widgets-cheaper-than Telephone 10) (list Receiver Buttons Numbers Cord Wire))
(check-expect (find-widgets-cheaper-than Jewelry 31) (list Jewelry Rings Necklace Chain Pendant Bracelet Beads Glass))
(check-expect (find-widgets-cheaper-than Jewelry 3) (list Chain Pendant))
(check-expect (cheaper-than-helper (widget-parts Cord) 6) (list Wire))
(check-expect (cheaper-than-helper (widget-parts Bracelet) 8) (list Beads Glass))
(check-expect (cheaper-than-helper (widget-parts Necklace) 0) empty)

(define (find-widgets-cheaper-than w p)
  (cond [(< (widget-price w) p) (cons w (cheaper-than-helper (widget-parts w) p))]
        [else (cheaper-than-helper (widget-parts w) p)]))

;;quantity-over-helper: (listof widget) natural -> (listof widget)
;;quantity-over-helper is the assitance that helps with the mutual recusion of find-widget-quantity-over by appending the lists

(define (cheaper-than-helper low p)
  (cond [(empty? low) empty]
        [else (append (find-widgets-cheaper-than (first low) p)
                   (cheaper-than-helper (rest low) p))]))

;find-widget-hard-make: widget natural number -> (listof widget)
;find widgets whose quantity is less than natural or cost is greater than number
(check-expect (find-widget-hard-make Buttons 9 10) (list Buttons))
(check-expect (find-widget-hard-make Buttons 9 4) (list Buttons Numbers))
(check-expect (find-widget-hard-make Buttons 1 10) empty)
(check-expect (find-widget-hard-make Telephone 6 6) (list Telephone Receiver Wire))
(check-expect (hard-make-list-helper (widget-parts Buttons) 9 10) empty)
(check-expect (hard-make-list-helper (widget-parts Jewelry) 8 100) (list Chain Pendant Bracelet Glass))
(check-expect (hard-make-list-helper (widget-parts Bracelet) 5 6) (list Beads))
(check-expect (widget-price-quantity? Bracelet 6 4) true)
(check-expect (widget-price-quantity? Telephone 4 13) true)
(check-expect (widget-price-quantity? Cord 7 5) false)

(define (find-widget-hard-make w q p)
  (cond [(widget-price-quantity? w q p) (cons w (hard-make-list-helper (widget-parts w) q p))]
        [else (hard-make-list-helper (widget-parts w) q p)]))

;widget-price-quantity?: widget natural number -> boolean
;checks to see if the widget quantity is less than q or if its price is greater than p.
(define (widget-price-quantity? w q p)
  (or (< (widget-quantity w) q) (> (widget-price w) p)))

;;hard-make--list-helper: (listof widget) natural number -> (listof widget)
;;hard-make--list-helper is the assitance that helps with the mutual recusion of find-widget-hard-make by appending the lists
(define (hard-make-list-helper low q p)
  (cond [(empty? low) empty]
        [else (append (find-widget-hard-make (first low) q p)
                   (hard-make-list-helper (rest low) q p))]))


       