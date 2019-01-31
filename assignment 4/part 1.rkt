;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |part 1|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;(require racket/list)
(require 2htdp/image)


(define TEXT-SIZE 20)    
(define TEXT-COLOR "black") 
(define TAB 5)

(define-struct widget (name quantity price))
;; a widget is a (make-widget String Natural Number)
; same as assignment #3, except no parts component

(define a (make-widget "a" 4 3))
(define b (make-widget "b" 5 4))
(define c (make-widget "c" 3 9))
(define d (make-widget "d" 4 2))
(define e1 (make-widget "e" 4 0))
(define f (make-widget "f" 3 1))
(define low (list d f e1 b a c))

(define-struct bst (widget left right))
;; a BST is either
;;          false, or
;;          a (make-bst widget bst bst)



;; returns a random natural  between 1 and max, inclusive
;; Natural -> Natural
(define (rnd max)
  (add1 (random max)))

;; Natural Natural -> (listof widget)
;; creates a list num random widgets whose values vary between 1 and max
(define (random-widgets num max)
  (build-list num
              (lambda (dummy)
                (make-widget 
                 (number->string (rnd max))
                 (rnd max)
                 (rnd max)))))

(check-expect (find-name "hi" false) false)
(check-expect (find-name "c" tree) c)
(check-expect (find-name "g" tree) false)
(check-expect (find-name "e" tree) e1)
;; string bst -> widget | false
;; finds widget in bst with that name, returns false if not there
(define (find-name name bst)
  (cond [(false? bst) false]
        [(string=? (widget-name (bst-widget bst)) name) (bst-widget bst)]
        [(string>? name (widget-name (bst-widget bst))) (find-name name (bst-right bst))]
        [else (find-name name (bst-left bst))]))

(check-expect (insert-name (make-widget "g" 4 3) tree)
              (make-bst
               c
               (make-bst a false (make-bst b false false))
               (make-bst e1 (make-bst d false false) (make-bst f false (make-bst (make-widget "g" 4 3) false false)))))
(check-expect (insert-name (make-widget "h" 2 7) false) (make-bst (make-widget "h" 2 7) false false))
(check-expect (insert-name (make-widget "h" 2 7) (insert-name (make-widget "g" 4 3) tree))
               (make-bst
               c
               (make-bst a false (make-bst b false false))
               (make-bst e1 (make-bst d false false) (make-bst f false (make-bst (make-widget "g" 4 3) false (make-bst (make-widget "h" 2 7) false false))))))
(check-expect (insert-name d tree)
              (make-bst
               c
               (make-bst a false (make-bst b false false))
               (make-bst e1 (make-bst d false (make-bst d false false)) (make-bst f false false))))
;;widget bst -> bst
;;inserts widget into bst at correct position
(define (insert-name v b)
  (cond [(false? b) (make-bst v false false)]
        [(string>? (widget-name (bst-widget b)) (widget-name v))
         (make-bst (bst-widget b) (insert-name v (bst-left b))(bst-right b))]
        [else
         (make-bst (bst-widget b) (bst-left b)(insert-name v (bst-right b)))]))

;; (listof widget) bst -> bst
;; inserts all widgets in low into bst
(define (build-tree low)
  (foldr insert-name false low))

(define tree (build-tree low))

;; here is some code related to displaying a tree
;; you might find it helpful for debugging
;; (render bst) -> image


;; helper functions, can ignore
(define (blanks n)
  (list->string (build-list n (lambda (x) #\ ))))

(define (to-text side w t)
  (text  (string-append (blanks t) side (widget-name w)) TEXT-SIZE TEXT-COLOR))

(define (render-helper b t img side)
  (if (false? b)
      img
      (above/align "left"
                   (to-text side (bst-widget b) t)
                   (render-helper (bst-left b) (+ t TAB) img "L: ")
                   (render-helper (bst-right b) (+ t TAB) img "R: "))))
;; end of helper functio

;; render:  BST -> image
;; provides a graphical representation of the tree
(define (render b)
  (render-helper b 0 (square 0 "solid" "white") "T: "))


