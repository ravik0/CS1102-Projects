;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |assignment 5 final|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;Alan Healy, Ravi Kirschner
;;Assignment 5 - Tic Tac Toe

;; Notes for reviewer:
;; Contains minimax algorithm to find best computer moves.
;;     - 0 will be random moves
;;     - numbers 1-9 will be increasing in difficulty
;;     - numbers above about 5 take a long time to process, so would recommend testing in increasing order of difficulty until it takes too long.
;; Check-expects will take a few seconds to run, due to some processing time required.
;; Type (main START) to initialize game.

(require 2htdp/image)
(require 2htdp/universe)
(require racket/list)


(define SIZE 800) ;; can be between 300 and 900
(define PERCENT-MARGIN 0.1)
(define MARG (* PERCENT-MARGIN SIZE))
(define SQUARE-SIZE (/ (- SIZE (* 2 MARG)) 3))
(define TEXT-SIZE (round (/ SQUARE-SIZE 1.3)))
(define X-COLOR "red")
(define O-COLOR "black")
(define X (text "X" TEXT-SIZE X-COLOR))
(define O (text "O" TEXT-SIZE O-COLOR))

;WS is (make-WS Board String)
;interp. WS is the world state, containing board and who has won. By default, WS-str is "".
(define-struct WS (bd str turn depth))

;; can use this to draw the board
(define PEN (make-pen "LightSalmon" 15 "solid" "round" "round"))
(define MTS (empty-scene SIZE SIZE))

;; one easy way to draw a line
;(add-line MTS 50 50 250 50 PEN)


(check-expect (add-lines MTS)
              (add-line
               (add-line
                (add-line
                 (add-line MTS MARG (+ MARG SQUARE-SIZE) (- SIZE MARG) (+ MARG SQUARE-SIZE) PEN) 
                 MARG (- SIZE MARG SQUARE-SIZE) (- SIZE MARG) (- SIZE MARG SQUARE-SIZE) PEN)
                (+ MARG SQUARE-SIZE) MARG (+ MARG SQUARE-SIZE) (- SIZE MARG) PEN)
               (- SIZE MARG SQUARE-SIZE) MARG (- SIZE MARG SQUARE-SIZE) (- SIZE MARG) PEN))

;Image -> Image
;adds board lines to image
(define (add-lines img)
  (add-line
   (add-line
    (add-line
     (add-line img MARG (+ MARG SQUARE-SIZE) (- SIZE MARG) (+ MARG SQUARE-SIZE) PEN) ;top horizontal line
     MARG (- SIZE MARG SQUARE-SIZE) (- SIZE MARG) (- SIZE MARG SQUARE-SIZE) PEN)
    (+ MARG SQUARE-SIZE) MARG (+ MARG SQUARE-SIZE) (- SIZE MARG) PEN)
   (- SIZE MARG SQUARE-SIZE) MARG (- SIZE MARG SQUARE-SIZE) (- SIZE MARG) PEN))
  

;Square is Natural [0,2]
;interp. Square is a square of a tic-tac-toe board, contains values from 0 to 2.
;0 represents blank
;1 represents X
;2 represents O
(define square1 0) ;square with blank
(define square2 1) ;square with x in it

;Board is (listof Square)
;interp. the tic tac toe board. list length is 9.
(define board1 (list square1 square1 square1
                     square1 square2 square1
                     square1 square1 square1)) ;board with only x in middle
(define board2 (list 0 0 0
                     0 1 0
                     0 0 0)) ;same as board1, but in integer form.


(define START (make-WS (list 0 0 0
                             0 0 0
                             0 0 0) "" 1 0))


(check-expect (render-board (list 0 0 0 0 0 0 0 0 0)) (add-lines MTS))
(check-expect (render-board (list 1 0 0 0 0 0 0 0 0)) (place-image X (* 2.3 MARG) (* 2.4 MARG) (add-lines MTS)))
(check-expect (render-board (list 1 2 0 1 0 0 0 0 0)) (place-image X (* 2.3 MARG) (* 2.4 MARG)
                                                                   (place-image O (+ (* 2.3 MARG) SQUARE-SIZE) (* 2.4 MARG)
                                                                                (place-image X (* 2.3 MARG) (+ (* 2.4 MARG) SQUARE-SIZE) (add-lines MTS)))))
;Board -> Image
;takes board and draws it on the screen
(define (render-board bd)
  (local [(define-struct pos (x y))
          (define (choose-image sq)
            (cond [(= sq 0) empty-image]
                  [(= sq 1) X]
                  [(= sq 2) O]
                  [else "ERROR"]))
          (define (next-pos x y)
            (cond [(> (/ x SQUARE-SIZE) 2) (make-pos (* 2.3 MARG) (+ y SQUARE-SIZE))]
                  [else (make-pos (+ x SQUARE-SIZE) y)]))
          (define (render-board bd x y)
            (local [(define nextpos (next-pos x y))]
              (cond [(empty? bd) (add-lines MTS)]
                    [else
                     (place-image (choose-image (first bd)) x y (render-board (rest bd) (pos-x nextpos) (pos-y nextpos)))])))]
    (render-board bd (* 2.3 MARG) (* 2.4 MARG))))


(check-expect (make-move 4 (make-WS (list 0 0 0 0 0 0 0 0 0) "" 1 0)) (make-WS (list 0 0 0 0 1 0 0 0 0) "" 2 0))
(check-expect (make-move 4 (make-WS (list 0 0 0 0 2 0 0 0 0) "" 1 0)) (make-WS (list 0 0 0 0 2 0 0 0 0) "" 1 0))
(check-expect (make-move 0 (make-WS (list 0 1 1 2 1 2 1 2 1) "" 1 0)) (make-WS (list 1 1 1 2 1 2 1 2 1) "" 2 0))

;; Natural[1,2] Natural[0,8] WS -> WS
;; Makes a move if the move is possible.
(define (make-move square ws)
  (cond [(= (read-square square (WS-bd ws)) 0)
         (make-WS(append (take (WS-bd ws) square)
                         (list (WS-turn ws))
                         (drop (WS-bd ws) (+ 1 square)))
                 (WS-str ws)
                 (+ 1 (modulo (WS-turn ws) 2))
                 (WS-depth ws))]
        [else
         ws]))


(check-expect (read-square 0 (list 0 1 1 1 1 1 1 1 1)) 0)
(check-expect (read-square 8 (list 0 1 1 1 1 1 1 1 2)) 2)
(check-expect (read-square 3 (list 1 2 1 2 2 2 1 1 1)) 2)

;Integer[0,8] Board -> Integer[0,2]
;returns value of square in board
(define (read-square sq bd)
  (list-ref bd sq))


(check-expect (skip-board (list 0 1 2 3 4 5 6 7 8) 2 2 3) (list 2 4 6))
(check-expect (skip-board (list 0 1 2 3 4 5 6 7 8) 0 4 3) (list 0 4 8))
(check-expect (skip-board (list 1 1 1 2 1 2 1 1 2) 0 3 3) (list 1 2 1))

;Board Integer[0,8] Integer Integer -> (listof Squares)
;takes board, start element, how many to skip, and how many times to skip, and returns those squares in board as a list.
(define (skip-board bd start skip skipcount)
  (local [(define (skip-board bd index rsf skipcount0)
            (cond [(or (empty? bd) (= skipcount skipcount0))  (reverse rsf)]
                  [else
                   (if (= index skip)
                       (skip-board (rest bd) 1 (cons (first bd) rsf) (add1 skipcount0))
                       (skip-board (rest bd) (add1 index) rsf skipcount0))]))
          (define bdNew (drop bd start))]
    (skip-board bdNew skip empty 0)))


(check-expect (tock (make-WS (list 1 1 1 0 2 0 2 0 0) "X WINS" 2 0)) (make-WS (list 1 1 1 0 2 0 2 0 0) "X WINS" 2 0))
(check-expect (tock (make-WS (list 1 1 1 2 0 0 2 0 0) "" 2 1)) (make-WS (list 1 1 1 2 0 0 2 0 0) "X WINS" 2 1))
(check-expect (tock (make-WS (list 2 2 2 1 1 0 1 0 0) "" 1 4)) (make-WS (list 2 2 2 1 1 0 1 0 0) "O WINS" 1 4))
(check-expect (tock (make-WS (list 1 2 1 2 1 1 2 1 2) "" 2 3)) (make-WS (list 1 2 1 2 1 1 2 1 2) "DRAW" 2 3))
(check-random (tock (make-WS (list 1 1 0 1 2 2 0 0 0) "" 2 3)) (comp-move (make-WS (list 1 1 0 1 2 2 0 0 0) "" 2 3)))
(check-expect (tock (make-WS (list 1 1 0 0 2 2 0 0 0) "" 1 3)) (make-WS (list 1 1 0 0 2 2 0 0 0) "" 1 3))

; WS -> WS
; Takes WS and checks for win/does computer move.
(define (tock ws) 
  (local [(define (numX ws)
            (length (filter (lambda (x) (= x 1)) (WS-bd ws))))
          (define (numO ws)
            (length (filter (lambda (x) (= x 2)) (WS-bd ws))))
          (define win (check-for-win (WS-bd ws)))]
    (cond [(false? (string=? (WS-str ws) "")) ws]
          [(= win 1) (make-WS (WS-bd ws) "X WINS" (WS-turn ws) (WS-depth ws))]
          [(= win 2) (make-WS (WS-bd ws) "O WINS" (WS-turn ws) (WS-depth ws))]
          [(full? (WS-bd ws)) (make-WS (WS-bd ws) "DRAW" (WS-turn ws) (WS-depth ws))]
          [(> (numX ws) (numO ws)) (comp-move ws)]
          [else ws])))


;; Board -> Number
;; Checks board for a win condition
(define (check-for-win bd)
  (local [ (define win (check-row-for-win bd))
           (define wincol (check-row-for-win (convert-columns-to-rows bd)))
           (define windiag (check-diag-for-win bd))]
    (cond[(or (= win 1) (= wincol 1) (= windiag 1)) 1]
         [(or (= win 2) (= wincol 2) (= windiag 2)) 2]
         [else
          0])))


;(check-random (comp-move (make-WS (list 0 0 0 0 0 0 0 0 0) "" 2 4)) (make-WS (list-set (list 0 0 0 0 0 0 0 0 0) (random 9) 2) "" 1 4))
;(check-random (comp-move (make-WS (list 1 2 2 1 0 1 2 2 1) "" 2 4)) (make-WS (list-set (list 1 2 2 1 0 1 2 2 1) 4 2) "" 1 4))

;WS -> WS
;Makes a move for the computer. (The original random move maker)
#;   ;(Outdated)
(define (comp-move ws)
  (local [(define random-index (random 9))]
    (cond [(= (list-ref (WS-bd ws) random-index) 0) (make-move random-index ws)]
          [else
           (comp-move ws)])))


(check-expect (comp-move (make-WS (list 0 0 1 0 0 0 0 0 0) "" 2 4))
              (make-WS (list 0 0 1 0 2 0 0 0 0) "" 1 4))

;;WS -> WS
;; Makes a move for the computer
(define (comp-move ws)
  (local [(define list-of-moves (next-boards ws))]
    (argmax minimax-search-move list-of-moves)))


(check-expect (minimax-search-move (make-WS (list 1 1 0 2 0 0 2 0 0) "" 1 1)) -10)
(check-expect (minimax-search-move (make-WS (list 2 2 0 1 0 0 1 0 0) "" 2 1)) 10)

;;WS -> Number
;;Searches for (WS-depth ws) layers down a tree of moves for the number of successful end states.

(define (minimax-search-move ws)
  (local [(define (search-board ws score worklist current-depth)
            
            (cond [(> current-depth (WS-depth ws)) 0]
                  [(end-state? ws) (assign-score (WS-bd ws))]
                  [else
                   (search-lobd (append worklist (next-boards ws)) score (+ current-depth 1))]))
          
          (define (search-lobd lobd score current-depth)
            (cond [(empty? lobd) 0]
                  [else
                   (local [(define try (search-board (first lobd) score (rest lobd) current-depth))]
                     (+ try
                        (search-lobd (rest lobd) score current-depth)))]))]
    
    (search-board ws 0 empty 0)))


(check-expect (end-state? (make-WS (list 1 1 1 0 2 0 2 0 0) "" 2 0)) true)
(check-expect (end-state? (make-WS (list 1 2 1 0 2 0 0 2 1) "" 1 0)) true)
(check-expect (end-state? (make-WS (list 1 2 1 2 2 1 1 1 2) "" 1 2)) true)
(check-expect (end-state? (make-WS (list 1 0 2 0 0 0 0 0 0) "" 1 2)) false)

;; WS -> Boolean
;; Checks if current WS is a win for either player or a draw.
(define (end-state? ws)
  (or (> (check-for-win (WS-bd ws)) 0)
      (full? (WS-bd ws))))
   


(check-expect (assign-score (list 1 1 1 0 2 0 2 0 0)) -10)
(check-expect (assign-score (list 2 2 2 0 1 1 1 0 0)) 10)
(check-expect (assign-score (list 1 2 1 2 1 2 2 1 2)) 0)

;; Board -> Number
;; Returns a score of a completed board (either player wins, or a draw)
;;     +10 for computer win
;;     +0 for draw
;;     -10 for player win
(define (assign-score bd)
  (cond [(= 2 (check-for-win bd)) 10]
        [(= 1 (check-for-win bd)) -10]
        [else
         0]))


(check-expect (next-boards (make-WS (list 1 2 2 2 1 2 1 0 0) "" 1 2)) (list (make-WS (list 1 2 2 2 1 2 1 1 0) "" 2 2) (make-WS (list 1 2 2 2 1 2 1 0 1) "" 2 2)))

;; WS -> (listof WS)
;; Generate next possible moves for a given world state
(define (next-boards ws)
  (local [(define possible-moves-list (list 0 1 2 3 4 5 6 7 8))
          
          (define (remove-bad-moves ws)
            (filter (λ (m) (not (or (= (list-ref (WS-bd ws) m) 1) (= (list-ref (WS-bd ws) m) 2)))) possible-moves-list))
          
          (define (make-possible-moves possible-moves-list)
            (cond [(empty? possible-moves-list) empty]
                  [else
                   (cons (make-move (first possible-moves-list) ws)
                   (make-possible-moves (rest possible-moves-list)))]))]
    
    (make-possible-moves (remove-bad-moves ws))))

                             

(check-expect (render (make-WS (list 1 1 1 0 2 0 0 2 0) "X WINS" 1 0)) (place-image
                                                                        (text "X WINS" (round (* SIZE 0.255)) (find-color "X WINS"))
                                                                        (/ SIZE 2) (/ SIZE 2) (render-board (list 1 1 1 0 2 0 0 2 0))))
(check-expect (render (make-WS (list 1 1 0 0 2 0 0 0 2) "" 1 0)) (render-board (list 1 1 0 0 2 0 0 0 2)))

;WS -> image
;Renders the worldstate
(define (render ws)
  (cond [(false? (string=? (WS-str ws) ""))
         (place-image (text (WS-str ws) (round (* SIZE 0.255)) (find-color (WS-str ws))) (/ SIZE 2) (/ SIZE 2) (render-board (WS-bd ws)))]
        [else (render-board (WS-bd ws))]))


(check-expect (find-color "X WINS") "green")
(check-expect (find-color "O WINS") "purple")
(check-expect (find-color "DRAW") "blue")

;String -> String
;determines the color of the win text
(define (find-color str)
  (cond [(string=? str "X WINS") "green"]
        [(string=? str "O WINS") "purple"]
        [else "blue"]))


(check-expect (convert-columns-to-rows (list 0 1 2 3 4 5 6 7 8)) (list 0 3 6 1 4 7 2 5 8))
(check-expect (convert-columns-to-rows (list 1 0 0 1 0 0 1 0 0)) (list 1 1 1 0 0 0 0 0 0))

;Board -> Board
;takes board and turns all of the columns into the rows.
(define (convert-columns-to-rows bd)
  (append
   (skip-board bd 0 3 3)
   (skip-board bd 1 3 3)
   (skip-board bd 2 3 3)))


(check-expect (check-row-for-win (list 1 2 1 1 1 2 2 1 2)) 0)
(check-expect (check-row-for-win (list 1 1 1 0 0 0 0 0 0)) 1)
(check-expect (check-row-for-win (list 2 2 2 0 0 0 0 0 0)) 2)
(check-expect (check-row-for-win (list 0 1 2 1 1 2 2 2 1)) 0)

;; board -> square
;; Takes board and returns 0 if no players have won yet, or the number of the player who has won
(define (check-row-for-win bd)
  (cond [(empty? bd) 0]
        [(win? (take bd 3) 1) 1]
        [(win? (take bd 3) 2) 2]
        [else (check-row-for-win (drop bd 3))]))


(check-expect (check-diag-for-win (list 1 0 2 0 1 0 0 2 1)) 1)
(check-expect (check-diag-for-win (list 1 0 2 0 2 0 2 0 1)) 2)
(check-expect (check-diag-for-win (list 1 0 0 2 0 0 0 0 0)) 0)

;board -> Integer[0,2]
;Returns the winner if there is a winner in the diagonal, otherwise 0.
(define (check-diag-for-win bd)
  (local [(define diag1 (skip-board bd 2 2 3))
          (define diag2 (skip-board bd 0 4 3))]
    (max (check-row-for-win diag1) (check-row-for-win diag2))))


(check-expect (win? (list 1 1 1) 2) false)
(check-expect (win? (list 1 1 1) 1) true)
(check-expect (win? empty 1) false)
(check-expect (win? (list 1 2 1) 1) false)

;(listof Squares) Integer[1,2] -> Boolean
;Takes a list of squares and either the player/computer (denoted by 1 or 2) and determines if they have won
(define (win? squares player)
  (cond [(empty? squares) false]
        [else (andmap (lambda (x) (= player x)) squares)]))


(check-expect (full? (list 0 0 0 0 0 0 0 0 0)) false)
(check-expect (full? (list 1 1 1 1 1 1 1 1 1)) true)
(check-expect (full? (list 1 1 1 1 1 1 1 1 0)) false)

;Board -> Boolean
;determines if a board has 9 spots and none of them are 0, 
(define (full? bd)
  (and (= (length bd) 9) (andmap (lambda (x) (not (= x 0))) bd)))


(check-expect (handle-mouse (make-WS (list 0 0 0 0 0 0 0 0 0) "" 1 0) (/ SIZE 2) (/ SIZE 2) "button-down") (make-WS (list 0 0 0 0 1 0 0 0 0) "" 2 0))
(check-expect (handle-mouse (make-WS (list 0 0 0 1 2 0 0 0 0) "" 1 0) (/ SIZE 2) (/ SIZE 2) "button-down") (make-WS (list 0 0 0 1 2 0 0 0 0) "" 1 0)) 
(check-expect (handle-mouse (make-WS (list 0 0 0 1 2 0 0 0 0) "" 1 2) (/ SIZE 2) (+ MARG (/ MARG 2)) "button-down") (make-WS (list 0 1 0 1 2 0 0 0 0) "" 2 2))
(check-expect (handle-mouse (make-WS (list 1 1 1 0 2 2 0 0 0) "X WINS" 1 2) (/ SIZE 2) (/ SIZE 2) "button-down")
              (make-WS (list 1 1 1 0 2 2 0 0 0) "X WINS" 1 2))

;; WS int int mouse-event -> WS
;; places an x where the user clicks
(define (handle-mouse ws x y me)
  (local [(define mouse-square (find-mouse-square x y))]
    (if (and (string=? (WS-str ws) "") (not (= -1 mouse-square)) (mouse=? me "button-down"))
        (make-move mouse-square ws)
        ws)))


(check-expect (find-mouse-square (/ SIZE 2) (/ SIZE 2)) 4)
(check-expect (find-mouse-square MARG MARG) -1)
(check-expect (find-mouse-square (+ MARG (/ MARG 2)) (+ MARG (/ MARG 2))) 0)
;; Number Number -> Natural[-1,8]
;; Determines what square the user clicked on

(define (find-mouse-square x y)
  (local [(define (find-coord-index coord)
            (cond [(and (> coord MARG) (< coord (+ MARG SQUARE-SIZE))) 0]
                  [(and (> coord (+ MARG SQUARE-SIZE)) (< coord (+ MARG (* 2 SQUARE-SIZE)))) 1]
                  [(and (> coord (+ MARG (* 2 SQUARE-SIZE))) (< coord (+ MARG (* 3 SQUARE-SIZE)))) 2]
                  [else false]))
          (define coord-x (find-coord-index x))
          (define coord-y (find-coord-index y))]
    (if (or (false? coord-x) (false? coord-y))
        -1
        (+ coord-x (* 3 coord-y)))))


(check-expect (handle-key (make-WS (list 0 0 0 0 0 0 0 0 0) "" 1 0) "1")
              (make-WS (list 0 0 0 0 0 0 0 0 0) "" 1 1))

;;WS KeyEvent -> WS
;; assigns depth of search for computer's moves
(define (handle-key ws ke)
  (cond [(member ke (list "0" "1" "2" "3" "4" "5" "6" "7" "8" "9"))
         (make-WS
          (WS-bd ws)
          (WS-str ws)
          (WS-turn ws)
          (string->number ke))]
        [else ws]))


;WS -> WS
;Handles the world.
(define (main ws)
  (big-bang ws
    (on-tick tock) 
    (to-draw render) 
    (on-mouse handle-mouse)
    (on-key handle-key)))

(main START)