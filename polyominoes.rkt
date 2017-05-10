;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname polyominoes) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require "a10.rkt")

;; Uncomment the following line if you want to use
;; the examples in kanoodle.rkt
;; (require "kanoodle.rkt")

;; A Grid is a (listof (listof Char))
;; requires: both inner and outer lists of Grid are non-empty

(define-struct pos (x y))
;; A Pos is a (make-pos Nat Nat)

(define-struct state (puzzle pieces))
;; A State is a (make-state Grid (listof Grid))



;; (solve-puzzle grid polys viz-style)
;; Solve a polyomino puzzle, given the initially empty (or partially filled 
;; in) grid, a set of pieces that must be placed, and a Symbol indicating
;; what visualization style to use.  Legal viz styles are 'interactive
;; (draw every step of the search), 'at-end (just draw the solution, if one
;; is found), or 'offline (don't draw anything).  Produce either the solved
;; Grid (converted to a list of Strings, just for convenience) or false if
;; no solution exists.
;;
;; You don't need to modify this function at all.  It is provided for you
;; so that you can test your puzzle solving algorithm interactively.  If
;; you decide you want to write check-expect tests using solve-puzzle
;; (which you don't have to do, but can if you want), be sure to consume
;; 'offline for viz-style.

;; solve-puzzle: Grid (listof Grid) Sym -> (anyof (listof Str) false)
;; requires: viz-style is one of {'interactive, 'at-end or 'offline}

;; Some Examples are included below after the solve-puzzle function definition.

;; DO NOT MODIFY THIS CODE
(define (solve-puzzle grid polys viz-style)
  (local
    [(define result
       (search 
        (lambda (S) (empty? (state-pieces S)))
        neighbours
        (cond
          [(symbol=? viz-style 'interactive)
           (lambda (S) (draw-grid (state-puzzle S)))]
          [else false])
        (make-state grid polys)))
     
     (define maybe-last-draw
       (cond
         [(and (state? result)
               (symbol=? viz-style 'at-end))
          (draw-grid (state-puzzle result))]
         [else false]))]
    (cond
      [(boolean? result) result]
      [else (map list->string (state-puzzle result))])))

;; Examples:
;; (The examples are not provided in check-expect form.  They're meant to
;; demonstrate typical uses of the function, but we don't want to them to
;; open interactive visualizations every time you start the program.)

;; Solve offline (i.e. work like a normal Scheme function).
;(solve-puzzle
;  (strlist->grid '("...." "...." "...." "...." "...." "...."))
;  (cons '((#\L #\L) (#\. #\L)) (cons '((#\O)) tetrominoes-uc))
;  'offline)

;; Display the result graphically, if a solution is found.
;(solve-puzzle
;  (strlist->grid '("...." "...." "...." "...." "...." "...."))
;  (cons '((#\L #\L) (#\. #\L)) (cons '((#\O)) tetrominoes-uc))
;  'at-end)

;; Display every step of the search as it progresses.
;(solve-puzzle
;  (strlist->grid '("...." "...." "...." "...." "...." "...."))
;  (cons '((#\L #\L) (#\. #\L)) (cons '((#\O)) tetrominoes-uc))
;  'interactive)


;;
;; ***************************************
;;   Zihao Wang (20649126)
;;   CS 135 Fall 2016
;;   Assignment 10, Problem 1
;; ***************************************
;;

;; 1 (a)
;; (build-2dlist nat1 nat2 f) produces a two dimensional list
;;     with all the coordinates of the point applied to f separately
;;     by consuming the width (nat1) and the depth (nat2)
;; build-2dlist: Nat Nat (Nat Nat -> Any) -> (listof (listof Any))
;; Examples:
(check-expect (build-2dlist 3 2 +)
              (list (list 0 1 2) (list 1 2 3)))
(check-expect (build-2dlist 0 0 -) empty)

(define (build-2dlist nat1 nat2 f)
  (local [(define mult-table
            (build-list nat2
                        (lambda (i)
                          (build-list nat1
                                      (lambda (j)
                                        (list j i))))))]
    (map (lambda (x)
           (foldr (lambda (a b)
                    (cons (f (first a) (second a)) b))
                  empty x)) mult-table)))

;; Tests:
(check-expect (build-2dlist 1 1 *) '((0)))
(check-expect (build-2dlist 3 3 -) '((0 1 2) (-1 0 1) (-2 -1 0)))
(check-expect (build-2dlist 1 4 +) '((0) (1) (2) (3)))
(check-expect (build-2dlist 4 1 -) '((0 1 2 3)))
(check-expect (build-2dlist 3 6 *)
              '((0 0 0) (0 1 2) (0 2 4) (0 3 6) (0 4 8) (0 5 10)))


;; 1 (b)
;; (all-positions w d) produces list of pos which contains
;;    all the possible positions in the grid which has
;;    width w and depth d by consuming w d
;; all-positions: Nat Nat -> (listof Pos)
;; Examples:
(check-expect (lists-equiv?
               (all-positions 3 2)
               (list
                (make-pos 0 0) (make-pos 1 0) (make-pos 2 0)
                (make-pos 0 1) (make-pos 1 1) (make-pos 2 1))) true)
(check-expect (lists-equiv? (all-positions 0 0)
                            empty) true)

(define (all-positions w d)
  (foldr append empty (build-2dlist w d make-pos)))

;; Tests:
(check-expect
 (lists-equiv? (all-positions 1 1)
               (list (make-pos 0 0))) true)
(check-expect
 (lists-equiv? (all-positions 2 2) (list (make-pos 0 0)
                                         (make-pos 1 0)
                                         (make-pos 0 1)
                                         (make-pos 1 1))) true)
(check-expect
 (lists-equiv? (all-positions 2 1) (list (make-pos 0 0)
                                         (make-pos 1 0))) true)
(check-expect
 (lists-equiv? (all-positions 1 2) (list (make-pos 0 0)
                                         (make-pos 0 1))) true)
(check-expect
 (lists-equiv? (all-positions 4 4) (list (make-pos 0 0)
                                         (make-pos 1 0)
                                         (make-pos 2 0)
                                         (make-pos 3 0)
                                         (make-pos 0 1)
                                         (make-pos 1 1)
                                         (make-pos 2 1)
                                         (make-pos 3 1)
                                         (make-pos 0 2)
                                         (make-pos 1 2)
                                         (make-pos 2 2)
                                         (make-pos 3 2)
                                         (make-pos 0 3)
                                         (make-pos 1 3)
                                         (make-pos 2 3)
                                         (make-pos 3 3))) true)
(check-expect
 (lists-equiv? (all-positions 3 3) (list (make-pos 0 0)
                                         (make-pos 1 0)
                                         (make-pos 2 0)
                                         (make-pos 0 1)
                                         (make-pos 1 1)
                                         (make-pos 2 1)
                                         (make-pos 0 2)
                                         (make-pos 1 2)
                                         (make-pos 2 2))) true)


;; 2
;; (all-orientations grid) produces a list of grid which has all
;;    distinct rotations and reflections of the polyomino by consuming
;;    the grid which represents the polyomino
;; all-orientations: Grid -> (listof Grid)
;; Examples:
(check-expect
 (lists-equiv? (all-orientations '((#\A #\A #\A)
                                   (#\. #\A #\.)
                                   (#\. #\A #\.)
                                   (#\. #\A #\.)))
               '(((#\A #\A #\A)
                  (#\. #\A #\.)
                  (#\. #\A #\.)
                  (#\. #\A #\.))
                 ((#\. #\. #\. #\A)
                  (#\A #\A #\A #\A)
                  (#\. #\. #\. #\A))
                 ((#\. #\A #\.)
                  (#\. #\A #\.)
                  (#\. #\A #\.)
                  (#\A #\A #\A))
                 ((#\A #\. #\. #\.)
                  (#\A #\A #\A #\A)
                  (#\A #\. #\. #\.)))) true)

(define (all-orientations my-grid)
  (local [;; (clock-rotate my-grid) produces the grid after being rotated
          ;;     clockwisely by consuming the original grid (my-grid)
          ;; clock-rorate: Grid -> Grid
          (define (clock-rotate my-grid)
            (cond
              [(empty? my-grid) empty]
              [(empty? (first my-grid)) empty]
              [else (cons (map first my-grid)
                          (clock-rotate (map rest my-grid)))]))
          (define rotate2 (clock-rotate my-grid))
          (define rotate3 (reverse my-grid))
          (define rotate4 (reverse rotate2))
          ;; (reflect my-grid) produces the grid after being reflected by its
          ;;     vertical center line by consuming the original grid (my-grid)
          ;; reflect: Grid -> Grid
          (define (reflect my-grid)
            (map reverse my-grid))
          (define reflect1 (reflect my-grid))
          (define reflect2 (reflect rotate2))
          (define reflect3 (reflect rotate3))
          (define reflect4 (reflect rotate4))]
    (foldr (lambda (x y)
             (cons x (filter
                      (lambda (a) (not (equal? x a))) y)))
           empty (list my-grid rotate2 rotate3 rotate4
                       reflect1 reflect2 reflect3 reflect4))))

;; Tests:
(check-expect (all-orientations empty) (list empty))
(check-expect
 (lists-equiv?
  (all-orientations '((#\. #\A #\.)
                      (#\A #\A #\A)
                      (#\. #\A #\.)))
  '(((#\. #\A #\.) (#\A #\A #\A) (#\. #\A #\.)))) true)
(check-expect
 (lists-equiv?
  (all-orientations '((#\A #\A)
                      (#\A #\A)
                      (#\A #\A)
                      (#\A #\A)))
  '(((#\A #\A)
     (#\A #\A)
     (#\A #\A)
     (#\A #\A))
    ((#\A #\A #\A #\A)
     (#\A #\A #\A #\A)))) true)
(check-expect
 (lists-equiv?
  (all-orientations '((#\A #\A #\A)
                      (#\. #\A #\.)
                      (#\. #\A #\.)))
  '(((#\A #\A #\A)
     (#\. #\A #\.)
     (#\. #\A #\.))
    ((#\. #\A #\.)
     (#\. #\A #\.)
     (#\A #\A #\A))
    ((#\A #\. #\.)
     (#\A #\A #\A)
     (#\A #\. #\.))
    ((#\. #\. #\A)
     (#\A #\A #\A)
     (#\. #\. #\A)))) true)
(check-expect
 (lists-equiv?
  (all-orientations '((#\A #\A #\A #\A)
                      (#\. #\A #\A #\A)))
  '(((#\A #\A #\A #\A)
     (#\. #\A #\A #\A))
    ((#\A #\A #\A #\A)
     (#\A #\A #\A #\.))
    ((#\. #\A)
     (#\A #\A)
     (#\A #\A)
     (#\A #\A))
    ((#\A #\.)
     (#\A #\A)
     (#\A #\A)
     (#\A #\A))
    ((#\. #\A #\A #\A)
     (#\A #\A #\A #\A))
    ((#\A #\A #\A #\.)
     (#\A #\A #\A #\A))
    ((#\A #\A)
     (#\A #\A)
     (#\A #\A)
     (#\. #\A))
    ((#\A #\A)
     (#\A #\A)
     (#\A #\A)
     (#\A #\.)))) true)


;; 3
;; (first-empty-pos grid) produces the pos of the first
;;     #\. character we could meet by consuming the grid
;; first-empty-pos: Grid -> (anyof Pos Bool)
;; Examples:
(check-expect
 (first-empty-pos '((#\A #\A #\A)
                    (#\A #\A #\A)
                    (#\. #\A #\A)))
 (make-pos 0 2))
(check-expect (first-empty-pos empty) false)

(define (first-empty-pos grid)
  (cond [(empty? grid) false]
        [else
         (local [(define width (length (first grid)))
                 (define depth (length grid))
                 (define all-pos (all-positions width depth))
                 (define flat-grid (foldr append empty grid))
                 ;; (produce-pos lopos n) produces the (n+1)th pos
                 ;;     in the list of pos consumed (lopos)
                 ;; produce-pos: (listof Pos) Nat
                 ;; requires: 0 <= n <= (length lopos)
                 (define (produce-pos lopos n)
                   (cond
                     [(= n 0) (first lopos)]
                     [else (produce-pos (rest lopos) (sub1 n))]))
                 (define index-al
                   (map list
                        flat-grid
                        (build-list (length flat-grid)
                                    (lambda (x)
                                      (produce-pos all-pos x)))))]
           (second (first
                    (filter (lambda (x) (char=? (first x) #\.)) index-al))))]))


;; Tests:
(check-expect (first-empty-pos '((#\. #\A #\A #\A)
                                 (#\. #\. #\. #\.))) (make-pos 0 0))
(check-expect (first-empty-pos '((#\A #\. #\A #\A)
                                 (#\. #\. #\. #\.))) (make-pos 1 0))
(check-expect (first-empty-pos '((#\A #\A #\A #\.)
                                 (#\. #\. #\. #\.))) (make-pos 3 0))
(check-expect (first-empty-pos '((#\A #\A #\A #\A)
                                 (#\. #\. #\. #\.))) (make-pos 0 1))
(check-expect (first-empty-pos '((#\A #\A #\A #\A)
                                 (#\A #\A #\A #\A)
                                 (#\. #\A #\A #\A))) (make-pos 0 2))
(check-expect (first-empty-pos '((#\A #\A #\A #\A)
                                 (#\A #\A #\A #\.)
                                 (#\. #\A #\A #\.))) (make-pos 3 1))
(check-expect (first-empty-pos '((#\A #\A #\A #\A)
                                 (#\A #\. #\. #\A)
                                 (#\A #\A #\A #\.))) (make-pos 1 1))
(check-expect (first-empty-pos '((#\A #\A #\A #\A)
                                 (#\A #\A #\. #\A)
                                 (#\A #\A #\A #\.))) (make-pos 2 1))
(check-expect (first-empty-pos '((#\A #\A #\A #\A)
                                 (#\. #\A #\A #\A)
                                 (#\A #\A #\A #\.))) (make-pos 0 1))
(check-expect (first-empty-pos '((#\A #\A #\A #\A)
                                 (#\A #\A #\A #\A)
                                 (#\A #\A #\A #\.))) (make-pos 3 2))
(check-expect (first-empty-pos '((#\A #\A #\A #\A)
                                 (#\A #\A #\A #\A)
                                 (#\A #\. #\A #\.))) (make-pos 1 2))
(check-expect (first-empty-pos '((#\A #\A #\A #\A)
                                 (#\A #\A #\A #\A)
                                 (#\. #\. #\. #\.))) (make-pos 0 2))
(check-expect (first-empty-pos '((#\A #\A #\A #\A)
                                 (#\A #\A #\A #\A)
                                 (#\A #\. #\A #\.))) (make-pos 1 2))
(check-expect (first-empty-pos '((#\A #\A #\A #\A)
                                 (#\. #\. #\. #\.)
                                 (#\A #\A #\A #\A))) (make-pos 0 1))
(check-expect (first-empty-pos '((#\. #\. #\. #\.)
                                 (#\A #\A #\A #\A)
                                 (#\A #\A #\A #\A))) (make-pos 0 0))


;; 4

;; (get-line h my-grid) produces the h-th line of the grid
;;     by consuming the grid (my-grid)
;; get-line Nat Grid -> (listof Char)
;; requires: 0 <= h <= (length my-grid)
;;           my-grid must not be empty
;; Examples:
(check-expect (get-line 0 '((#\A #\A #\A #\A)
                            (#\. #\. #\. #\.)))
              '(#\A #\A #\A #\A))
(check-expect (get-line 1 '((#\A #\A #\A #\A)
                            (#\. #\. #\. #\.)))
              '(#\. #\. #\. #\.))

(define (get-line h my-grid)
  (cond [(= 0 h) (first my-grid)]
        [else (get-line (sub1 h) (rest my-grid))]))
;; Tests:
(check-expect (get-line 2 '((#\. #\. #\. #\.)
                            (#\A #\A #\A #\A)
                            (#\A #\A #\A #\A)))
              '(#\A #\A #\A #\A))
(check-expect (get-line 1 '((#\. #\. #\. #\.)
                            (#\A #\A #\A #\A)
                            (#\A #\A #\A #\A)))
              '(#\A #\A #\A #\A))
(check-expect (get-line 0 '((#\. #\. #\. #\.)
                            (#\A #\A #\A #\A)
                            (#\A #\A #\A #\A)))
              '(#\. #\. #\. #\.))


;; (get-elem w line) produces the w-th element of the line
;;     by consuming the line of the elements (line)
;; get-line Nat (listof Char) -> Char
;; requires: 0 <= w <= (length line)
;;           line must not be empty
;; Examples:
(check-expect (get-elem 0 '(#\A #\A #\A #\A)) #\A)
(check-expect (get-elem 2 '(#\A #\A #\. #\A)) #\.)

(define (get-elem w line)
  (cond [(= w 0) (first line)]
        [else (get-elem (sub1 w) (rest line))]))

;; Tests:
(check-expect (get-elem 2 '(#\A #\B #\C #\D)) #\C)
(check-expect (get-elem 3 '(#\A #\B #\C #\D)) #\D)
(check-expect (get-elem 0 '(#\A #\B #\C #\D)) #\A)
(check-expect (get-elem 1 '(#\A #\B #\C #\D)) #\B)

          
;; (superimpose base top pos) produces the new grid with the top laid on
;;     the base by consuming the pos of the left corner of top
;; superimpose: Grid Grid Pos -> Grid
;; Examples:
(check-expect
 (superimpose '((#\A #\A #\A #\A #\A #\A)
                (#\A #\A #\A #\A #\A #\A)
                (#\A #\A #\A #\A #\A #\A))
              '((#\B #\C)
                (#\D #\E)) (make-pos 0 0))
 '((#\B #\C #\A #\A #\A #\A)
   (#\D #\E #\A #\A #\A #\A)
   (#\A #\A #\A #\A #\A #\A)))
(check-expect
 (superimpose '((#\A #\A #\A #\A #\A #\A)
                (#\A #\A #\A #\A #\A #\A)
                (#\A #\A #\A #\A #\A #\A))
              '((#\B #\C)
                (#\D #\E)) (make-pos 1 1))
 
 '((#\A #\A #\A #\A #\A #\A)
   (#\A #\B #\C #\A #\A #\A)
   (#\A #\D #\E #\A #\A #\A)))


(define (superimpose base top pos)
  (local [;; (elem-replace w h new-line) replaces the old element with
          ;;     the new one in new-line at the same position by consuming
          ;;     the width (w) the height (h) of the old element and the
          ;;     new line (new-line)
          ;; elem-replace: Nat Nat (listof Char) -> Char
          (define (elem-replace w h new-line)
            (cond
              [(= w 0) (first new-line)]
              [else (elem-replace (sub1 w) h (rest new-line))]))]
    (build-list
     (length base)
     (lambda (h)
       (cond [(< h (pos-y pos)) (get-line h base)]
             [else (build-list
                    (length (first base))
                    (lambda (w)
                      (cond
                        [(or (< w (pos-x pos))
                             (>= w (+ (pos-x pos) (length (first top))))
                             (>= h (+ (pos-y pos) (length top)))
                             (char=? (get-elem
                                      (- w (pos-x pos))
                                      (get-line (- h (pos-y pos)) top)) #\.))
                         (get-elem w (get-line h base))]
                        [else (elem-replace
                               (- w (pos-x pos)) h
                               (get-line (- h (pos-y pos)) top))])))])))))

;; Tests
(check-expect
 (superimpose '((#\A #\A #\A #\A #\A #\A)
                (#\A #\A #\A #\A #\A #\A)
                (#\A #\A #\A #\A #\A #\A))
              '((#\B #\C)
                (#\B #\D)) (make-pos 2 2))
 '((#\A #\A #\A #\A #\A #\A)
   (#\A #\A #\A #\A #\A #\A)
   (#\A #\A #\B #\C #\A #\A)))
(check-expect
 (superimpose '((#\A #\A #\A #\A #\A #\A)
                (#\A #\A #\A #\A #\A #\A)
                (#\A #\A #\A #\A #\A #\A))
              '((#\B #\.)
                (#\. #\C)) (make-pos 0 0))
 '((#\B #\A #\A #\A #\A #\A)
   (#\A #\C #\A #\A #\A #\A)
   (#\A #\A #\A #\A #\A #\A)))
(check-expect
 (superimpose '((#\A #\A #\A #\A #\A #\A)
                (#\A #\A #\A #\A #\A #\A)
                (#\A #\A #\A #\A #\A #\A))
              '((#\B #\.)
                (#\. #\C)) (make-pos 1 1))
 '((#\A #\A #\A #\A #\A #\A)
   (#\A #\B #\A #\A #\A #\A)
   (#\A #\A #\C #\A #\A #\A)))
(check-expect
 (superimpose '((#\A #\A #\A #\A #\A #\A)
                (#\A #\A #\A #\A #\A #\A)
                (#\A #\A #\A #\A #\A #\A))
              '((#\. #\B)
                (#\. #\C)) (make-pos 1 1))
 '((#\A #\A #\A #\A #\A #\A)
   (#\A #\A #\B #\A #\A #\A)
   (#\A #\A #\C #\A #\A #\A)))
(check-expect
 (superimpose '((#\A #\A #\A #\A #\A #\A)
                (#\A #\A #\A #\A #\A #\A)
                (#\A #\A #\A #\A #\A #\A))
              '((#\B #\C #\D #\E #\F #\G)
                (#\H #\I #\J #\K #\L #\M)) (make-pos 0 0))
 '((#\B #\C #\D #\E #\F #\G)
   (#\H #\I #\J #\K #\L #\M)
   (#\A #\A #\A #\A #\A #\A)))
(check-expect
 (superimpose '((#\A #\A #\A #\A #\A #\A)
                (#\A #\A #\A #\A #\A #\A)
                (#\A #\A #\A #\A #\A #\A))
              '((#\B #\C #\D #\E #\F)
                (#\H #\I #\J #\K #\L)) (make-pos 0 0))
 '((#\B #\C #\D #\E #\F #\A)
   (#\H #\I #\J #\K #\L #\A)
   (#\A #\A #\A #\A #\A #\A)))
(check-expect
 (superimpose '((#\A #\A #\A #\A #\A #\A)
                (#\A #\A #\A #\A #\A #\A)
                (#\A #\A #\A #\A #\A #\A))
              '((#\B #\C #\D #\E #\F #\G #\N)
                (#\H #\I #\J #\K #\L #\M #\O)) (make-pos 0 0))
 '((#\B #\C #\D #\E #\F #\G)
   (#\H #\I #\J #\K #\L #\M)
   (#\A #\A #\A #\A #\A #\A)))


;; 5
;; (neighbors my-state) produces a list of new state with the
;;     puzzle that has been created by placing an additional polynomial
;;     in the pieces of the consumed state and the list of polynomials
;;     not placed in the puzzle by consuming the original state (my-state)
;; neighbors: State -> (listof State)

(define (neighbours my-state)
  (local
    [(define (first-empty puzzle poly)
       (make-pos (- (pos-x (first-empty-pos puzzle))
                    (num-poly-empty (first poly)))
                 (pos-y (first-empty-pos puzzle))))
     (define (num-poly-empty line)
       (cond [(empty? line) 0]
             [(not (char=? (first line) #\.)) 0]
             [else (+ 1 (num-poly-empty (rest line)))]))
              
     (define (inbound? puzzle poly position)
       (and (> (+ (- (length (first puzzle)) (pos-x position)) 1)
               (length (first poly)))
            (> (+ (- (length puzzle) (pos-y position)) 1)
               (length poly))))

     (define (not-covered? puzzle poly position w h)
       (cond
         [(empty? poly) true]
         [(empty? (first poly)) true]
         [(< h (pos-y position)) (not-covered? (rest puzzle) poly
                                               position w (add1 h))]
         [(< w (pos-x position)) (not-covered? (map rest puzzle) poly
                                               position (add1 w) h)]
         [(or (char=? (first (first puzzle)) #\.)
              (char=? (first (first poly)) #\.))
          (and (not-covered? (rest puzzle) (rest poly) position (add1 w) h)
               (not-covered? (map rest puzzle) (map rest poly)
                             position w (add1 h)))]
         [else false]))


     (define (superimpose-puzzle puzzle poly d-for-notcovered lines-kept)
       (cond
         [(or (false? (first-empty puzzle poly))
              (false? (inbound? puzzle poly (first-empty puzzle poly)))
              (false? (not-covered? puzzle poly (first-empty puzzle poly)
                                    0 d-for-notcovered))) false]
         [else (append lines-kept (superimpose puzzle poly
                                               (first-empty puzzle poly)))]))


     (define (all-orients puzzle poly)
       (local [(define result (filter (lambda (x) (not (false? x)))
                                      (map (lambda (x)
                                             (superimpose-puzzle puzzle x
                                                                 0 '()))
                                           (all-orientations poly))))]
         (cond [(empty? result) false]
               [else result])))
     (define all-pieces (state-pieces my-state))
     (define (neighbours/poly my-state)
       (local
         [(define poly-result (all-orients (state-puzzle my-state)
                                           (first (state-pieces my-state))))]
         (cond
           [(false? poly-result) empty]
           [else
            (map
             (lambda (x)
               (make-state x
                           (filter (lambda (x)
                                     (not (equal? x
                                                  (first
                                                   (state-pieces my-state)))))
                                   all-pieces))) poly-result)])))
        
     
     (define (neighbours/list my-state)
       (cond
         [(empty? (state-pieces my-state)) empty]
         [else (append (neighbours/poly my-state)
                       (neighbours/list
                        (make-state (state-puzzle my-state)
                                    (rest (state-pieces my-state)))))]))]
    (neighbours/list my-state)))

