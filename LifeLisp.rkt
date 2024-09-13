#lang sicp

;; Example grid (5x5)

(define grid1 '((0 1 0 0 0)
                (0 0 1 0 0)
                (1 1 1 0 0)
                (0 0 0 0 0)
                (0 0 0 0 0)))

(define (get-cell grid row col)
  (if (and (>= row 0) (< row (length grid))
           (>= col 0) (< col (length (car grid))))
      (list-ref (list-ref grid row) col) 0))

;; Make a list of each of the coordinate deltas to find the neighbors
(define neighbor-deltas
  '((-1 -1) (-1 0) (-1 1) ( 0 -1) ( 0 1) ( 1 -1) ( 1 0) ( 1 1)))

;; Recursive function to count neighbors
(define (count-neighbors grid row col deltas)
  (if (null? deltas)
      0
      (+ (get-cell grid (+ row (car (car deltas))) (+ col (car (cdr (car deltas)))))
         (count-neighbors grid row col (cdr deltas)))))

(display (get-cell grid1 2 0))
(display (count-neighbors grid1 1 1))