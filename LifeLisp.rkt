#lang racket

; Create a list of coordinates
; Base case: if the input list is empty, return an empty list
(define (create-coordinates coord-list)
  (if (null? coord-list)
      '()
      (cons (car coord-list) (create-coordinates (cdr coord-list)))))

; Add a coordinate (x, y) to the front of the list
; Add the coordinate pair as the new head (car) of the list
(define (add-coordinate existing-list x y)
  (cons (list x y) existing-list))

; Find a coordinate (x, y) in the list
(define (find-coordinate lst x y)
  (cond ((null? lst) #f)  ; Base case: If the list is empty, return false (#f)
        ((and (= (car (car lst)) x) (= (cadr (car lst)) y)) #t)  ; If the coordinate matches, return true (#t)
        (else (find-coordinate (cdr lst) x y))))  ; Recursively search the rest of the list

; Remove a coordinate (x, y) from the list
(define (remove-coordinate lst x y)
  (cond ((null? lst) '())  ; Base case: If the list is empty, return an empty list
        ((and (= (car (car lst)) x) (= (cadr (car lst)) y)) (cdr lst))  ; If the coordinate matches, skip it
        (else (cons (car lst) (remove-coordinate (cdr lst) x y)))))  ; Otherwise, keep doing recursion

; A list of dltas to get all neighbors
(define neighbor-deltas
  '((-1 -1) (-1 0) (-1 1) ( 0 -1) ( 0 1) ( 1 -1) ( 1 0) ( 1 1)))

(define (flatten-once list)
(if (empty? list) '() 
(append (car list) (flatten-once (cdr list)))))

; Count the number of neighbors that are alive, given a list of x, y coordinates
; (Use let*  over let when a parameter is used inside another parameter lower down)
(define (count-neighbors-helper deltas cells x y count)
  (if (null? deltas)                   ; Base case: no more neighbors to check
      count                            ; Return the total count of live neighbors
      (let* ((dx (car (car deltas)))   ; Extract the delta for x
             (dy (cadr (car deltas)))  ; Extract the delta for y
             (neighbor-x (+ x dx))     ; Calculate the neighbor's x coordinate
             (neighbor-y (+ y dy)))    ; Calculate the neighbor's y coordinate
        (if (find-coordinate cells neighbor-x neighbor-y)                ; Check if the neighbor is alive
            (count-neighbors-helper (cdr deltas) cells x y (+ count 1))  ; If alive, increment the count
            (count-neighbors-helper (cdr deltas) cells x y count)))))    ; Otherwise, keep the count unchanged


; Main procedure to count live neighbors for a given (x, y) in a list of live cells
(define (count-live-neighbors lst x y)
  (count-neighbors-helper neighbor-deltas lst x y 0))

; procedure to check if a cell should live in the next generation
; Takes in the number of live neighbors and the cell (as a pair)
(define (should-live? live-cells cell neighbors)
  (or (and (find-coordinate live-cells (car cell) (cadr cell))       ; Cell is alive
           (or (= neighbors 2) (= neighbors 3)))                     ; Lives with 2 or 3 neighbors
      (and (not (find-coordinate live-cells (car cell) (cadr cell))) ; Cell is dead
           (= neighbors 3))))                                        ; Becomes alive with exactly 3 neighbors

; procedure to check a specific cell and add it to the new generation if it should live
; Gets number of live neighbors, checks the cell with should-live? and adds it to checked-cells if it passed
(define (check-cell live-cells cell checked-cells)
  (let ((neighbors (count-live-neighbors live-cells (car cell) (cadr cell))))
    (if (and (should-live? live-cells cell neighbors) (not (find-coordinate checked-cells (car cell) (cadr cell))))
      (append checked-cells (list cell))
        checked-cells)))

(define (check-all-cells-it list live checked)
(if (empty? list) checked
  
   (check-all-cells-it (cdr list) live (check-cell live (car list) checked))
)
)

(define (check-all-cells live-cells)
(check-all-cells-it (flatten-once (map neighbors-of-cell live-cells)) live-cells '()))


; procedure to get all the neighboring coordinates of a given cell
; Uses map with a lambda to create a new list of all cells that are neighbors to the cell that was passed in
(define (neighbors-of-cell cell)
  (map (lambda (delta)
         (list (+ (car cell) (car delta))     ; add the delta to the x coordinate
               (+ (cadr cell) (cadr delta)))) ; add the delta to the y coordinate
       neighbor-deltas))

; Function to check all the live cells and their neighbors
; This was the hardest procedure to write
; On the last line, I added the neighbors to the middle copy of live-cells, this queues them up to be checked
; I then ran check-cell on the current cell to determine if it should be added to checked-cells

; Function to apply the Game of Life rules and get the next generation of live cells
; call check-neighbors with two copies of the live-cells, and an empty list for visited cells
; Recursive function to simulate the Game of Life iterations
(define (game-of-life live-cells iterations)
  (if (> iterations 0)
      (let ((next-gen (check-all-cells live-cells))) ; let next-gen be 
        (display "Live cells: ")
        (display next-gen) ; Print the live cells' coordinates
        (newline)
        (game-of-life next-gen (- iterations 1))) ; Recurse to the next iteration
      (display "End of simulation.")))

; Example simulation with a glider
(game-of-life '((1 2) (2 2) (3 2)) 50)


; Tests
; Create a linked list of coordinates
(define coordinates (create-coordinates '((1 2) (3 4) (5 6))))
(display coordinates)
(display "\n")

; Add a new coordinate (7, 8)
(define coordinates2 (add-coordinate coordinates 7 8))
(display coordinates2)
(display "\n")

; Find if the coordinate (3, 4) exists
(display (find-coordinate coordinates2 3 4))
(display "\n")

; Remove the coordinate (3, 4)
(define coordinates3 (remove-coordinate coordinates2 3 4))
(display coordinates3)
(display "\n")

; Define a grid of live cells and count each of their live neighbors
(define live-cells '((1 1) (2 2) (3 2) (3 1) (3 0)))
(display (count-live-neighbors live-cells 2 2))
(display "\n")
(display (count-live-neighbors live-cells 1 1))
(display "\n")
(display (count-live-neighbors live-cells 3 1))
(display "\n")
(display (count-live-neighbors live-cells 4 4))
(display "\n")
(define pairs '((6 7) (8 9 (1 2))))

(map neighbors-of-cell pairs)
(display (flatten-once (list '((6 7) (8 9)) '((0 2) (2 3)))))
(display "\n")
(display "\n")
(display (check-all-cells '((0 1) (0 2) (0 3))))
(display "\n")
(display "\n")
(display (check-cell '((3 1) (2 1) (4 1)) '(3 1) '()))

