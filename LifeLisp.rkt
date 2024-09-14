#lang sicp

; Create a list of coordinates
; Base case: if the input list is empty, return an empty list
(define (create-coordinates coord-list)
  (if (null? coord-list)
      '()
      (cons (car coord-list) (create-coordinates (cdr coord-list)))))  ;; Recursively build the list

; Add a coordinate (x, y) to the front of the list
; Add the coordinate pair as the new head (car) of the list
(define (add-coordinate existing-list x y)
  (cons (list x y) existing-list))

; Find a coordinate (x, y) in the list
(define (find-coordinate lst x y)
  (cond ((null? lst) #f)  ;; Base case: If the list is empty, return false (#f)
        ((and (= (car (car lst)) x) (= (cadr (car lst)) y)) #t)  ;; If the coordinate matches, return true (#t)
        (else (find-coordinate (cdr lst) x y))))  ;; Recursively search the rest of the list

; Remove a coordinate (x, y) from the list
; Base case: If the list is empty, return an empty list
; If the coordinate matches, skip it
; Otherwise, keep doing recursion
(define (remove-coordinate lst x y)
  (cond ((null? lst) '())
        ((and (= (car (car lst)) x) (= (cadr (car lst)) y)) (cdr lst))
        (else (cons (car lst) (remove-coordinate (cdr lst) x y)))))

; A list of dltas to get all neighbors
(define neighbor-deltas
  '((-1 -1) (-1 0) (-1 1) ( 0 -1) ( 0 1) ( 1 -1) ( 1 0) ( 1 1)))

; Count the number of neighbors that are alive, give a list and x, y coordinates
; (Use let*  over let when a parameter is used inside another parameter lower down)
(define (count-neighbors-helper deltas lst x y count)
  (if (null? deltas)                   ; Base case: no more neighbors to check
      count                            ; Return the total count of live neighbors
      (let* ((dx (car (car deltas)))   ; Extract the delta for x
             (dy (cadr (car deltas)))  ; Extract the delta for y
             (neighbor-x (+ x dx))     ; Calculate the neighbor's x coordinate
             (neighbor-y (+ y dy)))    ; Calculate the neighbor's y coordinate
        (if (find-coordinate lst neighbor-x neighbor-y)                ; Check if the neighbor is alive
            (count-neighbors-helper (cdr deltas) lst x y (+ count 1))  ; If alive, increment the count
            (count-neighbors-helper (cdr deltas) lst x y count)))))    ; Otherwise, keep the count unchanged

; Main function to count live neighbors for a given (x, y) in a list of live cells
(define (count-live-neighbors lst x y)
  (count-neighbors-helper neighbor-deltas lst x y 0))


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
