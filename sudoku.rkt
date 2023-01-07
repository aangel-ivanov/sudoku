;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname sudoku) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;; ~v is used for casting to string when displaying output
(require racket/format)

;; Returns the corresponding row
(define (get-row s row)
  (cond [(= row 1) (first s)]
        [else (get-row (rest s) (sub1 row))]))

;; Returns the corresponding column
(define (get-col s col)
  (local [(define (iter res row)
            (cond [(> row 9) res]
                  [else (iter (append res (list (get-val s row col))) (add1 row))]))]
    (iter '() 1)))

;; Returns the corresponding val
(define (get-val s row col)
  (local [(define (iter row col)
            (cond [(= col 1) (first row)]
                  [else (iter (rest row) (sub1 col))]))]
    (iter (get-row s row) col)))

;; Returns the corresponding box
(define (get-box s row col)
            (local [; (define box (+ (* 3 (floor (/ (- row 1) 3))) (floor (/ (- column 1) 3)) 1))
                    (define start-row (+ (* 3 (floor (/ (- row 1) 3))) 1))
                    (define start-col (+ (* 3 (floor (/ (- col 1) 3))) 1))

                    (define (iter3 vals col)
                      (cond [(= col 1) (list (first vals) (second vals) (third vals))]
                            [else (iter3 (rest vals) (sub1 col))]))

                    (define (iter2 s row col res)
                      (cond [(zero? row) res]
                            [else (iter2 (rest s) (sub1 row) col (append res (iter3 (first s) col)))]))

                    (define (iter s row col)
                      (cond [(= row 1) (iter2 s 3 col '())]
                            [else (iter (rest s) (sub1 row) col)]))]
              (iter s start-row start-col)))


;; Displays the sudoku s in the interactions window
(define (display-sudoku s)
  (local [(define (iter s row col)
            (local [(define border
                      (string-append
                       (cond [(and (= (modulo row 3) 1) (= col 1)) "+---+---+---+\n"]
                             [else ""])
                       (cond [(= (modulo col 3) 1) "|"]
                             [else ""])
                       (cond [(zero? (get-val s row col)) "?"]
                             [else (~v (get-val s row col))])
                       (cond [(= col 9) "|\n"]
                             [else ""])))]

              (cond [(< col 9) (string-append
                                border
                                (iter s row (add1 col)))]
                    [(< row 9) (string-append
                                border
                                (iter s (add1 row) 1))]
                    [else (string-append
                           border
                           "+---+---+---+\n")])))]
    (display (iter s 1 1))))

;; Places val at corresponding position in s
(define (place s row col val)
  (local [(define (iter2 vals col res)
            (cond [(null? vals) (list res)]
                  [else (cond [(= col 1) (iter2 (rest vals) (sub1 col)
                                                (append res (list val)))]
                              [else (iter2 (rest vals) (sub1 col)
                                           (append res (list (first vals))))])]))

          (define (iter s row col res)
            (cond [(null? s) res]
                  [else (cond [(= row 1) (iter (rest s) (sub1 row) col
                                               (append res (iter2 (first s) col '())))]
                              [else (iter (rest s) (sub1 row) col
                                          (append res (list (first s))))])]))]
    (iter s row col '())))

;; Checks if value at corresponding position is valid
(define (value-okay? s row col)
  (local [;; Checks if row is valid
          (define (row-okay? vals col val)
            (local [(define (iter vals col res)
                      (cond [(null? vals) res]
                            [else (cond [(= col 1) (iter (rest vals) (sub1 col) res)]
                                        [else (iter (rest vals) (sub1 col)
                                                    (append res (list (first vals))))])]))]
              (foldl (lambda (n m) (cond [(zero? n) m]
                                         [else (cond [(= n val) #f]
                                                     [else m])]))
                     #t
                     (iter vals col '()))))

          ;; Checks if column is valid
          (define (column-okay? vals row val) (row-okay? vals row val))

          ;; Checks if box is valid
          (define (box-okay? vals row col val)
            (row-okay? vals (+ (* 3 (remainder (sub1 row) 3)) (remainder (sub1 col) 3) 1) val))]

    (and (not (zero? (get-val s row col)))
       (row-okay? (get-row s row) col (get-val s row col))
       (column-okay? (get-col s col) row (get-val s row col))
       (box-okay? (get-box s row col) row col (get-val s row col)))))

;; Solves the Sudoku puzzle
(define (solve puzzle)
  (local [(define (iter s row col back)
            (cond [(= row 10) (display-sudoku s)] ; Puzzle is solved
                  [(get-val (second puzzle) row col) ; Ran into a fixed value so skip it
                   (cond [back ; we are backtracking and need to continue to backtrack
                          (cond [(= col 1) (iter s (sub1 row) 9 #t)]
                                [else (iter s row (sub1 col) #t)])]
                         [else (cond [(= col 9) (iter s (add1 row) 1 #f)]
                                     [else (iter s row (add1 col) #f)])])]
                  [(and (value-okay? s row col) (not back)) ; Ran into a number that works so skip it
                   (cond [(= col 9) (iter s (add1 row) 1 #f)]
                         [else (iter s row (add1 col) #f)])]
                  [(< (get-val s row col) 9) ; Try a different value
                   (iter (place s row col (add1 (get-val s row col))) row col #f)]
                  [else ; Backtrack
                   (cond [(= col 1) (iter (place s row col 0) (sub1 row) 9 #t)]
                         [else (iter (place s row col 0) row (sub1 col) #t)])]))]

    (iter (first puzzle) 1 1 #f)))
    

;; Attaches the list of boolean values to the sudoku,
;;   where true corresponds to a given value and false not given
(define (make-sudoku-obj s)
  (local [(define (iter s-bools row col)
            (cond [(> col 9) (iter s-bools (add1 row) 1)]
                  [(> row 9) (list s s-bools)]
                  [(zero? (get-val s row col))
                   (iter (place s-bools row col #f) row (add1 col))]
                  [else (iter (place s-bools row col #t) row (add1 col))]))]

    (iter s 1 1)))


;; Example sudokus listed below

(define ex1
  (make-sudoku-obj
   '((0 0 0 9 7 0 0 0 0) 
     (0 4 0 2 5 0 1 0 7) 
     (0 0 7 6 0 0 4 0 3) 
     (0 1 2 8 0 0 6 0 0) 
     (9 7 0 0 4 0 0 3 5) 
     (0 0 4 0 0 2 9 1 0) 
     (2 0 1 0 0 7 5 0 0) 
     (4 0 9 0 8 1 0 6 0) 
     (0 0 0 0 2 9 0 0 0))))

(define ex2
  (make-sudoku-obj
   '((0 0 0 2 6 0 7 0 1) 
    (6 8 0 0 7 0 0 9 0) 
    (1 9 0 0 0 4 5 0 0) 
    (8 2 0 1 0 0 0 4 0) 
    (0 0 4 6 0 2 9 0 0) 
    (0 5 0 0 0 3 0 2 8) 
    (0 0 9 3 0 0 0 7 4) 
    (0 4 0 0 5 0 0 3 6) 
    (7 0 3 0 1 8 0 0 0))))

(define ex3
  (make-sudoku-obj
   '((0 0 0 6 0 0 4 0 0) 
    (7 0 0 0 0 3 6 0 0) 
    (0 0 0 0 9 1 0 8 0) 
    (0 0 0 0 0 0 0 0 0) 
    (0 5 0 1 8 0 0 0 3) 
    (0 0 0 3 0 6 0 4 5) 
    (0 4 0 2 0 0 0 6 0) 
    (9 0 3 0 0 0 0 0 0) 
    (0 2 0 0 0 0 1 0 0))))


  
