;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname sudoku-project) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
(define (all-satisfy? pred? matrix)
  (cond[(empty? matrix) true]
       [(foldr (lambda (item cumulative) (and (pred? item) cumulative)) true (first matrix))
        (all-satisfy? pred? (rest matrix))]
       [else false]))

(define (any-satisfy? pred? matrix)
  (cond[(empty? matrix) false]
       [(foldr (lambda (item cumulative) (or (pred? item) cumulative)) false (first matrix)) true]
       [else (any-satisfy? pred? (rest matrix))]))

(define (find-where pred? matrix)
  (local[(define (find-where/acc pred? matrix row rownum colnum)
           (cond[(empty? row) (find-where/acc pred? (rest matrix) (first (rest matrix))
                                              (add1 rownum) 0)]
                [(pred? (first row)) (list colnum rownum)]
                [else (find-where/acc pred? matrix (rest row) rownum (add1 colnum))]))]
    (find-where/acc pred? matrix (first matrix) 0 0)))
                
(define (strings->puzzle los)
  (local[(define loloc (map string->list los))
         (define natlist (build-list (length (first loloc)) add1))
         (define (convert-char char)
           (cond [(char=? char #\?) natlist]
                 [else (list (- (char->integer char) 48))]))
         (define (convert-row row)
           (map convert-char row))]
    (map convert-row loloc)))

(define (remove-singles puzzle)
  (local[(define (single? x)
           (and (list? x) (= (length x) 1)))
         (define (contains-single? puzzle)
           (any-satisfy? single? puzzle))
         (define (find-single-coords puzzle)
           (find-where single? puzzle))
         (define (list-ref-safe lst i)
           (cond[(zero? i) (first lst)]
                [else (list-ref-safe (rest lst) (sub1 i))]))
         (define (get-cell puzzle row col)
           (list-ref-safe (list-ref-safe puzzle row) col))
         (define (get-cell-or-val puzzle ri col ci val)
           (cond[(= ci col) val]
                [else (list-ref-safe (list-ref-safe puzzle ri) ci)]))
         (define (setting-row puzzle ri col val)
           (build-list (length (list-ref-safe puzzle ri))
                       (lambda (ci) (get-cell-or-val puzzle ri col ci val))))
         (define (set-cell puzzle row col val)
           (build-list (length puzzle) (lambda (ri) (cond[(= ri row) (setting-row puzzle ri col val)]
                                                         [else (list-ref-safe puzzle ri)]))))
         (define (remove-val-from-list val lst)
           (filter (lambda (x) (not (= x val))) lst))
         (define (process-cell-for-row val cell)
           (cond[(list? cell) (remove-val-from-list val cell)]
                [else cell]))
         (define (remove-from-row puzzle row val)
           (build-list (length puzzle) (lambda (ri)
                                         (cond[(= ri row)
                                               (map (lambda (cell) (process-cell-for-row val cell))
                                                    (list-ref-safe puzzle ri))]
                                              [else (list-ref-safe puzzle ri)]))))
         (define (process-cell-for-col val col row ci)
           (cond[(list? (list-ref-safe row ci)) (remove-val-from-list val (list-ref-safe row ci))]
                [else (list-ref-safe row ci)]))
         (define (process-col-cell col val row ci)
           (cond[(= ci col) (process-cell-for-col val col row ci)]
                [else (list-ref-safe row ci)]))
         (define (remove-from-col puzzle col val)
           (map (lambda (row) (build-list (length row)
                                          (lambda (ci) (process-col-cell col val row ci)))) puzzle))]
    (cond[(contains-single? puzzle)
          (local[(define coords (find-single-coords puzzle))
                 (define col (first coords))
                 (define row (second coords))
                 (define val (first (get-cell puzzle row col)))]
            (remove-singles (remove-from-col (remove-from-row (set-cell puzzle row col val) row val)
                                             col val)))]
         [else puzzle])))

(define (yes x) true)
(define (no x) false)

(define (diagonal-has-2? p)
  (and (not (empty? p))
       (or (= 2 (first (first p)))
           (diagonal-has-2? (map rest (rest p))))))

(define (solve-latin pred? puzzle)
  (local[(define partial (remove-singles puzzle))
         (define (get-cell puzzle row col)
           (cond[(= row 0) (get-cell-row (first puzzle) col)]
                [else (get-cell (rest puzzle) (sub1 row) col)]))
         (define (get-cell-row row col)
           (cond[(= col 0) (first row)]
                [else (get-cell-row (rest row) (sub1 col))]))
         (define (set-cell puzzle row col val)
           (cond[(= row 0) (cons (set-cell-row (first puzzle) col val) (rest puzzle))]
                [else (cons (first puzzle) (set-cell (rest puzzle) (sub1 row) col val))]))
         (define (set-cell-row row col val)
           (cond[(= col 0) (cons (list val) (rest row))]
                [else (cons (first row) (set-cell-row (rest row) (sub1 col) val))]))]
    (cond[(not (any-satisfy? (lambda (item) (list? item)) partial))
          (cond[(pred? partial) partial]
               [else empty])]
         [else (local[(define coords (find-where (lambda (item) (list? item)) partial))
                      (define col (first coords))
                      (define row (second coords))
                      (define choices (get-cell partial row col))
                      (define (tester loc)
                        (cond[(empty? loc) empty]
                             [else (local[(define guess (set-cell partial row col (first loc)))
                                          (define result (solve-latin pred? guess))]
                                     (cond[(empty? result) (tester (rest loc))]
                                          [else result]))]))]
                 (tester choices))])))

(define (sudoku? solution)
  (local[(define flatten (lambda (matrix) (foldr append empty matrix)))
         (define (maker master lst1 lst2 lst3 counter)
           (cond[(empty? master) (list lst1 lst2 lst3)]
                [(< counter 3) (maker (rest master) (cons (first master) lst1) lst2 lst3
                                      (add1 counter))]
                [(< counter 6) (maker (rest master) lst1 (cons (first master) lst2) lst3
                                      (add1 counter))]
                [(< counter 9) (maker (rest master) lst1 lst2 (cons (first master) lst3)
                                      (add1 counter))]
                [else (maker (rest master) (cons (first master) lst1) lst2 lst3 1)]))
         (define (maker2 master lst1 lst2 lst3 counter)
           (cond[(empty? master) (list lst1 lst2 lst3)]
                [(< counter 9) (maker2 (rest master) (cons (first master) lst1) lst2 lst3
                                       (add1 counter))]
                [(< counter 18) (maker2 (rest master) lst1 (cons (first master) lst2) lst3
                                        (add1 counter))]
                [(< counter 27) (maker2 (rest master) lst1 lst2 (cons (first master) lst3)
                                        (add1 counter))]))
         (define (in? item lst)
           (cond[(empty? lst) false]
                [(= item (first lst)) true]
                [else (in? item (rest lst))]))
         (define (all-unique? lst)
           (cond[(empty? lst) true]
                [(in? (first lst) (rest lst)) false]
                [else (all-unique? (rest lst))]))
         (define (final-checker lst)
           (cond[(empty? lst) true]
                [(all-unique? (first lst)) (final-checker (rest lst))]
                [else false]))]
    (final-checker (flatten (map (lambda (item) (maker2 item empty empty empty 0))
                                 (maker (flatten solution) empty empty empty 0))))))