(require 2htdp/universe)
(require 2htdp/image)
(require racket/include)
(require compatibility/mlist)

(require racket/mpair)
(require racket/vector)
(require data/gvector)
(require lang/posn)
(require "bitmaps.ss")

(define hard #f)
(define up #f)
(define up1 #t)
(define ai #f)
(define check-undo #t)
(define check-w #t)
(define check-b #t)


(define (vector-remove v n) 
  (vector-append (vector-take v n) (vector-drop v (+ n 1))))

(define all-positions (list (make-posn 0 0) (make-posn 1 0) (make-posn 2 0) (make-posn 3 0) 
                            (make-posn 4 0) (make-posn 5 0) (make-posn 6 0) (make-posn 7 0) 
                            (make-posn 0 1) (make-posn 1 1) (make-posn 2 1) (make-posn 3 1) 
                            (make-posn 4 1) (make-posn 5 1) (make-posn 6 1) (make-posn 7 1) 
                            (make-posn 0 2) (make-posn 1 2) (make-posn 2 2) (make-posn 3 2) 
                            (make-posn 4 2) (make-posn 5 2) (make-posn 6 2) (make-posn 7 2) 
                            (make-posn 0 3) (make-posn 1 3) (make-posn 2 3) (make-posn 3 3)
                            (make-posn 4 3) (make-posn 5 3) (make-posn 6 3) (make-posn 7 3) 
                            (make-posn 0 4) (make-posn 1 4) (make-posn 2 4) (make-posn 3 4) 
                            (make-posn 4 4) (make-posn 5 4) (make-posn 6 4) (make-posn 7 4) 
                            (make-posn 0 5) (make-posn 1 5) (make-posn 2 5) (make-posn 3 5) 
                            (make-posn 4 5) (make-posn 5 5) (make-posn 6 5) (make-posn 7 5)
                            (make-posn 0 6) (make-posn 1 6) (make-posn 2 6) (make-posn 3 6) 
                            (make-posn 4 6) (make-posn 5 6) (make-posn 6 6) (make-posn 7 6) 
                            (make-posn 0 7) (make-posn 1 7) (make-posn 2 7) (make-posn 3 7) 
                            (make-posn 4 7) (make-posn 5 7) (make-posn 6 7) (make-posn 7 7)))

;DEFINING THE NECESSARY STRUCTURES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (pos-to-cord pos)
  (cons (+ (* (posn-x pos) 56.6) 84.9) (+ (* (- 7 (posn-y pos)) 56.6) 84.9)))

(define (sum-fixed-value lst)
  (define (helper l)
    (if (null? l) 0
        (+ (get-field fixed-value (car l)) (helper (cdr l)))))
  (helper lst))

(define (sum-value lst)
  (define (helper i acc)
    (if (= i -1) acc
        (helper (- i 1) (+ (get-field value (vector-ref lst i)) acc))))
  (helper (- (vector-length lst) 1) 0))


(define (posn=? p1 p2)
  (and (= (posn-x p1) (posn-x p2)) (= (posn-y p1) (posn-y p2))))

(define (get posn0 l)
  (mlist-ref (mlist-ref l (posn-y posn0)) (posn-x posn0)))

(define piece%
  (class object%
    (init-field name)
    (init-field color)
    (init-field position)
    (init-field value)
    (init-field fixed-value)
    (init-field [image 0])
    (init-field [farthest '()])
    (init-field [valid-moves '()])
    (super-new)
    (define/public (update-valid-moves)
      (set! valid-moves (filter (possible-attacks this) all-positions)))
    (define/public (show back)
      (let ((cordi (pos-to-cord position)))
        (place-image image (car cordi) (cdr cordi) back)))
    (define/public (move to-posn)
      (begin
        (set! last-move (list this position to-posn (get to-posn board-conf)))
        (set! history (cons last-move history))
        
        (cond [(number? (get to-posn board-conf)) 
               (if (and (string=? (get-field name (car last-move)) "pawn")
                        (or (= (posn-y to-posn) 7) (= (posn-y to-posn) 0))) 
                   (begin (set-field! name (car last-move) "queen")
                          (set-field! image (car last-move) 
                                      (if (string=? (get-field color (car last-move)) "black") b-queen w-queen))
                          (update-all-farthest-initial))
                   (void))]
              [(string=? (get-field color (get to-posn board-conf)) "white")
               (if (and (string=? (get-field name (car last-move)) "pawn") (= (posn-y to-posn) 0)) 
                   (begin (set-field! name (car last-move) "queen")
                          (set-field! image (car last-move) 
                                      (if (string=? (get-field color (car last-move)) "black") b-queen w-queen))
                          (update-all-farthest-initial)
                          (set! alive-w (vector-remove alive-w (vector-member (get to-posn board-conf) alive-w))))
                   (set! alive-w (vector-remove alive-w (vector-member (get to-posn board-conf) alive-w))))]
              [(string=? (get-field color (get to-posn board-conf)) "black")
               (if (and (string=? (get-field name (car last-move)) "pawn") (= (posn-y to-posn) 7)) 
                   (begin (set-field! name (car last-move) "queen")
                          (set-field! image (car last-move) 
                                      (if (string=? (get-field color (car last-move)) "black") b-queen w-queen))
                          (update-all-farthest-initial)
                          (set! alive-b (vector-remove alive-b (vector-member (get to-posn board-conf) alive-b))))
                   (set! alive-b (vector-remove alive-b (vector-member (get to-posn board-conf) alive-b))))]
              ) 
        (change-two-d to-posn board-conf this)
        (change-two-d position board-conf 0)
        (set! position to-posn)
        (update-all-farthest-initial)
        (set! farthest (farthest-l this))
        (if check-undo (update-valid-moves) (void))
        ))
    
    (define/public (move1 to-posn)
      (begin
        (if up 
            (void)
            (begin
              (set! last-move (list this position to-posn (get to-posn board-conf)))
              (set! history (cons last-move history))))
        (if (number? (get to-posn board-conf)) (void)
            (if (string=? (get-field color (get to-posn board-conf)) "white") 
                (set! alive-w (vector-remove alive-w (vector-member (get to-posn board-conf) alive-w)))
                (set! alive-b (vector-remove alive-b (vector-member (get to-posn board-conf) alive-b))))) 
        (change-two-d to-posn board-conf this)
        (change-two-d position board-conf 0)
        (set! position to-posn)))
    
    (define/public (update-value)
      (define (for-kt)
        (filter (λ (x) (if ((attacks this) x)
                           (if (number? (get x board-conf)) #f (and
                                                                (not (string=? "king" (get-field name (get x board-conf))))
                                                                (string=? color (get-field color (get x board-conf)))))
                           #f)) all-positions))
      (define (for-p)
        (filter (λ (x) (if ((supports this) x)
                           (if (number? (get x board-conf)) #f (and
                                                                (not (string=? "king" (get-field name (get x board-conf))))
                                                                (string=? color (get-field color (get x board-conf)))))
                           #f)) all-positions))
      (cond
        [(or (string=? name "queen") (string=? name "rook") (string=? name "bishop"))
         (set! value (+ fixed-value (/ (sum-fixed-value (filter (λ (x) (if (posn? x) #f (and
                                                                                         (not (string=? "king" (get-field name x)))
                                                                                         (string=? color (get-field color x))))) (mlist->list farthest))) 20)))]
        [(string=? name "knight") (set! value (+ fixed-value (/ (sum-fixed-value (map (λ (x) (get x board-conf)) (for-kt))) 20)))]
        [(string=? name "pawn") (set! value (+ fixed-value (/ (sum-fixed-value (map (λ (x) (get x board-conf)) (for-p))) 20)))]
        ))))



;THE PLACES WHERE THE PIECE CAN ATTACK WHEN THE BOARD IS EMPTY ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (attacks piece0)
  (let* ([posn1 (get-field position piece0)])
    (cond
      [(string=? "queen" (get-field name piece0)) 
       (λ (posn2) (or (= (posn-x posn1) (posn-x posn2)) (= (posn-y posn1) (posn-y posn2))
                      (= (abs (- (posn-x posn1) (posn-x posn2))) (abs (- (posn-y posn1) (posn-y posn2))))))]
      [(string=? "rook" (get-field name piece0)) 
       (λ (posn2) (or (= (posn-x posn1) (posn-x posn2)) (= (posn-y posn1) (posn-y posn2))))]
      [(string=? "bishop" (get-field name piece0)) 
       (λ (posn2) (= (abs (- (posn-x posn1) (posn-x posn2))) (abs (- (posn-y posn1) (posn-y posn2)))))]
      [(string=? "knight" (get-field name piece0)) 
       (λ (posn2) (or (and (= (abs (- (posn-x posn1) (posn-x posn2))) 2) (= (abs (- (posn-y posn1) (posn-y posn2))) 1))
                      (and (= (abs (- (posn-x posn1) (posn-x posn2))) 1) (= (abs (- (posn-y posn1) (posn-y posn2))) 2))))]
      [(string=? "pawn" (get-field name piece0))
       (if (eq? (get-field color piece0) "white")
           (λ (posn2) (if (= (posn-y posn1) 1) 
                          (and (= (posn-y posn2) 3) (= (posn-x posn2) (posn-x posn1)))
                          (and (= (- (posn-y posn2) (posn-y posn1)) 1) (<= (abs (- (posn-x posn1) (posn-x posn2))) 1))))
           (λ (posn2) (if (= (posn-y posn1) 6)
                          (and (= (posn-y posn2) 4) (= (posn-x posn2) (posn-x posn1)))
                          (and (= (- (posn-y posn1) (posn-y posn2)) 1) (<= (abs (- (posn-x posn1) (posn-x posn2))) 1)))))]
      [(string=? "king" (get-field name piece0)) 
       (λ (posn2) (and (<= (abs (- (posn-x posn1) (posn-x posn2))) 1) (<= (abs (- (posn-y posn1) (posn-y posn2))) 1)))])))

;THE PLACES WHERE THE PIECE CAN ATTACK FOR THE GIVEN BOARD-CONFIGURTION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (possible-attacks piece0)
  (define (for-q/b/r piece1 piece2)
    (λ (posn3)
      (let* ([x3 (posn-x posn3)]
             [y3 (posn-y posn3)]
             [x1 (posn-x (if (posn? piece1) piece1 (get-field position piece1)))]
             [y1 (posn-y (if (posn? piece1) piece1 (get-field position piece1)))]
             [x2 (posn-x (if (posn? piece2) piece2 (get-field position piece2)))]
             [y2 (posn-y (if (posn? piece2) piece2 (get-field position piece2)))])
        (cond
          [(or (and (< x3 x2) (< x3 x1)) (and (< y3 y2) (< y3 y1)) (and (> x3 x2) (> x3 x1)) (and (> y3 y2) (> y3 y1))) #f]
          [(posn=? posn3 (get-field position piece0)) #f]
          [(posn=? posn3 (make-posn x1 y1)) 
           (if (posn? piece1) #t (if (string=? (get-field color piece0) (get-field color piece1)) #f #t))]
          [(posn=? posn3 (make-posn x2 y2)) 
           (if (posn? piece2) #t (if (string=? (get-field color piece0) (get-field color piece2)) #f #t))]
          [(= x2 x1) (= x3 x1)]
          [(= y2 y1) (= y3 y1)]
          [(= (abs (/ (- x3 x2) (- x2 x1))) (abs (/ (- y3 y2) (- y2 y1)))) #t]
          [else #f]))))
  
  (define (for-pawn)
    (if (eq? (get-field color piece0) "white")
        (λ (posn1)
          (let* ([x1 (posn-x posn1)]
                 [y1 (posn-y posn1)]
                 [posn0 (get-field position piece0)]
                 [x0 (posn-x posn0)]
                 [y0 (posn-y posn0)])
            (cond
              [(posn=? posn1 (make-posn x0 (+ y0 1))) (not (object? (mlist-ref (mlist-ref board-conf (+ y0 1)) x0)))]
              [(and (= y0 1) 
                    (posn=? posn1 (make-posn x0 (+ y0 2))) 
                    (not (object? (mlist-ref (mlist-ref board-conf (+ y0 1)) x0))))
               (not (object? (mlist-ref (mlist-ref board-conf (+ y0 2)) x0)))]
              [(or (posn=? posn1 (make-posn (+ x0 1) (+ y0 1))) (posn=? posn1 (make-posn (- x0 1) (+ y0 1))))               
               (if (object? (mlist-ref (mlist-ref board-conf y1) x1))
                   (if (string=? (get-field color (mlist-ref (mlist-ref board-conf y1) x1)) "black") #t #f)
                   #f)]
              [else #f])))
        (λ (posn1)
          (let* ([x1 (posn-x posn1)]
                 [y1 (posn-y posn1)]
                 [posn0 (get-field position piece0)]
                 [x0 (posn-x posn0)]
                 [y0 (posn-y posn0)])
            (cond
              [(posn=? posn1 (make-posn x0 (- y0 1))) (not (object? (get (make-posn x0 (- y0 1)) board-conf)))]
              [(and (= y0 6) (posn=? posn1 (make-posn x0 (- y0 2))) (not (object? (get (make-posn x0 (- y0 1)) board-conf))))
               (not (object? (get (make-posn x0 (- y0 2)) board-conf)))]
              [(or (posn=? posn1 (make-posn (+ x0 1) (- y0 1))) (posn=? posn1 (make-posn (- x0 1) (- y0 1))))
               (if (object? (mlist-ref (mlist-ref board-conf y1) x1))
                   (if (string=? (get-field color (mlist-ref (mlist-ref board-conf y1) x1)) "white") #t #f)
                   #f)]
              [else #f])))))
  
  (define (for-knight)
    (λ (posn1) (let* ([posn0 (get-field position piece0)]
                      [x0 (posn-x posn0)]
                      [y0 (posn-y posn0)]
                      [x1 (posn-x posn1)]
                      [y1 (posn-y posn1)]
                      )
                 (cond [(posn=? posn0 posn1) #f]
                       [(or (and (= (abs (- x1 x0)) 1) (= (abs (- y1 y0)) 2)) 
                            (and (= (abs (- x1 x0)) 2) (= (abs (- y1 y0)) 1)))
                        (if (object? (mlist-ref (mlist-ref board-conf y1) x1))
                            (if (string=? (get-field color piece0) 
                                          (get-field color (mlist-ref (mlist-ref board-conf y1) x1))) #f #t)
                            #t)]
                       [else #f]
                       )
                 )
      )
    )
  (define (for-king)
    (λ (posn1)
      (let* ([posn0 (get-field position piece0)]
             [x0 (posn-x posn0)]
             [y0 (posn-y posn0)]
             [color0 (get-field color piece0)]
             [x1 (posn-x posn1)]
             [y1 (posn-y posn1)]
             )
        (cond [(posn=? posn0 posn1) #f]
              [(and (<= (abs (- (posn-x posn1) (posn-x posn0))) 1) (<= (abs (- (posn-y posn1) (posn-y posn0))) 1))
               (if (object? (mlist-ref (mlist-ref board-conf y1) x1))
                   (not (equal? color0 (get-field color (mlist-ref (mlist-ref board-conf y1) x1))))
                   #t) 
               ]
              [else #f]
              )
        ))
    )
  (let* ([name0 (get-field name piece0)])
    (cond
      [(string=?  "queen" name0)
       (let* ([fl (get-field farthest piece0)])
         (λ (posn3)
           (or
            ((for-q/b/r (mlist-ref fl 0) (mlist-ref fl 1)) posn3)
            ((for-q/b/r (mlist-ref fl 2) (mlist-ref fl 3)) posn3)
            ((for-q/b/r (mlist-ref fl 4) (mlist-ref fl 5)) posn3)
            ((for-q/b/r (mlist-ref fl 6) (mlist-ref fl 7)) posn3)
            )))]
      [(or (string=? name0 "rook") (string=? name0 "bishop"))
       (let* ([fl (farthest-l piece0)])
         (λ (posn3)
           (or
            ((for-q/b/r (mlist-ref fl 0) (mlist-ref fl 1)) posn3)
            ((for-q/b/r (mlist-ref fl 2) (mlist-ref fl 3)) posn3)))
         )]
      [(string=? name0 "knight")
       (for-knight)]
      [(string=? name0 "pawn")
       (for-pawn)]
      [(string=? name0 "king")
       (for-king)]
      )
    ))


;POSITIONS WHICH THE PIECE CAN SUPPORT;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (supports piece0)
  (define (for-q/b/r piece1 piece2)
    (λ (posn3)
      (let* ([x3 (posn-x posn3)]
             [y3 (posn-y posn3)]
             [x1 (posn-x (if (posn? piece1) piece1 (get-field position piece1)))]
             [y1 (posn-y (if (posn? piece1) piece1 (get-field position piece1)))]
             [x2 (posn-x (if (posn? piece2) piece2 (get-field position piece2)))]
             [y2 (posn-y (if (posn? piece2) piece2 (get-field position piece2)))])
        (cond
          [(or (and (< x3 x2) (< x3 x1)) (and (< y3 y2) (< y3 y1)) (and (> x3 x2) (> x3 x1)) (and (> y3 y2) (> y3 y1))) #f]
          [(posn=? posn3 (get-field position piece0)) #f]
          [(= x2 x1) (= x3 x1)]
          [(= y2 y1) (= y3 y1)]
          [(= (abs (/ (- x3 x2) (- x2 x1))) (abs (/ (- y3 y2) (- y2 y1)))) #t]
          [else #f]))))
  
  (define (for-pawn)
    (λ (posn1)
      (let* ([x1 (posn-x posn1)]
             [y1 (posn-y posn1)]
             [posn0 (get-field position piece0)]
             [x0 (posn-x posn0)]
             [y0 (posn-y posn0)])
        (cond
          [(string=? (get-field color piece0) "white") 
           (or (posn=? posn1 (make-posn (+ x0 1) (+ y0 1))) (posn=? posn1 (make-posn (- x0 1) (+ y0 1))))]
          [else (or (posn=? posn1 (make-posn (+ x0 1) (- y0 1))) (posn=? posn1 (make-posn (- x0 1) (- y0 1))))]))))
  
  (define (for-knight)
    (attacks piece0)
    )
  (define (for-king)
    (attacks piece0)
    )
  (let* ([name0 (get-field name piece0)])
    (cond
      [(string=?  "queen" name0)
       (let* ([fl (get-field farthest piece0)])
         (λ (posn3)
           (or
            ((for-q/b/r (mlist-ref fl 0) (mlist-ref fl 1)) posn3)
            ((for-q/b/r (mlist-ref fl 2) (mlist-ref fl 3)) posn3)
            ((for-q/b/r (mlist-ref fl 4) (mlist-ref fl 5)) posn3)
            ((for-q/b/r (mlist-ref fl 6) (mlist-ref fl 7)) posn3)
            )))]
      [(or (string=? name0 "rook") (string=? name0 "bishop"))
       (let* ([fl (farthest-l piece0)])
         (λ (posn3)
           (or
            ((for-q/b/r (mlist-ref fl 0) (mlist-ref fl 1)) posn3)
            ((for-q/b/r (mlist-ref fl 2) (mlist-ref fl 3)) posn3)))
         )]
      [(string=? name0 "knight")
       (for-knight)]
      [(string=? name0 "pawn")
       (for-pawn)]
      [(string=? name0 "king")
       (for-king)]
      )
    ))


;THE FARTHEST PLACE WEHRE THE PIECE CAN GO;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                                                    
(define (farthest-d piece0 dirx diry)
  (define (farthest-d-h x y)
    (cond
      [(or (> x 7) (< x 0) (> y 7) (< y 0)) (make-posn (- x dirx) (- y diry))]
      [(and (= x (posn-x (get-field position piece0))) 
            (= y (posn-y (get-field position piece0)))) (farthest-d-h (+ x dirx) (+ y diry))]
      [(object? (mlist-ref (mlist-ref board-conf y) x)) (mlist-ref (mlist-ref board-conf y) x)]
      [else (farthest-d-h (+ x dirx) (+ y diry))]))
  (farthest-d-h (posn-x (get-field position piece0)) (posn-y (get-field position piece0)))
  )

(define (farthest-l piece0)
  (cond [(string=? (get-field name piece0) "pawn") '()]
        [(string=? (get-field name piece0) "knight") '()]
        [(string=? (get-field name piece0) "king") '()]
        [(string=? (get-field name piece0) "rook") 
         (list->mlist (map (λ (x) (farthest-d piece0 (car x) (cadr x))) '((1 0) (-1 0) (0 1) (0 -1))))] 
        [(string=? (get-field name piece0) "bishop") 
         (list->mlist (map (λ (x) (farthest-d piece0 (car x) (cadr x))) '((1 1) (-1 -1) (-1 1) (1 -1))))]
        [(string=? (get-field name piece0) "queen") 
         (list->mlist (map (λ (x) (farthest-d piece0 (car x) (cadr x))) '((1 0) (-1 0) (0 1) (0 -1) (1 1) (-1 -1) (-1 1) (1 -1))))]
        )
  )




;DEFINING THE PIECES;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;ROOKS
(define rook-wl (new piece% [name "rook"] [color "white"] [position (make-posn  0 0)] [value 6] [fixed-value 6] [image w-rook]))
(define rook-wr (new piece% [name "rook"] [color "white"] [position (make-posn  7 0)] [value 6] [fixed-value 6] [image w-rook]))
(define rook-bl (new piece% [name "rook"] [color "black"] [position (make-posn  0 7)] [value 6] [fixed-value 6] [image b-rook]))
(define rook-br (new piece% [name "rook"] [color "black"] [position (make-posn  7 7)] [value 6] [fixed-value 6] [image b-rook]))
;KNIGHTS
(define knight-wl (new piece% [name "knight"] [color "white"] [position (make-posn 1 0)] [value 3.5] [fixed-value 3.5] [image w-knight]))
(define knight-wr (new piece% [name "knight"] [color "white"] [position (make-posn 6 0)] [value 3.5] [fixed-value 3.5] [image w-knight]))
(define knight-bl (new piece% [name "knight"] [color "black"] [position (make-posn 1 7)] [value 3.5] [fixed-value 3.5] [image b-knight]))
(define knight-br (new piece% [name "knight"] [color "black"] [position (make-posn 6 7)] [value 3.5] [fixed-value 3.5] [image b-knight]))
;BISHOPS
(define bishop-wl (new piece% [name "bishop"] [color "white"] [position (make-posn 2 0)] [value 3] [fixed-value 3] [image w-bishop]))
(define bishop-wr (new piece% [name "bishop"] [color "white"] [position (make-posn 5 0)] [value 3] [fixed-value 3] [image w-bishop]))
(define bishop-bl (new piece% [name "bishop"] [color "black"] [position (make-posn 2 7)] [value 3] [fixed-value 3] [image b-bishop]))
(define bishop-br (new piece% [name "bishop"] [color "black"] [position (make-posn 5 7)] [value 3] [fixed-value 3] [image b-bishop]))
;QUEEN
(define queen-w (new piece% [name "queen"] [color "white"] [position (make-posn 3 0)] [value 9] [fixed-value 9] [image w-queen]))
(define queen-b (new piece% [name "queen"] [color "black"] [position (make-posn 3 7)] [value 9] [fixed-value 9] [image b-queen]))
;KING
(define king-w (new piece% [name "king"] [color "white"] [position (make-posn 4 0)] [value 200] [fixed-value 200] [image w-king]))
(define king-b (new piece% [name "king"] [color "black"] [position (make-posn 4 7)] [value 225] [fixed-value 225] [image b-king]))
;PAWN
(define pawn-w0 (new piece% [name "pawn"] [color "white"] [position (make-posn 0 1)] [value 1] [fixed-value 1] [image w-pawn]))
(define pawn-w1 (new piece% [name "pawn"] [color "white"] [position (make-posn 1 1)] [value 1] [fixed-value 1] [image w-pawn]))
(define pawn-w2 (new piece% [name "pawn"] [color "white"] [position (make-posn 2 1)] [value 1] [fixed-value 1] [image w-pawn]))
(define pawn-w3 (new piece% [name "pawn"] [color "white"] [position (make-posn 3 1)] [value 1] [fixed-value 1] [image w-pawn]))
(define pawn-w4 (new piece% [name "pawn"] [color "white"] [position (make-posn 4 1)] [value 1] [fixed-value 1] [image w-pawn]))
(define pawn-w5 (new piece% [name "pawn"] [color "white"] [position (make-posn 5 1)] [value 1] [fixed-value 1] [image w-pawn]))
(define pawn-w6 (new piece% [name "pawn"] [color "white"] [position (make-posn 6 1)] [value 1] [fixed-value 1] [image w-pawn]))
(define pawn-w7 (new piece% [name "pawn"] [color "white"] [position (make-posn 7 1)] [value 1] [fixed-value 1] [image w-pawn]))
(define pawn-b0 (new piece% [name "pawn"] [color "black"] [position (make-posn 0 6)] [value 1] [fixed-value 1] [image b-pawn]))
(define pawn-b1 (new piece% [name "pawn"] [color "black"] [position (make-posn 1 6)] [value 1] [fixed-value 1] [image b-pawn]))
(define pawn-b2 (new piece% [name "pawn"] [color "black"] [position (make-posn 2 6)] [value 1] [fixed-value 1] [image b-pawn]))
(define pawn-b3 (new piece% [name "pawn"] [color "black"] [position (make-posn 3 6)] [value 1] [fixed-value 1] [image b-pawn]))
(define pawn-b4 (new piece% [name "pawn"] [color "black"] [position (make-posn 4 6)] [value 1] [fixed-value 1] [image b-pawn]))
(define pawn-b5 (new piece% [name "pawn"] [color "black"] [position (make-posn 5 6)] [value 1] [fixed-value 1] [image b-pawn]))
(define pawn-b6 (new piece% [name "pawn"] [color "black"] [position (make-posn 6 6)] [value 1] [fixed-value 1] [image b-pawn]))
(define pawn-b7 (new piece% [name "pawn"] [color "black"] [position (make-posn 7 6)] [value 1] [fixed-value 1] [image b-pawn]))


;THE STATE VARIABLES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define attack-w (mlist (mlist (vector)  (vector)  (vector)  (vector)  (vector)  (vector)  (vector)  (vector) )
                        (mlist (vector)  (vector)  (vector)  (vector)  (vector)  (vector)  (vector)  (vector) )
                        (mlist (vector)  (vector)  (vector)  (vector)  (vector)  (vector)  (vector)  (vector) )
                        (mlist (vector)  (vector)  (vector)  (vector)  (vector)  (vector)  (vector)  (vector) )
                        (mlist (vector)  (vector)  (vector)  (vector)  (vector)  (vector)  (vector)  (vector) )
                        (mlist (vector)  (vector)  (vector)  (vector)  (vector)  (vector)  (vector)  (vector) )
                        (mlist (vector)  (vector)  (vector)  (vector)  (vector)  (vector)  (vector)  (vector) )
                        (mlist (vector)  (vector)  (vector)  (vector)  (vector)  (vector)  (vector)  (vector) )));instead of (vector)  there will be a vector of all attackers at that position
(define attack-b (mlist (mlist (vector)  (vector)  (vector)  (vector)  (vector)  (vector)  (vector)  (vector) )
                        (mlist (vector)  (vector)  (vector)  (vector)  (vector)  (vector)  (vector)  (vector) )
                        (mlist (vector)  (vector)  (vector)  (vector)  (vector)  (vector)  (vector)  (vector) )
                        (mlist (vector)  (vector)  (vector)  (vector)  (vector)  (vector)  (vector)  (vector) )
                        (mlist (vector)  (vector)  (vector)  (vector)  (vector)  (vector)  (vector)  (vector) )
                        (mlist (vector)  (vector)  (vector)  (vector)  (vector)  (vector)  (vector)  (vector) )
                        (mlist (vector)  (vector)  (vector)  (vector)  (vector)  (vector)  (vector)  (vector) )
                        (mlist (vector)  (vector)  (vector)  (vector)  (vector)  (vector)  (vector)  (vector) )))

(define supports-w (mlist (mlist (vector)  (vector)  (vector)  (vector)  (vector)  (vector)  (vector)  (vector) )
                          (mlist (vector)  (vector)  (vector)  (vector)  (vector)  (vector)  (vector)  (vector) )
                          (mlist (vector)  (vector)  (vector)  (vector)  (vector)  (vector)  (vector)  (vector) )
                          (mlist (vector)  (vector)  (vector)  (vector)  (vector)  (vector)  (vector)  (vector) )
                          (mlist (vector)  (vector)  (vector)  (vector)  (vector)  (vector)  (vector)  (vector) )
                          (mlist (vector)  (vector)  (vector)  (vector)  (vector)  (vector)  (vector)  (vector) )
                          (mlist (vector)  (vector)  (vector)  (vector)  (vector)  (vector)  (vector)  (vector) )
                          (mlist (vector)  (vector)  (vector)  (vector)  (vector)  (vector)  (vector)  (vector) )))
(define supports-b (mlist (mlist (vector)  (vector)  (vector)  (vector)  (vector)  (vector)  (vector)  (vector) )
                          (mlist (vector)  (vector)  (vector)  (vector)  (vector)  (vector)  (vector)  (vector) )
                          (mlist (vector)  (vector)  (vector)  (vector)  (vector)  (vector)  (vector)  (vector) )
                          (mlist (vector)  (vector)  (vector)  (vector)  (vector)  (vector)  (vector)  (vector) )
                          (mlist (vector)  (vector)  (vector)  (vector)  (vector)  (vector)  (vector)  (vector) )
                          (mlist (vector)  (vector)  (vector)  (vector)  (vector)  (vector)  (vector)  (vector) )
                          (mlist (vector)  (vector)  (vector)  (vector)  (vector)  (vector)  (vector)  (vector) )
                          (mlist (vector)  (vector)  (vector)  (vector)  (vector)  (vector)  (vector)  (vector) )))

(define board-conf (mlist (mlist rook-wl knight-wl bishop-wl queen-w king-w bishop-wr knight-wr rook-wr)
                          (mlist pawn-w0 pawn-w1 pawn-w2 pawn-w3 pawn-w4 pawn-w5 pawn-w6 pawn-w7)
                          (mlist 0 0 0 0 0 0 0 0)
                          (mlist 0 0 0 0 0 0 0 0)
                          (mlist 0 0 0 0 0 0 0 0)
                          (mlist 0 0 0 0 0 0 0 0)
                          (mlist pawn-b0 pawn-b1 pawn-b2 pawn-b3 pawn-b4 pawn-b5 pawn-b6 pawn-b7)
                          (mlist rook-bl knight-bl bishop-bl queen-b king-b bishop-br knight-br rook-br)))
(define last-move '())
(define alive-w (vector  pawn-w0 pawn-w1 pawn-w2 pawn-w3 pawn-w4 pawn-w5 pawn-w6 pawn-w7
                         rook-wl knight-wl bishop-wl queen-w king-w bishop-wr knight-wr rook-wr))
(define alive-b (vector pawn-b0 pawn-b1 pawn-b2 pawn-b3 pawn-b4 pawn-b5 pawn-b6 pawn-b7
                        rook-bl knight-bl bishop-bl queen-b king-b bishop-br knight-br rook-br))

(define history '())


;UPDATING STATE VARIABLES AFTER EVERY MOVE;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (update-attack-supports-w)
  (define (uaw1 i)
    (if (= i 64) (void)
        (let* ([r (quotient i 8)]
               [c (remainder i 8)])
          (begin
            (change-two-d (make-posn c r) attack-w (vector-filter (lambda (p) ((possible-attacks p) (make-posn c r))) alive-w))
            (change-two-d (make-posn c r) supports-w (vector-filter (lambda (p) ((supports p) (make-posn c r))) alive-w))
            (uaw1 (+ i 1))))))
  (uaw1 0))

(define (update-attack-supports-b)
  (define (uab1 i)
    (if (= i 63) (void)
        (let* ([r (quotient i 8)]
               [c (remainder i 8)])
          (begin
            (change-two-d (make-posn c r) attack-b (vector-filter (lambda (p) ((possible-attacks p) (make-posn c r))) alive-b))
            (change-two-d (make-posn c r) supports-b (vector-filter (lambda (p) ((supports p) (make-posn c r))) alive-b))
            (uab1 (+ i 1))))))
  (uab1 0))

(define (update-all-farthest)
  (let* ((initial-posn (cadr last-move))
         (last-part (car last-move))
         (final-posn (get-field position last-part)))
    
    (define (call-each-aliveb iter)
      (if (= iter -1) (void)
          (let* ((curr (vector-ref alive-b iter)))
            (begin (if (or ((possible-attacks curr) initial-posn)
                           ((possible-attacks curr) final-posn)
                           ((supports curr) final-posn)
                           ((supports curr) initial-posn)
                           (mmember last-part (get-field farthest curr))
                           (string=? (get-field name curr) "king"))
                       (begin
                         (set-field! farthest curr (farthest-l curr))
                         (send curr update-valid-moves))
                       (void)
                       )
                   (call-each-aliveb (- iter 1))))))
    (define (call-each-alivew iter)
      (if (= iter -1) (void)
          (let* ((curr (vector-ref alive-w iter)))
            (begin (if (or ((possible-attacks curr) initial-posn)
                           ((possible-attacks curr) final-posn)
                           ((supports curr) final-posn)
                           ((supports curr) initial-posn)
                           (mmember last-part (get-field farthest curr))
                           (string=? (get-field name curr) "king"))
                       (begin
                         (set-field! farthest curr (farthest-l curr))
                         (send curr update-valid-moves))
                       (void)
                       )
                   (call-each-alivew (- iter 1))))))
    (begin
      (call-each-aliveb (- (vector-length alive-b) 1))
      (call-each-alivew (- (vector-length alive-w) 1)))
    ))


(define (update-all-farthest-initial)
  (define (uafi1b i)
    (if (= i (vector-length alive-b)) (void)
        (begin
          (set-field! farthest (vector-ref alive-b i) (farthest-l (vector-ref alive-b i)))
          (send (vector-ref alive-b i) update-valid-moves)
          (uafi1b (+ i 1)))))
  
  
  (define (uafi1w i)
    (if (= i (vector-length alive-w)) (void)
        (begin
          (set-field! farthest (vector-ref alive-w i) (farthest-l (vector-ref alive-w i)))
          (send (vector-ref alive-w i) update-valid-moves)
          (uafi1w (+ i 1)))))
  (begin
    (if check-b (uafi1b 0) (void))
    (if check-w (uafi1w 0) (void))))

(define (update-all-farthest-only)
  (define (uafi1b i)
    (if (= i (vector-length alive-b)) (void)
        (begin
          (set-field! farthest (vector-ref alive-b i) (farthest-l (vector-ref alive-b i)))
          (uafi1b (+ i 1)))))
  
  
  (define (uafi1w i)
    (if (= i (vector-length alive-w)) (void)
        (begin
          (set-field! farthest (vector-ref alive-w i) (farthest-l (vector-ref alive-w i)))
          (uafi1w (+ i 1)))))
  (begin
    (if check-b (uafi1b 0) (void))
    (if check-w (uafi1w 0) (void))))

;;Change
(define (change-one-d ml i new-val)
  (if (= i 0) 
      (set-mcar! ml new-val)               
      (mcons (mcar ml) (change-one-d (mcdr ml) (- i 1) new-val)))
  )

(define (change-two-d posn1 mli new-val)
  (change-one-d (mlist-ref  mli (posn-y posn1)) (posn-x posn1) new-val))

(update-all-farthest-initial)

(update-attack-supports-w)
(update-attack-supports-b)


;CONDITION FOR CHECK AND CHECKMATE;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (check-condition)
  (begin (update-all-farthest-only) (update-attack-supports-w) (update-attack-supports-b)
         (let* ([w-king-posn (get-field position king-w)]
                [b-king-posn (get-field position king-b)]
                [attacks-on-w (get w-king-posn attack-b)]
                [attacks-on-b (get b-king-posn attack-w)])
           (cond
             [(not (equal? attacks-on-w (vector))) "check to white"]
             [(not (equal? attacks-on-b (vector))) "check to black"]
             [else #f]))))

(define chek #t)

(define (update-valid-moves-check-b)
  (define checkmate #t)
  (define temp #f)
  (define (uvmcb i)
    (if (= i (vector-length alive-b)) (void)
        (begin
          (set-field! valid-moves (vector-ref alive-b i)
                      (filter (λ (x) (begin (send (vector-ref alive-b i) move1 x)
                                            (update-all-farthest-initial)
                                            (set! temp (if chek (not (eq? (check-condition) "check to black")) 
                                                           (eq? (check-condition) #f)))
                                            (undo-last-move)
                                            temp)) (get-field valid-moves (vector-ref alive-b i))))
          (if (null? (get-field valid-moves (vector-ref alive-b i))) (void) (set! checkmate #f))
          (uvmcb (+ i 1)))))
  (begin (update-all-farthest-initial) 
         (set! check-undo #f) 
         (set! check-b #f) 
         (uvmcb 0) 
         (set! check-b #t) 
         (set! check-undo #t)
         (update-all-farthest-only)
         checkmate))

(define (update-valid-moves-check-w)
  (define checkmate #t)
  (define temp #f)
  (define (uvmcw i)
    (if (= i (vector-length alive-w)) (void)
        (begin
          (set-field! valid-moves (vector-ref alive-w i)
                      (filter (λ (x) (begin (send (vector-ref alive-w i) move1 x)
                                            (update-all-farthest-initial)
                                            (set! temp 
                                                  (if chek (not (eq? (check-condition) "check to white")) (eq? (check-condition) #f)))
                                            (undo-last-move)
                                            temp)) 
                              (get-field valid-moves (vector-ref alive-w i))))
          (if (null? (get-field valid-moves (vector-ref alive-w i))) (void) (set! checkmate #f))
          (uvmcw (+ i 1)))))
  (begin (update-all-farthest-initial) 
         (set! check-undo #f) 
         (set! check-w #f) 
         (uvmcw 0) 
         (set! check-w #t) 
         (set! check-undo #t)
         (update-all-farthest-only)
         checkmate))

(define (check-mate-condition clr)
  (define checkmate0 #f)
  (define stale-mate #f)
  (let* ([c (check-condition)])
    (begin
      (cond
        [(eq? c "check to white") (set! checkmate0 (begin (set! chek #f ) (update-valid-moves-check-w)))]
        [(eq? c "check to black") (set! checkmate0 (begin (set! chek #f) (update-valid-moves-check-b)))]
        [else (if (string=? clr "white") (set! stale-mate (update-valid-moves-check-w))
                  (set! stale-mate (update-valid-moves-check-b)))])
      (set! chek #t)
      (if stale-mate "stale-mate" checkmate0))))




;UNDO LAST MOVE FUNCTION;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (undo-last-move)
  (let* ([last-part (car last-move)]
         [initial-posn (cadr last-move)]
         [final-posn (caddr last-move)]
         [killed (cadddr last-move)])
    (begin
      (if (number? killed) (void)
          (if (string=? (get-field color killed) "white") (set! alive-w (vector-append (vector killed) alive-w))
              (set! alive-b (vector-append (vector killed) alive-b))))
      (set! up #t)
      (send last-part move1 initial-posn)
      (set! up #f)
      (change-two-d final-posn board-conf killed)
      (set! history (cdr history))
      (set! last-move (if (null? history) '() (car history))))))

;DEFINNING EVALUATION FUNCTION;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (update-all-values)
  (define (uavb i)
    (if (= i (vector-length alive-b)) (void)
        (begin
          (send (vector-ref alive-b i) update-value)
          (uavb (+ i 1)))))
  
  
  (define (uavw i)
    (if (= i (vector-length alive-w)) (void)
        (begin
          (send (vector-ref alive-w i) update-value)
          (uavw (+ i 1)))))
  (begin
    (uavb 0)
    (uavw 0)))
(define (evaluation)
  (begin
    (if hard (update-all-values) (void))
    (- (sum-value (if (string=? comp-clr "white") alive-w alive-b))
       (sum-value (if (string=? comp-clr "white") alive-b alive-w)))))


;FOR ONE PLAYER;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define comp-clr "black")
(define user-clr "white")
(define no-moves 0)
(define n 2)

(define (change-n)
  (cond
    [(= no-moves 10) (begin (set! n 2) (set! no-moves (+ no-moves 1)))]
    [(< no-moves 10) (set! no-moves (+ no-moves 1))]))

;for making list of all possible moves given the list of alive pieces;;;;;;;;;;;;;
(define (make-list col)
  (define (combine l)
    (if (null? l) '()
        (append (car l) (combine (cdr l)))))
  (define f1
    (λ (x)
      (map (f2 x) (get-field valid-moves x))))
  (define f2
    (λ (x)
      (λ (y)
        (list x (get-field position x) y (get y board-conf)))))
  (begin (update-all-farthest-initial) (send king-w update-valid-moves) (combine (map f1 (vector->list (if (string=? col "white") alive-w alive-b))))))

(define-struct tree (val subtrees-list))
(define-struct leaf (val))

(define stop (make-vector (+ n 1) #f))
(define val-max (make-vector (+ n 1) 0))
  
(define (change-n i)
  (set! n i)
  (set! stop (make-vector (+ n 1) #f))
  (set! val-max (make-vector (+ n 1) 0)))

(define (change-val-max-from i)
  (define (helper j ln)
      (cond
        [(= j ln) (void)]
        [(even? j) (begin (vector-set! val-max j -300) (helper (+ j 1) ln))]
        [(odd? j) (begin (vector-set! val-max j 300) (helper (+ j 1) ln))]
        )
      )
  (helper i (vector-length val-max)))

(define (change-stop-from i)
  (define (helper j ln)
    (cond
        [(= j ln) (void)]
        [else (begin (vector-set! stop j #f)(helper (+ j 1) ln))]
        
      ))
  (helper i (vector-length stop)))

(define (rough-tree i)
  (λ (x)
    (cond [(and (odd? i) (vector-ref stop (- i 1))) (make-leaf (+ 1 (vector-ref val-max (- i 1))))]
          [(and (not (= i 0)) (even? i) (vector-ref stop (- i 1))) (make-leaf (- (vector-ref val-max (- i 1)) 1))]
          [else
        (let* ([piece0 (car x)]
               [posn0 (get-field position piece0)]
               [posn1 (caddr x)]
               [killed (cadddr x)])
          (begin
            (define tr 0)
            (set! up1 #t)
            (send piece0 move1 posn1)
            (set! up1 #f)
            (define t
              (cond
                [(= i n) (make-leaf (evaluation))]
                [(even? i) (begin (set! tr (make-tree 0 (map (rough-tree (+ i 1)) 
                                                             (make-list (if (odd? i) comp-clr user-clr)))))
                                  (min1 tr)
                                  tr)]
                [(odd? i) (begin (set! tr (make-tree 0 (map (rough-tree (+ i 1))
                                                            (make-list (if (odd? i) comp-clr user-clr)))))
                                 (max1 tr)
                                 tr)])))
          (undo-last-move)
          (cond 
            [(and (odd? i) (>= (vector-ref val-max (- i 1)) (if (tree? t) (tree-val t) (leaf-val t)))) (vector-set! stop (- i 1) #t)]
            [(and (not (= i 0)) (even? i) (<= (vector-ref val-max (- i 1)) (if (tree? t) (tree-val t) (leaf-val t)))) (vector-set! stop (- i 1) #t)]
            [else (void)]
             )
          (cond [(and (not (= i n)) (even? i)) (begin (vector-set! stop i #f) (vector-set! val-max i (max (vector-ref val-max i) (tree-val t))))]
                [(and (not (= i n)) (odd? i)) (begin (vector-set! stop i #f) (vector-set! val-max i (min (vector-ref val-max i) (tree-val t))))]
                [else (void)])
          (change-stop-from i)
          (change-val-max-from (+ i 1))
          t)])))

(define r-tree 0)
(define (find val list0)
  (define (helper l i)
    (if (null? l) #f
        (if (= (car l) val) i (helper (cdr l) (+ i 1)))))
  (helper list0 0))

(define (best-move)
  (begin
    (change-stop-from 0)
    (change-val-max-from 0)
    (set! r-tree (cons (make-list comp-clr) (make-tree 0 (map (rough-tree 0) (make-list comp-clr)))))
    (max1 (cdr r-tree))
    (define max0 (tree-val (cdr r-tree)))
    (list-ref (car r-tree) (find max0 (map (λ (x) (if (tree? x) (tree-val x) (leaf-val x))) (tree-subtrees-list (cdr r-tree)))))
    ))

(define (max-tree r-tree)
  (set-tree-val! r-tree (foldr (lambda (x y) (max (if (tree? x) (begin
                                                                  (map min-tree (tree-subtrees-list r-tree))
                                                                  (tree-val x))
                                                      (leaf-val x)) y)) -300 (tree-subtrees-list r-tree))))
(define (min-tree r-tree)
  (set-tree-val! r-tree (foldr (lambda (x y) (min (if (tree? x) (begin
                                                                  (map min-tree (tree-subtrees-list r-tree))
                                                                  (tree-val x))
                                                      (leaf-val x)) y)) 300 (tree-subtrees-list r-tree))))

(define (max1 rtree)
  (set-tree-val! rtree (foldr (lambda (x y) (max (if (tree? x) (begin
                                                                 (tree-val x))
                                                     (leaf-val x)) y)) -300 (tree-subtrees-list rtree))))
(define (min1 rtree)
  (set-tree-val! rtree (foldr (lambda (x y) (min (if (tree? x) (begin
                                                                 (tree-val x))
                                                     (leaf-val x)) y)) 300 (tree-subtrees-list rtree))))





















