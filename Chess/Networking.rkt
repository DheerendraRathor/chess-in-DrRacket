(require 2htdp/universe)

(define adding #t)
(define timern 0)
(define server-time 0)
(define (add-world univ wrld)
  (begin0 (local ((define univ* (append univ (list wrld))))
            (make-bundle univ*
                         (if (not adding)
                             (list (make-mail (second univ*) (list "black" timern))
                                   (make-mail (first univ*) 'move))
                             (list (make-mail (first univ*) (list "white" timern))))
                         
                         '())) (set! adding #f)))

(define (disco uni w)
  (let ((uni* (if (equal? w (car uni)) (cdr uni) uni)))
    (make-bundle uni*
                 (list (make-mail (first uni*) 'mate)) '())))

(define (switch univ wrld m)
  (local ((define univ* (append (rest univ) (list (first univ)))))
     (make-bundle univ*
                 (list (make-mail (first univ*) m))
                 '())))


(define (start-server)
  (begin (display "timer:") (set! server-time (read)) (if (number? server-time) (set! timern (* 60 server-time))
             (set! timern "standard"))  (set! adding #t) (universe '() 
                                                                   (on-new add-world) 
                                                                   (on-msg switch)
                                                                   (state #f)
                                                                   (on-disconnect disco)
                                                                   )))
;(display "manually start server by typing (start-server) in console")

(start-server)