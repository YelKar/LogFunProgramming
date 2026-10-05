#lang racket
(require rackunit)
#|
(define (make-readers-ai s)
  (let ([current-position 0])
    (define (reader1)
      (if (< current-position (vector-length s))
          (begin
            (define result (vector-ref s current-position))
            (set! current-position (+ current-position 1))
            result)
          #f))
    
    (define (reader2)
      (if (< current-position (vector-length s))
          (begin
            (define result (vector-ref s current-position))
            (set! current-position (+ current-position 1))
            result)
          #f))
    
    (values reader1 reader2)))

;; Пример использования
(define s (vector 1 2 3 4 5))
(define-values (r1 r2) (make-readers s))

(displayln (r1)) ; 1
(displayln (r1)) ; 2
(displayln (r2)) ; 1
(displayln (r2)) ; 2
(displayln (r1)) ; 3
(displayln (r2)) ; 3
(displayln (r1)) ; 4
(displayln (r2)) ; 4
(displayln (r1)) ; 5
(displayln (r2)) ; 5
(displayln (r1)) ; #f
(displayln (r2)) ; #f
|#
;; Проблема: использование define в выражениии (if)

;; v2
(define (make-readers-ai s)
  (let ([current-position 0])
    (define (reader1)
      (if (< current-position (vector-length s))
          (let ([result (vector-ref s current-position)])
            (set! current-position (+ current-position 1))
            result)
          #f))
    
    (define (reader2)
      (if (< current-position (vector-length s))
          (let ([result (vector-ref s current-position)])
            (set! current-position (+ current-position 1))
            result)
          #f))
    
    (values reader1 reader2)))

;; Пример использования
(define s (vector 1 2 3 4 5))
(define-values (r1 r2) (make-readers-ai s))

; Косяк:
(check-equal? (r1) 1)
(check-equal? (r2) 1)


(define (make-readers-ai-fixed s)
  (let ([current-position-1 0] [current-position-2 0])
    (define (reader1)
      (if (< current-position-1 (vector-length s))
          (let ([result (vector-ref s current-position-1)])
            (set! current-position-1 (+ current-position-1 1))
            result)
          #f))
    
    (define (reader2)
      (if (< current-position-2 (vector-length s))
          (let ([result (vector-ref s current-position-2)])
            (set! current-position-2 (+ current-position-2 1))
            result)
          #f))
    
    (values reader1 reader2)))

(define-values (r1-ai-fixed r2-ai-fixed) (make-readers-ai-fixed s))

(check-equal? (r1-ai-fixed) 1)
(check-equal? (r2-ai-fixed) 1)
(check-equal? (r1-ai-fixed) 2)
(check-equal? (r1-ai-fixed) 3)
(check-equal? (r2-ai-fixed) 2)
(check-equal? (r1-ai-fixed) 4)
(check-equal? (r1-ai-fixed) 5)
(check-equal? (r2-ai-fixed) 3)
(check-equal? (r2-ai-fixed) 4)
(check-equal? (r1-ai-fixed) #f)
(check-equal? (r2-ai-fixed) 5)
(check-equal? (r1-ai-fixed) #f)
(check-equal? (r2-ai-fixed) #f)
(check-equal? (r2-ai-fixed) #f)
(check-equal? (r1-ai-fixed) #f)
(check-equal? (r1-ai-fixed) #f)
(check-equal? (r2-ai-fixed) #f)


(require racket/generator)

(define (make-readers-fixed s)
  (values
    (generator ()
      (for ([x (in-vector s)])
        (yield x)
      )
      #f
    )
   
    (generator ()
      (for ([x (in-vector s)])
        (yield x)
      )
      #f
    )
  )
)

(define-values (r1-fixed r2-fixed) (make-readers-fixed s))
(check-equal? (r1-fixed) 1)
(check-equal? (r2-fixed) 1)
(check-equal? (r1-fixed) 2)
(check-equal? (r1-fixed) 3)
(check-equal? (r2-fixed) 2)
(check-equal? (r1-fixed) 4)
(check-equal? (r1-fixed) 5)
(check-equal? (r2-fixed) 3)
(check-equal? (r2-fixed) 4)
(check-equal? (r1-fixed) #f)
(check-equal? (r2-fixed) 5)
(check-equal? (r1-fixed) #f)
(check-equal? (r2-fixed) #f)
(check-equal? (r2-fixed) #f)
(check-equal? (r1-fixed) #f)
(check-equal? (r1-fixed) #f)
(check-equal? (r2-fixed) #f)

