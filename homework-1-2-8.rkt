#lang racket                                                                                      
;; ============================================================================
;; Задача 1.8. Проверка кода от ИИ
;; ============================================================================
;; Я не использовал(а) ИИ при решении этой задачи.
;; Я использовал(а) ИИ (<модель>) в пункте (а) этой задачи в соответствии с правилами курса и условием.
;;
;; (а) Функция, написанная ИИ (скопируйте её сюда без изменений; полный диалог
;;     --- в transcript.md). Назовите её feedback-ai, чтобы не перекрывать
;;     вашу feedback из задачи 1.3.
(require rackunit)

(define (feedback-muse-spark guess answer)
  (define vec-g (list->vector guess))
  (define vec-a (list->vector answer))
  (define res (make-vector 5 'gray))
  (define freq (make-hash))
  (for ([i (in-range 5)])
    (if (eq? (vector-ref vec-g i) (vector-ref vec-a i))
        (vector-set! res i 'green)
        (hash-update! freq (vector-ref vec-a i) add1 0)))
  (for ([i (in-range 5)])
    (when (not (eq? (vector-ref res i) 'green))
      (define g (vector-ref vec-g i))
      (define c (hash-ref freq g 0))
      (when (> c 0)
        (vector-set! res i 'yellow)
        (hash-set! freq g (- c 1)))))
  (vector->list res))

;(define (feedback-gemma3-4b guess answer)
;  (let loop ([i 0]
;             [feedback 'gray']
;             [guess-list guess]
;             [answer-list answer])
;    (if (= i 5)
;        feedback
;        (let ([guess-char (car guess-list)])
;          (let ([answer-char (car answer-list)])
;            (cond
;              [(= guess-char answer-char)
;               (loop (+ i 1) 'green' (cdr guess-list) (cdr answer-list))]
;              [(member guess-char (list answer-char))
;               (loop (+ i 1) 'yellow' (cdr guess-list) (cdr answer-list))]
;              [else
;               (loop (+ i 1) 'gray' (cdr guess-list) (cdr answer-list))]))))))

(define (feedback-qwen-coder guess answer)
  (define answer-counts (make-hash))
  (for ([char (in-list answer)])
    (hash-set! answer-counts char (add1 (hash-ref answer-counts char 0))))
  
  (define result (make-vector 5))
  (define (set-result idx color)
    (vector-set! result idx color))
  
  (define (try-green guess-index)
    (let ([guess-char (list-ref guess guess-index)]
          [answer-char (list-ref answer guess-index)])
      (when (char=? guess-char answer-char)
        (set-result guess-index 'green)
        (hash-update! answer-counts guess-char sub1))))
  
  (define (try-yellow guess-index)
    (let ([guess-char (list-ref guess guess-index)])
      (when (hash-has-key? answer-counts guess-char)
        (if (> (hash-ref answer-counts guess-char) 0)
            (begin
              (set-result guess-index 'yellow)
              (hash-update! answer-counts guess-char sub1))
            #f))))
  
  (for ([i (in-range 5)])
    (if (try-green i)
        #f
        (try-yellow i)))
  
  (vector->list result))

(define (feedback-qwen-coder-fixed guess answer)
  (define answer-counts (make-hash))
  (for ([char (in-list answer)])
    (hash-set! answer-counts char (add1 (hash-ref answer-counts char 0))))
  (define result (make-vector 5 'gray))
  (define (set-result idx color)
    (vector-set! result idx color))
  
  (define (try-green guess-index)
    (let ([guess-char (list-ref guess guess-index)]
          [answer-char (list-ref answer guess-index)])
      (when (char=? guess-char answer-char)
        (set-result guess-index 'green)
        (hash-update! answer-counts guess-char sub1))
      (unless (char=? guess-char answer-char)
        #f
      )
      ))
  
  (define (try-yellow guess-index)
    (let ([guess-char (list-ref guess guess-index)])
      (when (hash-has-key? answer-counts guess-char)
        (if (> (hash-ref answer-counts guess-char) 0)
            (begin
              (set-result guess-index 'yellow)
              (hash-update! answer-counts guess-char sub1))
            #f))))
  
  (for ([i (in-range 5)])
    (if (try-green i)
        #f
        (try-yellow i)))
  
  (vector->list result))


;; (б) Четыре проверки, из них хотя бы две с повторяющимися буквами.
;;     Для каждой напишите в комментарии ожидаемый ответ, а проверку
;;     оформите как check-equal? с ожидаемым значением (не с вашей feedback:
;;     сверяться нужно с правилом из условия, а не с другой программой).
;;
;; Проверка 1: попытка ..., ответ ..., ожидаю ...
;; Проверка 2: ...
;; Проверка 3: ...
;; Проверка 4: ...

(define (check-feedback func)
  (check-equal? (func (string->list "mouse") (string->list "mouse")) '(green green green green green)) ; полное совпадение
  (check-equal? (func (string->list "mumma") (string->list "mmauo")) '(green yellow yellow gray yellow)) ; Повторяющиеся буквы
  (check-equal? (func (string->list "mouse") (string->list "emous")) '(yellow yellow yellow yellow yellow)) ; Все жёлтые
  (check-equal? (func (string->list "mouse") (string->list "click")) '(gray gray gray gray gray)) ; Все серые
)

(check-feedback feedback-muse-spark)

;; (в) Наименьший контрпример и исправление, либо объяснение, почему
;;     правило повторов соблюдено (с указанием строк).
;;
;; ...
;; контрпример
(check-equal? (feedback-qwen-coder (string->list "mouse") (string->list "click")) '(gray gray gray gray gray))

(check-feedback feedback-qwen-coder-fixed)
