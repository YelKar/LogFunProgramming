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
  (check-equal? (func '(m o u s e) '(m o u s e)) '(green green green green green)) ; полное совпадение
  (check-equal? (func '(m u m m a) '(m m a u o)) '(green yellow yellow gray yellow)) ; Повторяющиеся буквы
  (check-equal? (func '(m o u s e) '(e m o u s)) '(yellow yellow yellow yellow yellow)) ; Все жёлтые
  (check-equal? (func '(m o u s e) '(c l i c k)) '(gray gray gray gray gray)) ; Все серые
)

(check-feedback feedback-muse-spark)
;; (в) Наименьший контрпример и исправление, либо объяснение, почему
;;     правило повторов соблюдено (с указанием строк).
;;
;; ...
