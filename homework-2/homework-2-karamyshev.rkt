#lang racket

(require rackunit
         racket/generator
         racket/stream)

;; Домашнее задание 2. Темы 3--4
;; Максимум: 112 баллов.
;; Фамилия и имя:
;; Использование ИИ: см. условие задачи 2.9.

;; 2.1. Сводка расписания

(define (agenda-summary agenda)
  (for/fold ([total 0] [longest #f]) ([lesson agenda])
    (define start (second lesson))
    (define end (third lesson))
    (define duration (- end start))
    (values 
      (+ total duration)
      (cond
        [
          (or 
            (not longest)
            (> duration (- (third longest) (second longest)))
          ) 
          lesson
        ]
        [else longest]
      )
    )
  )
)

(define (conflicts left right)
  (for*/list (
      [left-lesson left] 
      [right-lesson right]
      #:when 
        (and 
          (< (second left-lesson) (third right-lesson))
          (< (second right-lesson) (third left-lesson))
        )
      )
    (list (first left-lesson) (first right-lesson))
  )
)

(define (overlap-minutes left right)
  (for*/fold ([total 0]) ([left-lesson left] [right-lesson right])
    (define start (max (second left-lesson) (second right-lesson)))
    (define end (min (third left-lesson) (third right-lesson)))

    (cond
      [(< start end) (+ total (- end start))]
      [else total]
    )
  )
)

;; 2.2. Функции высшего порядка над ленивыми потоками

(define (stream-map2 f left right)
  (cond
    [(or (stream-empty? left) (stream-empty? right)) empty-stream]
    [else
      (stream-cons
        (f (stream-first left) (stream-first right))
        (stream-map2 f (stream-rest left) (stream-rest right))
      )
    ]
  )
)

(define (stream-scan step initial input)
  (cond
    [(empty? input) (stream initial)]
    [else 
      (stream-cons 
        initial 
        (stream-scan
          step
          (step initial (first input)) 
          (rest input)
        )
      )
    ]
  )
)

(define (positions-where predicate input)
  (stream-filter
    (compose predicate cdr)
    (stream-map2 cons (in-naturals 0) input)
  )
)

(define (record-highs input)
  (cond
    [(stream-empty? input) empty-stream] 
    [else (stream-scan max (stream-first input) (stream-rest input))]
  )
)


;(define (stream-flat-map f s)
;  (define (concat s1 s2)
;    (cond
;      [(stream-empty? s1) s2]
;      [else
;        (stream-cons
;          (stream-first s1)
;          (concat (stream-rest s1) s2)
;        )
;      ]
;    )
;  )
;
;  (cond
;    [(stream-empty? s) empty-stream]
;    [else
;      (concat
;        (f (stream-first s))
;        (stream-flat-map f (stream-rest s))
;      )
;    ]
;  )
;)

(define (stream-flat-map f s)
  (for*/stream ([sv s] [fv (f sv)]) fv)
)

;; Почему бесконечный первый внутренний поток не даёт перейти ко второму:
;; TODO

;; 2.3. Генератор и последовательность

(define (make-signal phases)
  (generator ()
    (define (go acc)
      (cond
        [(empty? acc) (go phases)]
        [(zero? (cdr (first acc))) (go (rest acc))]
        [else 
          (define curr (first acc))
          (yield (car curr)) 
          (go (cons (cons (car curr) (- (cdr curr) 1)) (rest acc)))
        ]
      )
    )
    (go phases)
  )
)

(define (signal-sequence phases cycles)
  (in-generator
     (define signal (make-signal phases))
    (for 
      ([i 
        (in-range 
          (* 
            cycles
            (apply + (map cdr phases))
          )
        )
      ])
      (yield (signal))
    )
  )
)

;; Что хранит генератор, чем отличаются псевдоним и новый вызов фабрики,
;; почему второй обход signal-sequence пуст:
;; TODO

;; 2.4. Подстановочная модель ленивого потока

(define (from n)
  (stream-cons n (from (+ n 1))))

(define (lazy-map function input)
  (stream-cons (function (stream-first input))
               (lazy-map function (stream-rest input))))

;; Трасса (stream-first (stream-rest squares)):
;; TODO
;; Прогноз для observed и объяснение мемоизации:
;; TODO
;; Трасса adjacent-sums: результат, число вызовов и схема общих узлов:
;; TODO
;; Что изменится для двух независимо построенных потоков:
;; TODO

;; 2.5. Счёт с историей

(define (make-account initial)
  (let ([balance initial] [history '()])
    (define (deposit! value)
      (set! balance (+ balance value))
      (set! history
            (append history (list (list 'deposit value))))
      balance
    )

    (define (withdraw! value)
      (cond
        [(>= balance value)
          (set! balance (- balance value))
          (set! history (append 
            history 
            (list (list 'withdraw value))
          ))
          #t
        ]
        [else #f]
      )
    )

    (define (statement)
      (list balance history)
    )

    (values deposit! withdraw! statement)
  )
)

;; Какие связывания разделяют функции и какие вызовы имеют эффект:
;; Функции разделяют между собой владение переменными balance и history.
;; Эффекты имеют функции пополнения (deposit!) и снятия (withdraw!). 
;; Функция statement только читает, при этом не меняя состояние переменных.

;; 2.6. Динамическая ставка

(define current-rate (make-parameter 1))

(define (make-live-quote price)
  (lambda (value)
    (* price value (current-rate))
  )
)

(define (make-fixed-quote price)
  (define rate (current-rate))
  (lambda (value)
    (* price value rate)
  )
)

(define (with-rate rate action)
  (parameterize ([current-rate rate])
    (action)
  )
)

(define (freeze-rate action)
  (define rate (current-rate))
  (lambda ()
    (parameterize ([current-rate rate])
      (action)
    )
  )
)

;; Когда каждая функция читает current-rate:
;; make-live-quote: в момент вызова лямбды. Технически вообще не знает о нём.
;; make-fixed-quote: в момент её вызова запоминает значение параметра. Лямбда использщует сохранённое значение.
;; with-rate: не использует значение по-умолчанию. Параметризирует.
;; freeze-rate: запоминает значение при её вызове (как make-fixed-quote). Параметризирует current-rate сохранённым значением.

;; 2.7. Python и JavaScript
;; Python: результат, объяснение ленивости и однократности map, исправление readers:
;; TODO
;; JavaScript: результаты с var и let, объяснение const, исправление readers:
;; TODO

;; 2.8. Правила событий

(define (login-block observation)
  'TODO)

(define (payment-review observation)
  'TODO)

(define (timeout-retry observation)
  'TODO)

(define (audit-event observation)
  'TODO)

(define event-rules
  (list login-block payment-review timeout-retry audit-event))

(define (apply-event-rules rules observations)
  'TODO)

;; 2.9. Вставьте первую версию make-readers из ответа ИИ или свою первую
;; версию. Не исправляйте её до проверок: ниже покажите контрпример, затем
;; поместите исправленную версию под именем make-readers-fixed.

(define (make-readers input)
  (values 'TODO 'TODO))

(define (make-readers-fixed input)
  (values 'TODO 'TODO))

;; Ошибка первой версии, причина и исправление:
;; TODO

(module+ test
  (define left '((talk 0 30) (lab 40 70)))
  (define right '((call 20 40) (break 30 40) (demo 60 80)))

  (check-equal?
   (call-with-values (lambda () (agenda-summary left)) list)
   '(60 (talk 0 30)))
  (check-equal? (conflicts left right) '((talk call) (lab demo)))
  (check-equal? (overlap-minutes left right) 20)

  (check-equal? (stream->list (stream-map2 + '(1 2 3) '(10 20)))
                '(11 22))
  (check-equal? (stream->list (stream-scan + 0 '(3 1 4)))
                '(0 3 4 8))
  (check-equal? (stream->list (positions-where positive? '(-2 8 0 5)))
                '((1 . 8) (3 . 5)))
  (check-equal? (stream->list (record-highs '(3 1 5 5 2 7)))
                '(3 3 5 5 5 7))
  (check-equal?
   (stream->list
    (stream-take
     (stream-flat-map (lambda (number) (stream number (- number)))
                      (in-naturals 1))
     6))
   '(1 -1 2 -2 3 -3))
  (define signal (make-signal '((red . 2) (green . 1))))
  (check-equal? (for/list ([_ (in-range 7)]) (signal))
                '(red red green red red green red))
  (check-equal?
   (for/list ([colour (signal-sequence '((red . 2) (green . 1)) 2)])
     colour)
   '(red red green red red green))

  (define-values (deposit! withdraw! statement) (make-account 10))
  (check-equal? (deposit! 5) 15)
  (check-true (withdraw! 8))
  (check-false (withdraw! 10))
  (check-equal? (statement)
                '(7 ((deposit 5) (withdraw 8))))

  ;; --------

  (define-values (deposit-a! withdraw-a! statement-a)  (make-account 10))

  (define-values (deposit-b! withdraw-b! statement-b) (make-account 20))

  (check-equal? (deposit-a! 5) 15)
  (check-equal? (statement-a) '(15 ((deposit 5))))

  (check-equal? (statement-b) '(20 ()))


  (define-values (deposit-c! withdraw-c! statement-c) (make-account 10))

  (check-true (withdraw-c! 10))
  (check-equal? (statement-c) '(0 ((withdraw 10))))


  (check-false (withdraw-c! 1))
  (check-equal? (statement-c) '(0 ((withdraw 10))))

  ;; ----------------

  (define live (with-rate 2 (lambda () (make-live-quote 10))))
  (define fixed (with-rate 2 (lambda () (make-fixed-quote 10))))
  (check-equal? (list (live 3) (fixed 3)) '(30 60))
  (define frozen
    (with-rate 2
      (lambda ()
        (freeze-rate (lambda () (live 3))))))
  (check-equal? (with-rate 7 frozen) 60)

  (check-equal? (login-block '(login . 0.80)) '(block login 0.8))
  (check-equal? (payment-review '(payment . 0.60)) '(review payment 0.6))
  (check-equal? (audit-event '(login . 0.95)) '(audit login 0.95))
  (check-false (login-block '(login . 0.79)))

  (check-equal?
   (stream->list
    (apply-event-rules
     event-rules
     '((login . 0.95) (payment . 0.50)
                       (payment . 0.80) (timeout . 0.99))))
   '((block login 0.95)
     (audit login 0.95)
     (review payment 0.8)
     (retry timeout 0.99)
     (audit timeout 0.99)))

  ;; Добавьте проверки всех граничных случаев из условия, независимости
  ;; объектов и контрпример для первой версии make-readers.
)
