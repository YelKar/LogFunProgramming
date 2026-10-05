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
;; (stream->list
;;   (stream-take (stream-flat-map in-naturals '(10 20)) 4))
;; for*/x сопоставляет каждый элемент первой последовательности с каждым элементом второй.
;; Соответственно, алгоритм работает так: берётся первый элемент первой последовательности и создаются пары с каждым элементом второй.
;; Поскольку in-naturals возвращает бесконечный поток, мы никогда не закончим его перебирать, как следствие не дойдём до второго элемента списка '(10 20).

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
;; 1.1. Псевдоним ссылается на тот же генератор. Соответственно один влияет на состояние другого.
;; 1.2. Вызов фабрики создаёт назависимый генератор. Его изменение не влияет на другой генератор этой же фабрики.
;; 2. Внутри signal-sequence перебирается конечный цикл (cycles раз). Когда он заканчаивается, генератор больше ничего не возвращает.

;; 2.4. Подстановочная модель ленивого потока

(define (from n)
  (stream-cons n (from (+ n 1))))

(define (lazy-map function input)
  (stream-cons (function (stream-first input))
              (lazy-map function (stream-rest input))))

(define squares (lazy-map (lambda (x) (* x x)) (from 1)))

;; Трасса (stream-first (stream-rest squares)):
;; (stream-first (stream-rest squares))
;; = (stream-first (stream-rest (lazy-map (lambda (x) (* x x)) (from 1)))) ; пусть lambda (x) (* x x) => square
;; = (stream-first (stream-rest (stream-cons (square (stream-first (from 1))) (lazy-map square (stream-rest (from 1)))))) ; (from 1) = (stream-cons 1 (from 2))
;; = (stream-first (stream-rest (stream-cons (square 1) (lazy-map square (from 2)))))
;; = (stream-first (lazy-map square (from 2))); (from 2) = (stream-cons 2 (from 3))
;; = (stream-first (stream-cons (square 2) (lazy-map square (from 3))))
;; = (square 2)
;; = 4
;; )))
;;
;; Прогноз для observed и объяснение мемоизации:
;; Объявление потока не вычисляет значения параметров сразу. Расчёт переданных параметров происходит только по запросу.
;; (define (foo) 1)
;; ()
;; Трасса adjacent-sums: результат, число вызовов и схема общих узлов:
;; TODO
;; Что изменится для двух независимо построенных потоков:
;; TODO

;; 2.5. Счёт с историей

(define (make-account initial)
  (let ([balance initial] [history '()])
    (define (deposit! value)
      (set! balance (+ balance value))
      (set! history (append 
        history 
        (list (list 'deposit value))
      ))
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
;; После перебора range(3) i принимает значение i = 2. 
;; Тело генератора держит ссылку на i, а не его значение в момент вызова.
;; Следовательно лямбда захватвает не значение от 0 до 2, а ссылку на i.
;; Поэтому в момент вызова лямбды, используется значение переменной i в момент вызова. То есть 2.
;; Соответственно в результате выполнения map, все элементы генератора (после их вычисления) становятся равными marks[2] * 2. 
;; И так как массив marks обновляется до вычисления генератора, мы получаем marks[2] * 2 = 10 * 2 = 20.
;; И поскольку это генератор, его значения мы можем получить только один раз.
;; Ответ: ([20, 20, 20], [])
;; Как исправить:
;; Чтобы этот код работал как, вероятно, задумывалось, необходимо сделать две вещи:
;; 1. Явно захватить i в лямбду.
;; 2. Провести явное копирование списка marks. Простое `lambda marks=marks: ...` не подойдёт, 
;;    так как изменение массива происходит не пересозданием переменной marks, а переопределением элементов списка. 
;; То есть: lambda i=i, marks=marks: marks[i] => ([8, 14, 20], [])
;; А: lambda i=i, marks=[*marks]: marks[i] => ([3, 12, 18], [])
;; 
;; А если необходимо получить результат несколько раз, необходимо его явно перевести в список и сохранить в переменную:
;; scaled = list(map(...))
;; 
;; 
;; JavaScript: результаты с var и let, объяснение const, исправление readers:
;; var является устревшим способом объявления переменной, который не в полной мере соблюдает области видимости.
;; Тут ситуация такая же как и с python. В момент расчёта мы ссылаемся на последнее значение i.
;; Однако здесь i равно 3, а не 2. Следовательно ссылкается не на 3-й, а не 4-й элемент списка.
;; Поэтому вывод: [undefined, undefined, undefined].
;; Для конструкции let в заголовке цикла JavaScript создаёт новое связывание на каждой итерации.
;; Следовательно в случае с let лямбды ссылаются на копии переменной i.
;; Теперь вывод: [30, 6, 9] так как исходное значение marks было изменено.
;; Поэтому необходимо в теле цикла выполнить копирование ЭЛЕМЕНТОВ списка marks:
;;
;; for (let i = 0; ...) {
;;    // const marks_ = marks; не подойдёт, так как здесь происходит компирование указателя на список, а не самого списка.
;;    const marks_ = [...marks];
;; }
;; ...
;;
;; Вывод: [3, 6, 9]
;;
;; 2.8. Правила событий

(define (login-block observation)
  (define event (car observation))
  (define p (cdr observation))
  (and 
    (equal? event 'login)
    (>= p 0.8)
    (list 'block 'login p)
  )
)

(define (payment-review observation)
  (define event (car observation))
  (define p (cdr observation))
  (and
    (equal? event 'payment)
    (>= p 0.6)
    (list 'review 'payment p)
  )
)

(define (timeout-retry observation)
  (define event (car observation))
  (define p (cdr observation))
  (and
    (equal? event 'timeout)
    (>= p 0.7)
    (list 'retry 'timeout p)
  )
)

(define (audit-event observation)
  (define event (car observation))
  (define p (cdr observation))
  (and
    (>= p 0.95)
    (list 'audit event p)
  )
)

(define event-rules
  (list login-block payment-review timeout-retry audit-event))

(define (apply-event-rules rules observations)
  (stream-flat-map
    (lambda (observation)
      (stream-filter
        (compose not false?)
        (stream-map (lambda (rule) (rule observation)) rules)
      )
    )
    observations
  )
)

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
  
  (check-equal? (conflicts '((1 0 20) (2 20 30) (3 50 70) (4 35 45))) '())
  
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
