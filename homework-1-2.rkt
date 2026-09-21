#lang racket
;; Домашнее задание 1. Темы 1--2.
;; Функциональное и логическое программирование, осень 2026, Институт iSpring.
;;
;; Фамилия Имя, группа ПС-31
;;
;; Как пользоваться этим файлом.
;; 1. Переименуйте файл в homework-1-<Фамилия>.rkt и впишите себя выше.
;; 2. Код пишите как код, а рассуждения, выкладки по подстановочной модели
;;    и доказательства --- в комментариях под соответствующей задачей.
;; 3. У каждой задачи оставьте одну строку декларации об использовании ИИ
;;    и удалите вторую. Задача без декларации считается несделанной.
;; 4. Запускайте файл (racket homework-1-<Фамилия>.rkt или кнопка Run
;;    в DrRacket): примеры из условия оформлены как проверки, и каждая
;;    несовпадающая печатает ожидаемое и фактическое значение.
;;    Если проверки молчат, значит все примеры сходятся.
;; 5. Заглушки 'todo замените своим кодом. Все функции должны быть
;;    определены, даже если задача не решена: файл должен запускаться.
;; 6. К задаче 1.8 приложите transcript.md отдельным файлом.

(require rackunit)

;; ============================================================================
;; Общие определения (даны в условии, не меняйте)
;; ============================================================================

;; Слово --- список символов.
(define (word->list w) (string->list w))

;; Длина списка явной рекурсией (для задачи 1.4).
(define (my-length lst)
  (cond
    [(empty? lst) 0]
    [else (+ 1 (my-length (rest lst)))]))

;; Небольшой словарь пятибуквенных слов.
(define words
  (map word->list
       '("топор" "ротор" "мотор" "робот" "табор"
         "баран" "сарай" "сахар" "халат" "канат"
         "карат" "парад" "народ" "салат" "касса"
         "масса" "крыса" "берег" "бегун" "бетон"
         "белок" "билет" "буква" "волна" "ворон"
         "ворот" "город" "горох" "гость" "дверь")))

;; Коммит --- список (автор добавлено удалено); сборка --- список (ветка исход секунды).
(define (commit-author c) (first c))
(define (commit-added c) (second c))
(define (commit-deleted c) (third c))
(define (build-branch b) (first b))
(define (build-status b) (second b))
(define (build-seconds b) (third b))

;; ============================================================================
;; Задача 1.1. Списки явной рекурсией
;; ============================================================================
;; Я не использовал(а) ИИ при решении этой задачи.
;; Я использовал(а) ИИ (<модель>) в <части> этой задачи в соответствии с правилами курса и условием.

(define (index-of x lst)
    (cond 
       [(empty? lst) #f]
       [(equal? (first lst) x) 0]
       [(index-of x (rest lst)) => (lambda (index) (+ index 1))]
       [else #f]
    )
)

(define (dedupe-adjacent lst)
  (define (snd lst)
    (first (rest lst))
  )
  (cond 
    [(or (empty? lst) (empty? (rest lst))) lst]
    [(equal? (first lst) (snd lst)) (dedupe-adjacent (rest lst))]
    [else (cons (first lst) (dedupe-adjacent (rest lst)))]
  )
)


(define (merge-sorted a b)
  (cond 
    [(empty? b) a]
    [(empty? a) b]
    [(< (first a) (first b)) (cons (first a) (merge-sorted (rest a) b))]
    [else (cons (first b) (merge-sorted (rest b) a))]
  )
)

(define (rotate-left lst k)
  (cond 
    [(equal? k 0) lst]
    [else (rotate-left (append (rest lst) (list (first lst))) (- k 1))]
  )
)

(define (split-on sep lst)
  (define (concat lst split-res) 
    (cons (cons (first lst) (first split-res)) (rest split-res))
  )
  (cond
    [(empty? lst) '(())]
    [(equal? (first lst) sep) (cons '() (split-on sep (rest lst)))]
    [else (concat lst (split-on sep (rest lst)))]
  )
)

(check-equal? (index-of 3 '(1 3 5 3)) 1)
(check-equal? (index-of 4 '(1 3 5)) #f)
(check-equal? (index-of #\о (word->list "топор")) 1)
(check-equal? (dedupe-adjacent '(1 1 2 2 2 1)) '(1 2 1))
(check-equal? (dedupe-adjacent '()) '())
(check-equal? (dedupe-adjacent (word->list "касса")) (word->list "каса"))
(check-equal? (merge-sorted '(1 4 6) '(2 3 7 9)) '(1 2 3 4 6 7 9))
(check-equal? (merge-sorted '() '(5)) '(5))
(check-equal? (merge-sorted '(2 2) '(2)) '(2 2 2))
(check-equal? (rotate-left '(1 2 3 4 5) 2) '(3 4 5 1 2))
(check-equal? (rotate-left '(1 2 3) 0) '(1 2 3))
(check-equal? (rotate-left '(1 2 3) 3) '(1 2 3))
(check-equal? (split-on 0 '(1 2 0 3 0 0 4)) '((1 2) (3) () (4)))
(check-equal? (split-on 0 '(0)) '(() ()))
(check-equal? (split-on 0 '()) '(()))
(check-equal? (split-on 0 '(1 2)) '((1 2)))
(check-equal? (split-on #\newline (string->list "ok\nfail\n")) '((#\o #\k) (#\f #\a #\i #\l) ()))

;; ============================================================================
;; Задача 1.2. Параметр-аккумулятор
;; ============================================================================
;; Я не использовал(а) ИИ при решении этой задачи.
;; Я использовал(а) ИИ (<модель>) в <части> этой задачи в соответствии с правилами курса и условием.

(define (digits n)
  (define (go n acc)
    (cond
      [(< n 10) (cons n acc)]
      [else (go (quotient n 10) (cons (remainder n 10) acc))]
    )
  )
  (go n '())
)

(define (from-digits lst)
  (define (go lst acc)
    (cond 
      [(empty? lst) acc]
      [else (go (rest lst) (+ (* acc 10) (first lst)))]
    )
  )
  (go lst 0)
)

(define (longest-run lst)
  (cond 
    [(empty? lst) 0]
    [else
      (define (max2 a b)
        (cond 
          [(> a b) a]
          [else b]
        )
      )
      (define (go lst c n mx) 
        (cond
          [(empty? lst) (max2 mx n)]
          [(equal? (first lst) c) (go (rest lst) c (+ n 1) mx)]
          [else (go (rest lst) (first lst) 1 (max2 mx n))]
        )
      )
      (go (rest lst) (first lst) 1 0)
    ]
  )
)

;; (г) Вычисление (digits 205) по подстановочной модели:
;;   (digits 205)
;;   = (go 205 '())
;;   = (go (quotient 205 10) cons (remainder n 10) '())
;;   = (go 20 '(5 ()))
;;   = (go (quotient 20 10) cons (remainder 20 10) '(5))
;;   = (go 2 '(0 5))
;;   = (cons 2 '(0 5))
;;   = '(2 0 5)

(check-equal? (digits 2026) '(2 0 2 6))
(check-equal? (digits 0) '(0))
(check-equal? (digits 7) '(7))
(check-equal? (from-digits '(2 0 2 6)) 2026)
(check-equal? (from-digits '(0)) 0)
(check-equal? (from-digits (digits 90210)) 90210)
(check-equal? (longest-run '(1 1 2 2 2 1)) 3)
(check-equal? (longest-run '()) 0)
(check-equal? (longest-run '(4 4 4)) 3)
(check-equal? (longest-run (word->list "масса")) 2)

;; ============================================================================
;; Задача 1.3. Игра в слова
;; ============================================================================
;; Я не использовал(а) ИИ при решении этой задачи.
;; Я использовал(а) ИИ (<модель>) в <части> этой задачи в соответствии с правилами курса и условием.

(define (feedback:wrong guess answer)
  (define (green-pass g a) 
     (cond
      [(or (empty? g) (empty? a)) '()]
      [else (cons 
              (and (equal? (first g) (first a)) 'green)
              (green-pass (rest g) (rest a)))]
    )
  )

  (define (escape-green g r)
    (cond
      [(empty? g) '()]
      [(equal? (first r) 'green) (escape-green (rest g) (rest r))]
      [else (cons (first g) (escape-green (rest g) (rest r)))]
    )
  )

  (define (shift-to lst letter)
    (cond 
      [(empty? lst) '()]
      [(equal? (first lst) letter) lst]
      [else (shift-to (rest lst) letter)]
     )
  )

  (define (yellow-gray-letter-pass c g a r)
    (cond
      [(empty? r) '()]
      [(equal? (first r) 'green) (cons 'green (yellow-gray-letter-pass c (rest g) a (rest r)))]
      [(not (equal? c (first g))) (cons #f (yellow-gray-letter-pass c (rest g) a (rest r)))]
      [else 
        (cons 
          (
            cond [(and (not (empty? a)) (equal? c (first a))) 'yellow] 
            [else 'gray]
          ) 
          (yellow-gray-letter-pass c (rest g) 
            (
             shift-to (cond [(empty? a) '()] [else (rest a)]) c) (rest r)))]
    )
  )

  (define (merge r1 r2)
    (cond
      [(or (empty? r1) (empty? r2)) '()]
      [else (cons (or (first r1) (first r2)) (merge (rest r1) (rest r2)))]
    )
  )

  (define (yellow-gray-pass g a r)
    (cond
      [(empty? g) '()]
      [else (merge (yellow-gray-letter-pass (first g) g (shift-to a (first g)) r) (cons (first r) (yellow-gray-pass (rest g) a (rest r))))]
    )
  )

  (yellow-gray-pass
    guess
    (escape-green answer (green-pass guess answer)) 
    (green-pass guess answer)
    )
)

;;(feedback (word->list "топор") (word->list "ротор"))

;(define (feedback guess answer) 
;  (define green
;    (map
;      (lambda (g a) (and (equal? g a) 'green))
;      guess, answer
;    )
;  )
;  (define unknown-letters
;    (map 
;      second 
;      (filter 
;        (lambda (letter) (not (equal? (first letter) 'green)))
;
;      )
;    )
;  )
;)


(define (consistent? word guess fb)
  'todo)

(define (candidates dict guess fb)
  'todo)

(define (best-guess dict)
  'todo)

(check-equal? (feedback (word->list "топор") (word->list "ротор"))
              '(yellow green gray green green))
(check-equal? (feedback (word->list "робот") (word->list "робот"))
              '(green green green green green))
(check-equal? (feedback (word->list "касса") (word->list "сарай"))
              '(gray green yellow gray yellow))
(check-equal? (feedback (word->list "масса") (word->list "касса"))
              '(gray green green green green))
(check-equal? (feedback (word->list "сахар") (word->list "салат"))
              '(green green gray green gray))
(check-equal? (consistent? (word->list "ротор") (word->list "топор")
                           '(yellow green gray green green))
              #t)
(check-equal? (consistent? (word->list "табор") (word->list "топор")
                           '(yellow green gray green green))
              #f)
(check-equal? (candidates words (word->list "топор") '(yellow green gray green green))
              (map word->list '("ротор" "мотор")))
(check-equal? (candidates words (word->list "робот") '(gray gray gray gray gray))
              (map word->list '("касса" "масса")))
(check-equal? (best-guess (map word->list '("топор" "ротор" "мотор" "робот" "табор")))
              (word->list "ротор"))

;; ============================================================================
;; Задача 1.4. Подстановочная модель
;; ============================================================================
;; Я не использовал(а) ИИ при решении этой задачи.
;; Я использовал(а) ИИ (<модель>) в <части> этой задачи в соответствии с правилами курса и условием.
;;
;; (а) (my-length (merge-sorted a b)) = (+ (my-length a) (my-length b))
;;
;; Доказательство:
;; (my-length a) --> (len a):
;; Если a = '():
;; (len '())
;; = (cond [empty? '()] 0)
;; = 0
;; Иначе a = '(1 2 3 4 ... n):
;; (len a)
;; = (+ 1 (len (rest a)))
;; = (+ 2 (len (rest (rest a))))
;; = ...
;; = (+ n (len '()))
;; = (+ n 0)
;; = n
;;
;; Если a = '() или b = '():
;; (len (merge-sorted '() b))
;; = (length (cond [(empty? '()) b])) 
;; = (len b) 
;; = (+ (len b) (len '()))
;; = (+ (len b) (len a))
;; = (+ (len b) 0)
;;
;; Иначе:
;; (len (merge-sorted a b)) 
;; = (len (cons (first a) (merge-sorted (rest a) b))) 
;; = (+ 1 (len (merge-sorted (rest a) b)))
;; = (+ 2 (len (merge-sorted (rest (rest a)) b)))
;; = (+ (len a) (merge-sorted '() b))
;; = (+ (len a) (len b))
;;
;;
;; (б) (dedupe-adjacent (dedupe-adjacent lst)) = (dedupe-adjacent lst)
;;
;; Доказательство:
;;   dedupe возвращает список соответствующей

;; ============================================================================
;; Задача 1.5. Функции как значения
;; ============================================================================
;; Я не использовал(а) ИИ при решении этой задачи.
;; Я использовал(а) ИИ (<модель>) в <части> этой задачи в соответствии с правилами курса и условием.

(define (negate p)
  'todo)

(define (all-of ps)
  'todo)

(define (compose2 f g)
  'todo)

(define (on cmp key)
  'todo)

(define (argmax-by f lst)
  'todo)

;; (г) Почему (4) осталось перед (7), и что было бы при (on <= length):
;;   ...

(check-equal? ((negate even?) 3) #t)
(check-equal? ((negate even?) 4) #f)
(check-equal? (filter (all-of (list even? positive?)) '(-2 1 4 6)) '(4 6))
(check-equal? (filter (all-of '()) '(1 2)) '(1 2))
(check-equal? ((compose2 add1 (lambda (x) (* 2 x))) 5) 11)
(check-equal? (sort '((1 2 3) (4) (5 6) (7)) (on < length)) '((4) (7) (5 6) (1 2 3)))
(check-equal? ((on string<? symbol->string) 'b 'a) #f)
(check-equal? (argmax-by string-length '("да" "нет" "ага")) "нет")
(check-equal? (argmax-by - '(3 1 2)) 1)

;; ============================================================================
;; Задача 1.6. Свёртки и история коммитов
;; ============================================================================
;; Я не использовал(а) ИИ при решении этой задачи.
;; Я использовал(а) ИИ (<модель>) в <части> этой задачи в соответствии с правилами курса и условием.

(define (count-if p lst)
  'todo)

(define (my-map f lst)
  'todo)

(define (repo-size commits)
  'todo)

(define (peak-size commits)
  'todo)

(define (added-by-author commits)
  'todo)

(define commits
  '(("Аня" 120 10) ("Борис" 40 60) ("Вера" 0 80) ("Аня" 5 5)))

(check-equal? (count-if even? '(1 2 3 4 6)) 3)
(check-equal? (count-if (lambda (w) (member #\о w)) words) 14)
(check-equal? (my-map add1 '(1 2 3)) '(2 3 4))
(check-equal? (my-map string-length '("ab" "" "abc")) '(2 0 3))
(check-equal? (repo-size commits) 10)
(check-equal? (repo-size '()) 0)
(check-equal? (peak-size commits) 110)
(check-equal? (peak-size '()) 0)
(check-equal? (added-by-author commits) '(("Аня" . 125) ("Борис" . 40) ("Вера" . 0)))

;; ============================================================================
;; Задача 1.7. Конвейеры и сборки CI
;; ============================================================================
;; Я не использовал(а) ИИ при решении этой задачи.
;; Я использовал(а) ИИ (<модель>) в <части> этой задачи в соответствии с правилами курса и условием.

(define (passed builds)
  'todo)

(define (branches-of builds)
  'todo)

(define (success-table builds)
  'todo)

(define (stable-branches builds)
  'todo)

(define (slowest builds)
  'todo)

(define builds
  '(("main" ok 210) ("main" fail 190) ("feature-login" ok 320)
    ("main" ok 205) ("feature-login" fail 300) ("hotfix" ok 95)))

(check-equal? (passed builds) 4)
(check-equal? (branches-of builds) '("main" "feature-login" "hotfix"))
(check-equal? (success-table builds) '(("hotfix" . 1) ("main" . 2/3) ("feature-login" . 1/2)))
(check-equal? (stable-branches builds) '("hotfix"))
(check-equal? (slowest builds) '("feature-login" ok 320))

;; ============================================================================
;; Задача 1.8. Проверка кода от ИИ
;; ============================================================================
;; Я не использовал(а) ИИ при решении этой задачи.
;; Я использовал(а) ИИ (<модель>) в пункте (а) этой задачи в соответствии с правилами курса и условием.
;;
;; (а) Функция, написанная ИИ (скопируйте её сюда без изменений; полный диалог
;;     --- в transcript.md). Назовите её feedback-ai, чтобы не перекрывать
;;     вашу feedback из задачи 1.3.

(define (feedback-ai guess answer)
  'todo)

;; (б) Четыре проверки, из них хотя бы две с повторяющимися буквами.
;;     Для каждой напишите в комментарии ожидаемый ответ, а проверку
;;     оформите как check-equal? с ожидаемым значением (не с вашей feedback:
;;     сверяться нужно с правилом из условия, а не с другой программой).
;;
;; Проверка 1: попытка ..., ответ ..., ожидаю ...
;; Проверка 2: ...
;; Проверка 3: ...
;; Проверка 4: ...

;; (в) Наименьший контрпример и исправление, либо объяснение, почему
;;     правило повторов соблюдено (с указанием строк).
;;
;; ...
