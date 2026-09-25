#lang racket
;; Лабораторная работа 1. Темы 1--2. Заготовка.
;; Запустите файл: rackunit покажет, какие проверки ещё не проходят.
;; Замените 'todo на свои определения. Задачи необязательные.

(require rackunit)

(define (word s) (string->list s))
(define vowels (word "аеёиоуыэюя"))
(define (next n) (+ n 1))

(define (contains? x lst)
  (and (member x lst) #t))

;; Задача 1. Без гласных
; (define (remove-vowels w) 
;     (define (go lst n) 
;         (cond
;             [(and (cons? lst) (list-))] ()
;         ) 
;     )
; )
(define (remove-vowels w) 
    (filter (lambda (x) (not (contains? x vowels))) w)
)

;; Задача 2. Палиндром
(define (palindrome? w) 
(equal? w (reverse w))
)

(define (my-reverse w) 
    (define (go lst acc)
        (cond
            [(cons? lst) (go (rest lst) (cons (first lst) acc))]
            [else acc]
        ) 
    )
    (go w '())
)

(define (palindrome2? w) 
    (equal? w (my-reverse w))
)   ; без reverse, с аккумулятором

;; Задача 3. Позиции буквы
(define (positions ch w) 
    (define (go lst i) 
        (cond
            [(empty? lst) '()]
            [(equal? ch (first lst)) (cons i (go (rest lst) (+ i 1)))]
            [else (go (rest lst) (+ i 1))] 
        )
    )
    (go w 0)
)

;; Задача 4. Два списка в один
(define (zip a b) 
    (map cons a b)
)
(define (zip2 a b) 
    (cond 
        [(and (cons? a) (cons? b)) (cons (cons (first a) (first b)) (zip2 (rest a) (rest b)))]
        [else '()]
    )
)

;; Задача 5. Самое длинное слово
(define (longest words) 
    (foldl (lambda (x y) 
       (if (> (length x) (length y)) x y)
    ) '() words)
)


;; Задача 6. Средний рейтинг
(define (average-rating club) 
    (/ (foldl + 0 (map cdr club)) (length club))
)

;; Задача 7. Зелёные буквы через конвейер
(define (green-count guess answer) 
    (length (filter (lambda (x) (equal? (car x) (cdr x))) (zip guess answer)))
)

;; Задача 8. Применить n раз
(define (apply-n f n x) 
    (cond
        [(> n 0) (apply-n f (- n 1) (f x))]
        [else x] 
    )
)

;; Задача 9. Префикс по условию
(define (take-while p lst) 'todo)
(define (drop-while p lst) 'todo)

;; Задача 10. Разбиение на куски
(define (chunks lst n) 'todo)

;; Задача 11. Композиция списка функций
(define (compose-all fs) 'todo)

;; Задача 12. Все подмножества
(define (subsets lst) 'todo)

;; --- Проверки -----------------------------------------------------------
(check-equal? (remove-vowels (word "карта")) (word "крт"))
(check-equal? (remove-vowels (word "око")) (word "к"))
(check-equal? (palindrome? (word "топот")) #t)
(check-equal? (palindrome? (word "топор")) #f)
(check-equal? (palindrome2? (word "топот")) #t)
(check-equal? (palindrome2? (word "топор")) #f)
(check-equal? (positions #\а (word "карта")) '(1 4))
(check-equal? (positions #\я (word "карта")) '())
(check-equal? (zip '(1 2 3) '(а б в)) '((1 . а) (2 . б) (3 . в)))
(check-equal? (zip2 '(1 2 3) '(а б в)) '((1 . а) (2 . б) (3 . в)))
(check-equal? (longest (list (word "кот") (word "плотина") (word "карта"))) (word "плотина"))
(check-equal? (longest (list (word "кот") (word "дом"))) (word "кот"))
(check-equal? (average-rating '(("Аня" . 1500) ("Вера" . 1600))) 1550)
(check-equal? (green-count (word "карта") (word "парта")) 4)
(check-equal? (green-count (word "торта") (word "карта")) 3)
; (check-equal? (apply-n next 3 5) 8)
; (check-equal? (apply-n (lambda (s) (* 2 s)) 10 1) 1024)
; (check-equal? (take-while even? '(2 4 5 6)) '(2 4))
; (check-equal? (drop-while even? '(2 4 5 6)) '(5 6))
; (check-equal? (take-while even? '()) '())
; (check-equal? (drop-while even? '(2 4)) '())
; (check-equal? (chunks '(1 2 3 4 5) 2) '((1 2) (3 4) (5)))
; (check-equal? (chunks '(1 2 3 4) 2) '((1 2) (3 4)))
; (check-equal? (chunks '() 3) '())
; (check-equal? ((compose-all (list add1 (lambda (x) (* 2 x)))) 5) 11)
; (check-equal? ((compose-all '()) 7) 7)
; (check-equal? (length (subsets '(1 2 3 4))) 16)
; (check-equal? (sort (map length (subsets '(1 2))) <) '(0 1 1 2))
; (check-equal? (member '(1 2) (subsets '(1 2))) '((1 2)))
