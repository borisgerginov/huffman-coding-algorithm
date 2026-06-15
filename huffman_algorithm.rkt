#lang racket

(define (frequency-counter-helper p list el ctr)
  (cond
    ((null? list) ctr)
    ((p el (car list))
        (frequency-counter-helper p (cdr list) el (+ ctr 1)))
    (else
        (frequency-counter-helper p (cdr list) el ctr))))

(define (frequency-counter p list el)
  (frequency-counter-helper p list el 0))

(define (member? el p list)
  (cond
    ((null? list) #f)
    ((p el (car list)) #t)
    (else (member? el p (cdr list)))))

(define (duplicates-remove-helper p list acc)
  (cond
    ((null? list) (reverse acc))
    ((member? (car list) p acc)
         (duplicates-remove-helper p (cdr list) acc))
    (else
         (duplicates-remove-helper p (cdr list) (cons (car list) acc)))))

(define (duplicates-remove p list)
  (duplicates-remove-helper p list '()))


(define (frequency-list p list)
  (map (lambda (x) (cons x (frequency-counter p list x))) (duplicates-remove p list)))


(define (leaf weight symbol) (list 'leaf weight symbol))
(define (node weight left right) (list 'node weight left right))


(define (weight t) (cadr t))

(define (leaf? t)
  (and (list? t) (not (null? t)) (eq? (car t) 'leaf)))

(define (huffman-tree? t)
  (cond
    [(null? t) #t]
    [(and (list? t)
          (= (length t) 3)
          (eq? (car t) 'leaf)
          (number? (cadr t)))
     #t]
    [(and (list? t)
          (= (length t) 4)
          (eq? (car t) 'node)
          (number? (cadr t)))
     (let* ([w (weight t)]
            [l (caddr t)]
            [r (cadddr t)]
            [wl (weight l)]
            [wr (weight r)])
       (and (huffman-tree? l)
            (huffman-tree? r)
            (number? wl)
            (number? wr)
            (= w (+ wl wr))))]
    [else #f]))

(define (insert-by-weight tree tree-sort)
  (cond
    ((null? tree-sort) (list tree))
    ((<= (weight tree) (weight (car tree-sort)))
           (cons tree tree-sort))
    (else
           (cons (car tree-sort) (insert-by-weight tree (cdr tree-sort))))))


(define (make-leaves freq-list)
  (foldl (lambda (elem acc) (insert-by-weight (leaf (cdr elem) (car elem)) acc)) '() freq-list))
  

(define (make-huffman-tree-helper sort-forest)
  (cond
    ((null? sort-forest) '())
    ((null? (cdr sort-forest)) (car sort-forest))
    (else
         (let* ((t1 (car sort-forest))
                (t2 (cadr sort-forest))
                (rest (cddr sort-forest))
                (to-add (node (+ (weight t1) (weight t2)) t1 t2)))
            (make-huffman-tree-helper (insert-by-weight to-add rest))))))
  
(define (make-huffman-tree data-list p)
  (make-huffman-tree-helper (make-leaves (frequency-list p data-list))))



;; Използван модел: OpenAI, GPT-5.2 Thinking
;; Запитване №1:
;; > Има ли вградена функция за намиране на четвъртия елемент на списък в Scheme?
;; Оригинален отговор:
;; > Да — в повечето Scheme реализации има.
;; > cadddr връща 4-тия елемент на списък (т.е. (car (cdddr lst))).
;; > Пример:
;; > (cadddr '(a b c d e))  ; => d
;; > Алтернативи (ако искаш да е по-четимо):
;; > (list-ref '(a b c d e) 3)   ; индексът е 0-базиран => d
;; > (car (cdddr '(a b c d e)))  ; => d
;; > Забележка: това работи за правилен списък с поне 4 елемента; иначе ще гръмне с грешка в зависимост от интерпретатора.
;; Направени промени:
;; - Добавена функционалност за намиране на дясното поддърво на възел (node)

;; Използван модел: OpenAI, GPT-5.2 Thinking
;; Запитване №2:
;; > Има ли вградена функция в Scheme, която превръща лист от цифри в низ, "залепяйки" ги?
;; Оригинален отговор:
;; > Да — има вградени тухлички, но обикновено няма една-единствена стандартна функция “list of digits → string” във всички Scheme диалекти. Най-често се прави по един от тези стандартни начини:
;; > 1) Ако цифрите са числа (0–9)
;; > (list->string (map (lambda (d) (integer->char (+ d (char->integer #\0)))) digits))
;; > Пример:
;; > (define digits '(1 0 7 3))
;; > (list->string (map (lambda (d) (integer->char (+ d (char->integer #\0)))) digits))
;; > => "1073"
;; > 2) Ако “цифрите” ти вече са символи/знаци (characters)
;; > Ако имаш (#\1 #\0 #\7 #\3):
;; > (list->string chars)  ; => "1073"
;; > 3) Ако искаш общо “залепяне” (и диалектът ти има SRFI-13)
;; > В някои реализации има string-join, но не е задължително стандартно навсякъде.
;; Направени промени:
;; - Превръщане на 0 и 1 в символи (#\0 и #\1) в момента на добавянето им
;; - Използване на вградената функция list->string, която превръща списъка от символи в общ низ


(define (make-code-table-helper htree code)
  (cond
    ((null? htree) '())
    ((and (equal? code '()) (leaf? htree)) (list (cons (caddr htree) "0")))
    ((leaf? htree) (list (cons (caddr htree) (list->string (reverse code)))))
    (else
         (let ((left (caddr htree))
                (right (cadddr htree)))
           (append (make-code-table-helper left (cons #\0 code))
                 (make-code-table-helper right (cons #\1 code)))))))

(define (make-code-table htree)
  (make-code-table-helper htree '()))


(define (find-element-code el code-table p)
  (cond
    ((null? code-table) '())
    ((p el (car (car code-table))) (cdr(car code-table)))
    (else
     (find-element-code el (cdr code-table) p))))

(define (make-compressed-string htree data p)
  (let
      ((code-table (make-code-table htree)))
    (apply string-append (map (lambda (elem) (find-element-code elem code-table p)) data))))


(define (encode data p)
  (let
      ((htree (make-huffman-tree data p)))
  (cons htree (make-compressed-string htree data p))))


;; Използван модел: OpenAI, GPT-5.2 Thinking
;; Запитване:
;; > Има ли вградена функция за намиране на подниз на низ в Scheme?
;; Оригинален отговор:
;; > Да — в Scheme обикновено е substring.
;; > (substring s start end)   ; взима от start (включително) до end (изключително)
;; > Примери:
;; > (substring "abcdef" 1 4)  ; => "bcd"
;; > (substring "Здрасти" 0 3) ; => "Здр"   ; ако е Unicode низ, работи по индекси на символи в повечето среди
;; > Ако искаш “подниз от позиция до края”:
;; > (substring s start (string-length s))
;; Направени промени:
;; - Добавена функционалност за намиране на първия символ в компресирания низ (compr-string)
;; - Добавена функционалност за взимане на "опашката" на низ (всички символи освен първия)

(define (decode-helper htree temp-subtree compr-string)
  (cond
    ((eq? htree '()) '())
    ((leaf? temp-subtree) (cons (caddr temp-subtree) (decode-helper htree htree compr-string)))
    ((= (string-length compr-string) 0) '())
    ((eq? (string-ref compr-string 0) #\0) (decode-helper htree (caddr temp-subtree) (substring compr-string 1 (string-length compr-string))))
    (else
     (decode-helper htree (cadddr temp-subtree) (substring compr-string 1 (string-length compr-string))))))


(define (decode-single-leaf el string)
  (if(= (string-length string) 0) '()
       (cons el (decode-single-leaf el (substring string 1)))))

(define (decode pair)
  (let ((htree (car pair))
        (compr-string (cdr pair)))
    (if (leaf? htree) (decode-single-leaf (caddr htree) compr-string)
                      (decode-helper htree htree compr-string))))

(define (encode-data-from-file filename p)
  (let* ([port (open-input-file filename)]
         [list (read port)])
    (close-input-port port)
    (cond
      [(eof-object? list) 
       (error "File is empty or contains no readable list")]
      [(not (list? list))
        (error "Expected a list in file")]
      [else
        (encode list p)])))


;; Използван модел: OpenAI, GPT-5.2 Thinking
;; Запитване:
;; > Искам да запиша резултата от кодирането, по което работя на Scheme, във файл.
;; > Как да се предпазя, ако вече съществува файл със същото име?
;; Оригинален отговор:
;; > Най-сигурното е да откажеш запис, ако файлът вече съществува, вместо да презаписваш “тихо”.
;; > Ако си на Racket (най-лесно)
;; > (call-with-output-file "out.txt"
;; >   (lambda (out)
;; >     (display result out))
;; >   #:exists 'error)   ; гръмва, ако out.txt вече съществува
;; > Алтернативи за #:exists:
;; > #:exists 'replace — презаписва
;; > #:exists 'append — добавя в края
;; > #:exists 'truncate — изтрива съдържанието и пише отначало
;; > По-универсална идея (работи в повечето Scheme-и): провери и тогава пиши
;; > (define (write-if-new filename content)
;; >   (if (file-exists? filename)
;; >       (error "File already exists" filename)
;; >       (call-with-output-file filename
;; >         (lambda (out) (display content out)))))
;; > Забележка: file-exists? и точните функции за файлове са стандартни в много диалекти, но имената/опциите може леко да се различават.
;; Направени промени:
;; - Добавена превенция за писане във вече съществуващ файл

(define (encode-data-to-file filename list p)
  (if (file-exists? filename)
      (error "File already exists" filename)
      (let* ([port (open-output-file filename)]
             [result (encode list p)])
    (write result port)
    (newline port)
    (close-output-port port))))


(define (decode-result-from-file filename)
  (let* ([port (open-input-file filename)]
         [result (read port)])
    (close-input-port port)
    (cond
      [(eof-object? result)
          (error "File is empty or contains no readable Huffman tree or compressed string")]
      [(not (pair? result))
          (error "File content is not a valid pair (Tree . String)")]
      [else
       (let ([htree (car result)]
             [compr-string (cdr result)])
         (cond
           [(not (huffman-tree? htree))
              (error "Expected a Huffman tree in file")]
           [(not (string? compr-string))
              (error "Expected a string in file")]
           [else
              (decode result)]))])))

(define (decode-result-to-file filename pair)
  (if (file-exists? filename)
        (error "File already exists")
      (let* ([port (open-output-file filename)]
             [result (decode pair)])
        (write result port)
        (newline port)
        (close-output-port port))))


;; Тестове:

(define test-list '(a b #t a c #t "asd" a 3 10 a))
(define test-pair '((node
   11
   (leaf 4 a)
   (node
    7
    (node 3 (leaf 1 b) (node 2 (leaf 1 "asd") (leaf 1 c)))
    (node 4 (node 2 (leaf 1 10) (leaf 1 3)) (leaf 2 #t))))
  .
  "01001110101111110100110111000"))

(encode test-list equal?)

(decode test-pair)


(decode (encode test-list equal?))
(encode (decode test-pair) eq?)
(encode '() eq?)
(decode '(() . ""))
(encode '(4 4 4) equal?)
(decode '((leaf 3 4) . "000"))

;(encode-data-to-file "result.txt" test-list equal?)

(encode-data-from-file "input.txt" equal?)

;(decode-result-to-file "list.txt" test-pair)

(decode-result-from-file "data.txt")

(define (p? el1 el2) #t)
(encode test-list p?)
(decode (encode test-list p?))



  















  


