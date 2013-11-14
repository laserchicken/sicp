;;Exercise 2.1
;;Oryginalna definicja konstruktora:

(define (numer x) (car x))

(define (denom x) (cdr x))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))

(define (Make-rat n d)
  (let ((g (gcd n d)))
    (if (< d 0)
	(cons (* -1 (/ n g)) (* -1 (/ d g)))
	(cons (/ n g) (/ d g)))))


;;Exercise 2.2

;;#a i b to odcinki (segment)
(define (make-rectangle a b)
  (cons a b))

(define (rect-a rect)
  (car rect))

(define (rect-b rect)
  (cdr rect))

(define (perimeter rect)
  (+ (* 2 (rect-a rect))
     (* 2 (rect-b rect))))

(define (area rect)
  (* (rect-a rect)
     (rect-b rect)))

;;Exercise 2.4

(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

;;Na wstepie warto sobie przypomniec definicje lambda:
;;     (lambda                     (x)     (+   x     4))
;;         |                        |       |   |     |
;;     the procedure of an argument x that adds x and 4


;;nalezy pokazac ze (car (cons x y)) daje x

;;(car (cons x y)) <=>
;;(car (lambda (m) (m x y))) <=>
;;((lambda (m) (m x y)) (lambda (p q) p)) =>
;;((lambda (p q) p) x y) =>
;;x

(define (cdr z)
  (z (lambda (p q) q)))

;;Exercise 2.5
;;(a,b) => (2^a * 3^b)

(define (cons a b)
  (* (expt 2 a)
     (expt 3 b)))

;;Po pierwsze należy pokazać że
;;f(a,b) => (2^a * 3^b)
;;zwraca unikalną wartość dla każdego punktu (a,b)



;;Exercise 2.17

(define (last-pair list)
  (if (null? (cdr list))
      list
      (last-pair (cdr list))))

;;Exercise 2.18

(define (reverse list)
  (reverse-iter list '()))

(define (reverse-iter list reversed-list)
  (if (null? list)
      reversed-list
      (reverse-iter (cdr list) (cons (car list) reversed-list))))

;;Czy da sie to napisac rekurencyjnie?

;;Chcielibyśmy mieś takie coś:
;;(cons n-1 (cons n-2 (cons n-3 (... (cons 0 '())))))


;;W książce The Scheme Programming Language jest cos takiego:

(define reverse
  (lambda (ls)
    (let rev ((ls ls) (new '()))
      (if (null? ls)
          new
          (rev (cdr ls) (cons (car ls) new))))))

;;Czyli też iteracyjnie.
;;Moją uwagę zwrócił szczególny format let, na wiki wyczytałem że
;;A variant of let, the "named let" form, has an identifier after the let keyword.
;;This binds the let variables to the argument of a procedure whose name is the given identifier and whose body is the body of the let form.
;;The body may be repeated as desired by calling the procedure. The named let is widely used to implement iteration.
(let loop ((n 1))
  (if (> n 10)
      '()
      (cons n
            (loop (+ n 1)))))
;;=> (1 2 3 4 5 6 7 8 9 10)

;;Czyli to oznacza ze takie let definiuje procedure o danej nazwie i podaje jej wartosc poczatkowa
;;(która jest wykorzystywana tylko na starcie).

;;W związku z powyższym procedure reverse z książki The Scheme... (dalej nazywana "tspl") można by
;;zapisac jako:

(define (rev ls new)
  (if (null? ls)
      new
      (rev (cdr ls) (cons (car ls) new))))

(define (reverse ls)
  (rev ls '()))

;;Pomoca takiej formy let mozna pisac procedury iteracyjne bez rozbijania ich na dwie (wlasciwa procedura + procedura inicjalizujaca).

;;Exercise 2.20

(define (even? n)
  (= (remainder n 2) 0))

(define (same-parity x . list)
  (if (null? list)
      x
      (if (even? x)
	  (get-even-list list)
	  (get-odd-list list))))

(define (get-even-list list)
  (if (null? list)
      '()
      (if (even? (car list))
	  (cons (car list)
		(get-even-list (cdr list)))
	  (get-even-list (cdr list)))))

(define (get-odd-list list)
  (if (null? list)
      '()
      (if (even? (car list))
	  (get-odd-list (cdr list))
	  (cons (car list)
		(get-odd-list (cdr list))))))

;;Exercise 2.21

(define (map proc items)
  (if (null? items)
      '()
      (cons (proc (car items))
	    (map proc (cdr items)))))

(define (square-list items)
  (if (null? items)
      '()
      (cons (* (car items) (car items))
	    (square-list (cdr items)))))

(define (square-list items)
  (map (lambda (x) (* x x))
       items))

;;Exercise 2.22

(define (square x)
  (* x x))

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
	answer
	(iter (cdr things)
	      (cons (square (car things))
		    answer))))
  (iter items '()))

;;Operacja

(cons (sqare (car things))
	       (answer))

;;powoduje dopisywanie kwadratow elementow listy (thigs) na początek wynikowej tablicy (answer).
;;Iterujemy po elementach tablicy things w normalnym porzadku (cdr things)
;;a wiec dodawanie do tablicy wynikowej na poczatku powoduje odwrocenie porzadku.

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
	answer
	(iter (cdr things)
	      (cons answer
		    (square (car things))))))
  (iter items '()))

;;Operacja

(cons answer
      (square (car things)))

;;powoduje dopisanie listy do listy jednoelementowej, np.:
(cons (list 1 2 3) 4)
;;=>
((1 2 3) . 4)
;;(kropka oznacza ze stworzono pare - ((lista) . atom)
;;lub tak:
(cons (list 1 2 3) (list 4))
;;=>
((1 2 3) 4)
;;(bez kropki oznacza ze stowrzono liste - ((lista) lista)

;;Dzialanie tego procesu powoduje zagniezdzanie listy na liscie na liscie ...


;;Exercise 2.23

(for-each (lambda (x) (newline) (display x))
	  (list 57 321 88))

(define (for-each proc items)
  (cond ((null? items) )
	(else
	 (proc (car items))
	 (for-each proc (cdr items)))))


(define (count-leaves x)
  (cond ((null? x) 0)
	((not (pair? x)) 1)
	(else (+ (count-leaves (car x))
		 (count-leaves (cdr x))))))

;;np.:
((1 2) 3 4)
(+ (count-leaves '(1 2))
   (count-leaves '(3 4))
;;=>
(+ (+ (count-leaves 1)
      (count-leaves 2))
   (+ (count-leaves 3)
      (count-leaves 4)))
;;=>
;;4


;;Exercise 2.25

(define list '(1 3 (5 7) 9))
(cdr (car (cdr (cdr list))))

(define list '((7)))
(car (car list))

(define list '(1 (2 (3 (4 (5 (6 7)))))))
(cdr (cdr (cdr (cdr (cdr list)))))


;;Exercise 2.26

(define x (list 1 2 3))
(define y (list 4 5 6))

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

;;(append x y) -> '(1 2 3 4 5 6)
;;(cons x y) -> ((1 2 3) 4 5 6)

;;(list <A_1> <A_2> ... <A_N>)
;;<=>
;;(cons <A_1>
;;      (cons <A_2>
;;	    (cons ...
;;		  (cons <A_N>
;;			nil)
;;		  ...)))


;;(list x y) ->
;;(list (list 1 2 3) (list 4 5 6))
;;(cons (list 1 2 3) (cons (list 4 5 6) nil))
;;(cons (list 1 2 3) ((4 5 6)))
;;((1 2 3) (4 5 6))



;;Exercise 2.27

(define (reverse list)
  (reverse-sublists-iter list '()))

(define (reverse-sublists-iter list reversed-list)
  (if (null? list)
      reversed-list
      (if (not (pair? (car list)))
	  (reverse-sublists-iter (cdr list) (cons (car list) reversed-list))
	  (reverse-sublists-iter (cdr list) (cons (reverse (car list)) reversed-list)))))
