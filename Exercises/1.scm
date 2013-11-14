
(define (sum-of-squares x y)
  (+ (square x) (square y)))

(define (sum-of-squares-of-two-greatests a b c)
  (cond ( (and
	   (> a c)
	   (> b c)
	   ) (sum-of-squares a b) )
	((and (> a c) (> c b)) (sum-of-squares a c) )
	((and (> b a) (> c a)) (sum-of-squares b c) )))



;;Exercise 1.6

;;Metoda oryginalna uzywa formy specjalnej if ktorej nie dotycza przedstawione metody ewaluacji wyrazen -
;;najpierw jest badana wartosc predykatu a nastepnie (jesli predykat ma wartosc "prawda") wykonuje sie akcja then bez wykonywania akcji else.
(define (sqrt-iter guess x)
       (if (good-enough? guess x)
           guess
           (sqrt-iter (improve guess x)
                      x)))

;;W tym przypadku new-if nie jest forma specjalna tylko normalna funkcja wiec nawet gdy predykat jest true wszystkie wyrazenia (argumenty) sa najpierw rozwijane
;;- w tym przypadku oznacza to nieskonczone rekurencyjne rozwijanie 3ciego wyrazenia (sqrt-iter ...)

(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
	  guess
	  (sqrt-iter (improve guess x)
		     x)))

;;Exercise 1.7
;;Dotychczas bylo:

;;blad wzgledny
(define (good-enough? guess previous-guess)
    (< (/ (abs (- guess previous-guess))
       	  guess)
       0.00000001))


(define (sqrt-iter guess previous-guess x)
  (if (good-enough? guess previous-guess)
	  guess
	  (sqrt-iter (improve guess x)
	  	     guess
		     x)))       

 (define (sqrt x)
       (sqrt-iter 1.0 2.0 x))		     


;;Exercise 1.9

          (define (+ a b)
            (if (= a 0)
                b
                (inc (+ (dec a) b))))

(+ 4 5)
(inc (+ 3 5))
(inc (inc (+ 2 5)))
(inc (inc (inc (+ 1 5))))
(inc (inc (inc (inc (+ 0 5)))))
(inc (inc (inc (inc 5)))))
(inc (inc (inc 6))))
(inc (inc 7))
(inc 8)
9


          (define (+ a b)
            (if (= a 0)
                b
                (+ (dec a) (inc b))))

(+ 4 5)
(+ 3 6)
(+ 2 7)
(+ 1 8)
(+ 0 9)
9

;;Exercise 1.10

(define (A x y)
  (cond ((= y 0) 0)
	((= x 0) (* 2 y))
	((= y 1) 2)
	(else (A (- x 1)
		 (A x (- y 1))))))


(define (f n) (A 0 n)) - n^2
(define (g n) (A 1 n)) - 2^n
(define (h n) (A 2 n)) -


;;Example: Counting change

;;

;;Exercise 1.11

f(n) = n if n<3 
f(n) = f(n - 1) + 2f(n - 2) + 3f(n - 3) if n>= 3

(define (f-recursive n)
  (if (< n 3)
      n
      (+ (f-recursive (- n 1))
	 (* 2 (f-recursive (- n 2)))
	 (* 3 (f-recursive (- n 3))))))



;;f(n) = n if n<3 
;;f(n) = f(n - 1) + 2f(n - 2) + 3f(n - 3) if n>= 3

;;f(2) = 2
;;f(1) = 1
;;f(0) = 0

;;f(3) = f(2) + 2*f(1) + 3*f(0)
;;f(4) = f(3) + 2*f(2) + 3*f(1)
;;f(5) = f(4) + 2*f(3) + 3*f(2)
;;f(6) = f(5) + 2*f(4) + 3*f(3)
;;...

;;należało by pamiętać 3 ostatnie wartości funkcji (plus licznik i n)


(define (f-iter n)
  (f-iterative 2 1 0 2 n))

(define (f-iterative fn-1 fn-2 fn-3 counter n)
  (if (= counter n)
      fn-1
      (f-iterative (+ fn-1 (* 2 fn-2) (* 3 fn-3)) fn-1 fn-2 (+ counter 1) n))))

;;(f-iterative 2 1 0 2 4)
;;(f-iterative 4 2 1 3 4)
;;(f-iterative 11 4 2 4 4)
;;11


;;Exercise 1.12

;;                  1
;;                1   1
;;              1   2   1
;;            1   3   3   1
;;          1   4   6   4   1

;;---------------------------------------------

;;  0   1   2   3   4
;;0 1
;;1 1   1
;;2 1   2   1
;;3 1   3   3   1
;;4 1   4   6   4   1

;;f(i, j) = 1, j=0
;;f(i, j) = 1, i==j
;;f(i, j) = f(i-1, j-1) + f(i-1, j)

(define (pascal i j)
  (cond ((= j 0) 1)
	((= i j) 1)
	(else
	 (+ (pascal (- i 1) (- j 1))
	    (pascal (- i 1) j)))))


;;Exercise 1.15

;;a. 
;;(12.15)/3^n < 0.1
;;121.5 < 3^n
;;powyzsze zachodzi dla n=5

;;Odp. : procedura zostanie zastosowana 5 razy

;;b.
;;(sine a)
;;(p (sine (/ a 3))
;;(p (p sine (/ a 9)))
;;...n....
;;(p (p (p (p ... (p (/ a 3^n))))))

;;a/3^n < 0.1
;;10a < 3^n
;;roof(log(3)10a) = n
;;log(3)10 + log(3)a = n
;;~log(a) podstawa staje sie nieistotna przy zlozonosci obliczeniowej 
;;Odp.:Zlozonosc zarowno czau jak i pamieci to O(log a)



;;-------------------------------
;;1.2.4 Exponentiation


(define (fast-expt b n)
  (cond ((= n 0) 1)
       ((even? n) (square (fast-expt b (/ n 2))))
       (else (* b (fast-expt b (- n 1))))))

(define (even? n)
  (= (remainder n 2) 0))

(fast-expt 2 4)
(square (fast-expt 2 (/ 4 2)))
(square (fast-expt 2 (2)))
(square (square (fast-expt 2 1)))
(square (square (* b (fast-expt b 0))))
(square (square (* b 1)))

;;Powyzszy program gdy dochodzi do n=1 przechodzi do else (bo n ani nie rowna sie 0 ani nie jest parzyste),
;;w else z n jest robione 0 i zwraca 1
;;czyli mamy (square (square ... (* b 1)))
;;czyli (square (square ... b ))

;;ile jest wyrazen square ?
;;Z kazdym zmniejszeniem n o 2 dostajemy jeden square
;;Pytamy wiec dla jakiego m
;;n/(2^m) = 1
;;n = 2^m
;;wiec zlozonosc to log(2)n
;;czyli O(n) = log(n)
							    
;;--------------------------------------
;;ja proponuje takie rozwiazanie :

(define (fast-expt b n)
  (cond ((= n 1) b)
       ((even? n) (square (fast-expt b (/ n 2))))
       (else (* b (fast-expt b (- n 1))))))

(define (even? n)
  (= (remainder n 2) 0))

;;dziala tak samo a jest bardziel czytelne wedlug mnie

;;Jeszcze raz je porownajmy - nie wierze ze autorzy TEJ ksiazki na to nie wpadli:

;;-----ich
(define (fast-expt b n)
  (cond ((= n 0) 1)
       ((even? n) (square (fast-expt b (/ n 2))))
       (else (* b (fast-expt b (- n 1))))))

;;-----moja
(define (fast-expt b n)
  (cond ((= n 1) b)
       ((even? n) (square (fast-expt b (/ n 2))))
       (else (* b (fast-expt b (- n 1))))))


;;PARZYSTE
;;porownanie dla (fast-expt 2 4)

;;----ich
;;(fast-expt 2 4)
;;(square (fast-expt 2 2))
;;(square (square (fast-expt 2 1)))
;;(square (square (* b (fast-expt b 0))))
;;(square (square (* b 1)))
;;(square (square b))
;;(square b^2)
;;b^4

;;-----moja
;;(fast-expt 2 4)
;;(square (fast-expt 2 2))
;;(square (square (fast-expt 2 1)))
;;(square (square b))
;;(square b^2)
;;b^4

;;NIEPARZYSTE
;;porownanie dla (fast-expt 2 3)

;;----ich
;;(fast-expt 2 3)
;;(* 2 (fast-expt 2 2))
;;(* 2 (square (fast-expt 2 1)))
;;(* 2 (square (* 2 (fast-expt 2 0))))
;;(* 2 (square (* 2 1)))
;;(* 2 (square 2))
;;(* 2 (2^2))
;;8

;;-----moja
;;(fast-expt 2 3)
;;(* 2 (fast-expt 2 2))
;;(* 2 (square (fast-expt 2 1)))
;;(* 2 (square 2))
;;(* 2 (2^2))
;;8


;;Exercise 1.16

(define (fast-expt-iter a b n counter)
  (if (= counter n)
      a

;;b^8
;;b^8 = b^4 * b^4

;;b^2 = b * b
;;b^4 = b^2 * b^2
;;b^8 = b^4 * b^4

;;b <= b^2

;;Najprostsza wersja iteracyjna:

(define (exp-iter b n)
  (if (even? n) (expt-iter b n)
      (* b (expt-iter b (- n 1)))))

(define (expt-iter result counter)
  (if (= counter 1)
      result
      (expt-iter (square result) (/ counter 2))))

;;(ale jest blad - gdy napiszemy (exp-iter 4 1) nic nie zwroci)

;;policzmy zlozonosc:

(expt-iter b n)
(expt-iter (square result) (/ n 2))
(expt-iter (result^4)) (/ (/ n 2) 2))
;;...
;;po log(2)n krokach otrzymamy
;;(expt-iter result^n 1)
;;result^n

(define (expt-iter result counter)
  (cond ((= counter 1) result)
	((even? counter) (expt-iter (square result) (/ counter 2)))
	(else (* result (expt-iter result (- counter 1))))))

(define (expt-iter result counter)
  (cond ((= counter 0) 1)
	((even? counter) (expt-iter (square result) (/ counter 2)))
	(else (* result (expt-iter result (- counter 1))))))

(define (exp-iter b n)
  (expt-iter 1 b n))


(define (expt-iter a b n)
  (cond ((= n 0) a)
	((even? n) (expt-iter a (square b) (/ n 2)))
	(else (expt-iter (* a b) b (- n 1)))))


(expt-iter 2 3)
(* 2 (expt-iter 2 2))
(* 2 (expt-iter 2^2 1))
(* 2 2^2)

(expt-iter 2 5)
(* 2 (expt-iter 2 4))
(* 2 (expt-iter (square 2) 2))
(* 2 (expt-iter 2^2 2))
(* 2 (expt-iter 2^4 1))
(* 2 2^4)
;;2^5


(expt-iter 2 6)
(expt-iter 2^2 3)
(* 2^2 (expt-iter 2^2 2))
(* 2^2 (expt-iter 2^4 1))
(* 2^2 2^4)
;;2^6

(expt-iter 2 12)
(expt-iter 2^2 6)
(expt-iter 2^4 3)
(* 2^4 (expt-iter 2^4 2))
(* 2^4 (expt-iter 2^6 1))
(* 2^4 2^8)
;;2^12

(define (expt-iter result counter)
  (cond ((= counter 0) 1)
	((even? counter) (expt-iter (square result) (/ counter 2)))
	(else (* result (expt-iter result (- counter 1))))))

(define (expt-iter a b n)
  (cond ((= n 0) a)
	((even? n) (expt-iter a (square b) (/ n 2)))
	(else (expt-iter (* a b) b (- n 1)))))

;;n moze byc albo liczba parzysta albo nieparzysta
;;n=2x lub n=2x+1
;;jesli n jest parzyste to algorytm wchodzi zawsze w 2gi cond, oprocz ostatniego cyklu, gdy n zostanie zredukowane do 1,
;;wtedy wchodzi w else i mnozy b(ktore wynosi wtedy b^n) * a(ktore jest 1) - zwraca a czyli b^n - proste

;;jesl n jest nieparzyste to w ktoryms z cykli alg. wejdzie w else przed zakonczeniem - pomnozy

(expt-iter 1 2 18)
(expt-iter 1 2^2 9)
(expt-iter 2^2 2^2 8)
(expt-iter 2^2 2^4 4)
(expt-iter 2^2 2^8 2)
(expt-iter 2^2 2^16 1)
(expt-iter 2^18 2^16 0)
;;2^18

(expt-iter 1 2 19)
(expt-iter 2 2 18)
(expt-iter 2 2^2 9)
(expt-iter 2^3 2^2 8)
(expt-iter 2^3 2^4 4)
(expt-iter 2^3 2^8 2)
(expt-iter 2^3 2^16 1)
(expt-iter 2^19 2^16 0)
;;2^19


;;Exercise 1.29

(define (even? n)
  (= (remainder n 2) 0))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
	 (sum term (next a) next b))))

(define (cube x) (* x x x))

(define (simpson-integral f a b n)
  (let ((h (/ (- b a) n)))
   (define (y k) (f (+ a (* k h))))
   (define (next k) (+ k 1))
   (define (term k)
     (cond ((= k 0) (y 0))
	   ((= k n) (y n))
	   ((even? k) (* 2 (y k)))
	   (else (* 4 (y k)))))
   (* (sum term 0 next n)
      (/ h 3))))


;;Exercise 1.30

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (+ result (term a)))))
  (iter a 0.0))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

;;czemu wynik wywolania :
;;(integral cube 0 1 0.001)
;;jest nieznacznie inny
;;dla sumy iteracyjnej (.24999987500000073)
;;niż dla sumy rek. (.249999875000001)
;;???


;;Exercise 1.31

(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
	 (product term (next a) next b))))

(define (identity x) x)

(define (factorial n)
  (product identity 1 (lambda (x) (+ x 1)) n))


(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (* (term a) result))))
  (iter a 1.0))

(define (factorial n)
  (product-iter identity 1 (lambda (x) (+ x 1)) n))

;;Exercise 1.32

;;null-value przypadku sum jest 0, zaś w przypadku product jest 1.
;;combiner w przypadku sum jest +, zaś w przypadku product jest *.

(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
		(accumulate combiner null-value term (next a) next b))))

;;Troche nadmiarowe jest to przekazywanie tego wszystkiego (co sie nie zmienia) przy kolejnych
;;wywolaniach - czy nie ma innego sposobu?

(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (accumulate-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (combiner (term a) result))))
  (iter a null-value))

(define (sum term a next b)
  (accumulate-iter + 0 term a next b))


;;Exercise 1.33

(define (divides? a b)
  (= (remainder b a) 0))

(define (square x) (* x x))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
	((divides? test-divisor n) test-divisor)
	(else (find-divisor n (+ test-divisor 1)))))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (filtered-accumulate predicate combiner null-value term a next b)
  (if (> a b)
      null-value
      (if (predicate a)
	  (combiner (term a)
		    (filtered-accumulate predicate combiner null-value term (next a) next b))
	  (combiner null-value
		    (filtered-accumulate predicate combiner null-value term (next a) next b)))))

(define (sum-prime-squares a b)
  (filtered-accumulate prime? + 0.0 square a (lambda (x) (+ x 1)) b))

(define (test-sum-prime-squares a b)
  (cond ((> a b) 1)
	(else (cond ((prime? a) (display (* a a)) (display " ")))
	      (test-sum-prime-squares (+ a 1) b))))



;;Exercise 1.34

(define (f g)
  (g 2))

(f f)
(f 2)
(2 2)
;;-> powyzsze wywolanie zwraca blad - proboje wywolac proc. 2 na rzecz 2, nie ma jednak proc. 2
;;(The object 2 is not applicable.)