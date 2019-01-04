;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname bignum) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t write repeating-decimal #f #t none #f () #f)))
;; Input: two natural numbers between 0 and 99, a and b
;; Output: the sum of the a and b, or an error if
;;         a, b or their sum is outside the range [0,99]

(define digit-add 
  (lambda (a b)
    (if (and (integer? a)
            (integer? b)
            (<= 0 a)
            (<= a 99)
            (<= 0 b)
            (<= b 99)
            (<= (+ a b) 99))
        (+ a b)
        (error 'add "digit-add inputs or output out of range. tried (+ ~a ~a) " a b))))



;; Input: two natural numbers between 0 and 99, a and b
;; Output: the result of subtractig b from a, or an error if
;;         a or b or their difference is outside the range [0,99]

(define digit-sub 
  (lambda (a b)
    (if (and
         (integer? a)
         (integer? b)
         (>= a 0)
         (>= b 0)
         (<= a 99)
         (<= b 99)
         (>= (- a b) 0)
         (<= (- a b) 99))
      (- a b)
      (error 'add "digit-sub inputs or output out of range. tried (- ~a ~a) " a b))))



;; Input: two natural numbers between 0 and 99, a and b
;; Output: the product of the inputs, or an error if
;;         a or b or their product is outside the range [0,99]

(define digit-mult 
  (lambda (a b)
    (if (and (integer? a)
            (integer? b)
            (<= 0 a)
            (<= a 99)
            (<= 0 b)
            (<= b 99)
            (<= (* a b) 99))
        (* a b)
        (error 'add "digit-mult inputs or output out of range. tried (* ~a ~a) " a b))))



;; Input: two natural numbers between 0 and 99, a and b
;; Output: the quotient of dividend a and divisor b, or an error if
;;         a or b or their quotient is outside the range [0,99]

(define digit-quo 
  (lambda (a b)
    (if (and (integer? a)
            (integer? b)
            (<= 0 a)
            (<= a 99)
            (<= 0 b)
            (<= b 99))
        (quotient a b)
        (error 'add "digit-quo inputs or output out of range. tried (quotient ~a ~a) " a b))))



;; Input: two natural numbers between 0 and 99, a and b
;; Output: the remainder of dividend a and divisor b, or an error if
;;         a or b or the remainder is outside the range [0,99]

(define digit-rem 
  (lambda (a b)
    (if (and (integer? a)
            (integer? b)
            (<= 0 a)
            (<= a 99)
            (<= 0 b)
            (<= b 99))
        (remainder a b)
        (error 'add "digit-rem inputs or output out of range. tried (remainder ~a ~a) " a b))))



;; Input: two lists of integers, aloi1 aloi2, in reverse order
;;        with non-negative elements less than or equal to 9
;; Output: an integer list consisting of the sums
;;         of L1 and L2 in reverse order; elements may be
;;         greater than 9 (but less than or equal to 18)

;; Recursion Diagrams
;; Original Input: (3 9 1) (2 4 1)
;;   Recursive Input: (9 1) (4 1)
;;   Recursive Output: (13 2)
;; Original Output: (5 13 2)

;; Original Input: (9) (9 9 9)
;;   Recursive Input: () (9 9)
;;   Recursive Output: (9 9)
;; Original Output: (18 9 9)

(define plain-adding
  (lambda (aloi1 aloi2)
    (cond
      [(empty? aloi1) aloi2]
      [(empty? aloi2) aloi1]
      [#true
       (cons (digit-add (car aloi1) (car aloi2))
             (plain-adding (cdr aloi1) (cdr aloi2)))])))

;; test cases
(check-expect (plain-adding '() '()) '())
(check-expect (plain-adding '() '(1 3 3 3)) '(1 3 3 3))
(check-expect (plain-adding '(9 9 9 9) '()) '(9 9 9 9))
(check-expect (plain-adding '(9 9 9 9 9) '(8 8 8 8 8)) '(17 17 17 17 17))
(check-expect (plain-adding '(9 9 9 9 9 9) '(8 8 8 8 8)) '(17 17 17 17 17 9))



;; Input: a list of numbers, aloi, in reverse order
;;        where each element is a non-negative integer
;;        and less than or equal to 81
;; Output: a list of integers in reverse order
;;         where the first element is non-negative
;;         and less than or equal to 9

(define carry-helper
  (lambda (aloi)
    (if (empty? (cdr aloi))
        (cons (digit-rem (car aloi) 10)
              (cons (digit-quo (car aloi) 10) empty))
        (cons (digit-rem (car aloi) 10)
              (cons (+ (digit-quo (car aloi) 10) (car (cdr aloi)))
                    (cdr (cdr aloi)))))))

;; test cases
(check-expect (carry-helper (quote (81))) (quote (1 8)))
(check-expect (carry-helper (quote (23 0))) (quote (3 2)))
(check-expect (carry-helper (quote (63 24 0 1 5 77))) (quote (3 30 0 1 5 77)))



;; Input: a list of numbers, aloi, in reverse order
;;        where each element is a non-negative integer
;;        and less than or equal to 81
;; Output: a list of numbers in reverse order
;;         where each element is a non-negative integer
;;         and less than or equal to 9

;; Recursion Diagrams
;; Original Input: (5 5)
;;   Recursive Input: (5)
;;   Recursive Output: (5)
;; Original Output: (5 5)

;; Original Input: (82 82)
;;   Recursive Input: (90)
;;   Recursive Output: (0 9)
;; Original Output: (2 0 9)

(define carry
  (lambda (aloi)
    (cond
      [(empty? aloi) empty]
      [#true (if (> (car aloi) 9)
                 (cons (digit-rem (car aloi) 10)
                       (carry (cdr (carry-helper aloi))))
                 (cons (car aloi) (carry (cdr aloi))))])))

;; test cases
(check-expect (carry empty) empty)
(check-expect (carry (quote (0))) (quote (0)))
(check-expect (carry (quote (3))) (quote (3)))
(check-expect (carry (quote (81))) (quote (1 8)))
(check-expect (carry (quote (0 0 0 44))) (quote (0 0 0 4 4)))
(check-expect (carry (quote (4 6 0 1 6 8))) (quote (4 6 0 1 6 8)))
(check-expect (carry (quote (36 77 21 1 45))) (quote (6 0 9 3 5 4))) 



;; Input: two bignums, L1 L2, or lists of numbers in reverse order
;;        where each element is a non-negative integer
;;        and less than or equal to 9
;; Output: a bignum that represents the sum of L1 and L2

(define bignum+
  (lambda (L1 L2)
    (carry (plain-adding L1 L2))))

;; test cases
(check-expect (bignum+ '() '()) '())
(check-expect (bignum+ '() '(2 3 1)) '(2 3 1))
(check-expect (bignum+ '(0 2 4) '()) '(0 2 4))
(check-expect (bignum+ '(0 2 4) '(2 3 1)) '(2 5 5))
(check-expect (bignum+ '(9 9 9) '(1)) '(0 0 0 1))
(check-expect (bignum+ '(1) '(9 9 9)) '(0 0 0 1))
(check-expect (bignum+ '(7 6 5 4 3 2 1) '(0 1 2 3 4 5 6)) '(7 7 7 7 7 7 7))



;; Input: an integer, num, between 0 and 9, inclusive;
;;        a list of numbers, alon, where each element is an integer
;;        between 0 and 9, inclusive
;; Output: a list of integers between 0 and 81, inclusive,
;;         that is the result of the multiplication of num
;;         and each element in alon

;; Recursion Diagrams
;; Original Input: 1 (1 2 3 4 5)
;;   Recursive Input: 1 (2 3 4 5)
;;   Recursive Output: (2 3 4 5)
;; Original Output: (1 2 3 4 5)

;; Original Input: 8 (0 0 0 0)
;;   Recursive Input: 8 (0 0 0)
;;   Recursive Output: (0 0 0)
;; Original Output: (0 0 0 0)

(define partial-multiplication
  (lambda (num alon)
    (cond
      [(empty? alon) empty]
      [#true (cons
              (digit-mult num (car alon))
              (partial-multiplication num (cdr alon)))])))

;; test cases
(check-expect (partial-multiplication 4 '()) '())
(check-expect (partial-multiplication 9 '(1)) '(9))
(check-expect (partial-multiplication 1 '(1 1)) '(1 1))
(check-expect (partial-multiplication 0 '(1 4 2 5 1)) '(0 0 0 0 0))
(check-expect (partial-multiplication 4 '(1 4 2 5 1)) '(4 16 8 20 4))



;; Input: two bignums L1 and L2
;; Output: a bignum that represents the product of L1 and L2

;; Recursion Diagrams
;; Original Input: (1 2 3) (4 5 6)
;;   Recursive Input: (2 3) (4 5 6)
;;   Recursive Output: (8 2 9 0 2)
;; Original Output: (4 3 9 9 0 2)

;; Original Input: (9 9) (9 9 9)
;;   Recursive Input: (9) (9 9 9)
;;   Recursive Output: (1 9 9 8)
;; Original Output: (1 0 9 8 9)

(define bignum*
  (lambda (L1 L2)
    (cond
      [(or (empty? L1) (empty? L2)) empty]
      [#true (bignum+ (carry (partial-multiplication (car L1) L2))
                      (cons 0 (bignum* (cdr L1) L2)))])))

;; test cases
(check-expect (bignum* '() '())'())
(check-expect (bignum* '() '(1 2 3))'())
(check-expect (bignum* '(1 2 3) '())'())
(check-expect (bignum* '(7 2 5 1) '(6 1 9 3 0 2)) '(2 3 7 9 7 3 1 1 3))
(check-expect (bignum* '(6 1 9 3 0 2) '(7 2 5 1)) '(2 3 7 9 7 3 1 1 3))
(check-expect (bignum* '(9 3 0 2) '(7 2 5 1)) '(3 5 5 3 1 1 3))
(check-expect (bignum* '(7 2 5 1) '(9 3 0 2)) '(3 5 5 3 1 1 3))
(check-expect (bignum* '(1 2 3 4 5) '(1 2 3 4 5)) '(1 4 0 1 7 7 0 5 9 2))
(check-expect (bignum* '(1 1 1 1 1) '(1 1 1 1 1)) '(1 2 3 4 5 4 3 2 1))