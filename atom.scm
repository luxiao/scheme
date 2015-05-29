(define atom?
    (lambda (x)
        (and (not (pair? x)) (not (null? x)))))

(define insertR
  (lambda (n o lat)
    (cond
      ((null? lat) ())
      (else (cond
	      ((eq? o (car lat)) (cons o (cons n (insertR n o (cdr lat)))))
	      (else (cons (car lat) (insertR n o (cdr lat)))))))))

(define subst
  (lambda (n o lat)
    (cond
      ((null? lat) ())
      (else (cond
	      ((eq? o (car lat)) (cons n (subst n o (cdr lat))))
	      (else (cons (car lat) (subst n o (cdr lat)))))))))

(define add1
  (lambda (n)
    (+ n 1)))

(define sub1
  (lambda (n)
    (- n 1)))

(define add2
   (lambda (n m)
     (cond
       ((zero? m) n)
       (else (add1 (add2 n (sub1 m)))))))

(define sub2
(lambda (n m)
(cond
((zero? m) n)
(else (sub1 (sub2 n (sub1 m)))))))

(define addtup
(lambda (tup)
(cond
((null? tup) 0)
(else (add2 (car tup) (addtup (cdr tup)))))))

(define mtp2
  (lambda (n m)
    (cond
      ((zero? m) 0)
      (else (add2 n (mtp2 n (sub1 m)))))))

(define tup+
  (lambda (atup btup)
    (cond
      ((and (null? atup) (null? btup)) ())
      (else (cons (add2 (car atup) (car btup)) (tup+ (cdr atup) (cdr btup)))))))

(define gt
  (lambda (n m)
    (cond
      ((zero? n) #f)
      ((zero? m) #t)
      (else (gt (sub1 n) (sub1 m))))))

(define lt
  (lambda (n m)
    (cond
      ((zero? m) #f)
      ((zero? n) #t)
      (else (lt (sub1 n) (sub1 m))))))

(define mylt
  (lambda (n m)
    (gt m n)))

(define eq
  (lambda (n m)
    (cond
      ((gt n m) #f)
      ((lt n m) #f)
      (else #t))))


(define myeq
  (lambda (n m)
      (and (not (gt n m)) (not (lt n m)))))

(define pow
  (lambda (n m)
    (cond
      ((zero? m) 1)
      (else (mtp2 n (pow n (sub1 m)))))))

(define div
  (lambda (n m)
    (cond
      ((< n m) 0)
      (else (add1 (div (sub2 n m) m))))))

(define len
  (lambda (lat)
    (cond
      ((null? lat) 0)
      (else (add1 (len (cdr lat)))))))

(define pick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (car lat))
      (else (pick (sub1 n) (cdr lat))))))

(define rempick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (cdr lat))
      (else (cons (car lat) (rempick (sub1 n) (cdr lat)))))))

(define non_nums
  (lambda (lat)
    (cond
      ((null? lat) ())
      (else 
	(cond
	  ((number? (car lat)) (non_nums (cdr lat)))
	  (else (cons (car lat) (non_nums (cdr lat)))))))))

(define all_nums
  (lambda (lat)
    (cond
      ((null? lat) ())
      (else
	(cond
	  ((number? (car lat)) (cons (car lat) (all_nums (cdr lat))))
	  (else (all_nums (cdr lat))))))))

(define occur
  (lambda (a lat)
    (cond
      ((null? lat) 0)
      (else (cond
	      ((eq? a (car lat)) (add1 (occur a (cdr lat))))
	      (else (occur a (cdr lat))))))))

(define one?
  (lambda (n)
    (cond
      ((zero? n) #f)
      (else (zero? (sub1 n))))))

(define rember_all
  (lambda (a l)
    (cond
      ((null? l) ())
      ((atom? (car l))
       (cond ((eq? a (car l)) (rember_all a (cdr l)))
	     (else (cons (car l) (rember_all a (cdr l))))))       
      (else (cons (rember_all a (car l)) (rember_all a (cdr l)))))))
;(rember_all 'cup '(abc ((abc) cup) ((abc (cup)) cup)))

(define insertR*
  (lambda (n o l)
    (cond
      ((null? l) ())
      ((atom? (car l))
       (cond
	 ((eq? (car l) o) (cons o (cons n (insertR* n o (cdr l)))))
	 (else (cons (car l) (insertR* n o (cdr l))))))
      (else (cons (insertR* n o (car l)) (insertR* n o (cdr l)))))))

(define occur*
  (lambda (a l)
    (cond
      ((null? l) 0)
      ((atom? (car l))
       (cond
	 ((eq? a (car l)) (add1 (occur* a (cdr l))))
	 (else (occur* a (cdr l)))))
      (else (add2 (occur* a (car l)) (occur* a (cdr l)))))))
;(occur* 'banana '((banana) (split ((((banana ice))) (cream (banana)) sherbet)) (banana) (bread) (banana brandy)))

(define subst*
  (lambda (n o l)
    (cond
      ((null? l) ())
      ((atom? (car l))
       (cond
	 ((eq? (car l) o) (cons n (subst* n o (cdr l))))
	 (else (cons (car l) (subst* n o (cdr l))))))
      (else (cons (subst* n o (car l)) (subst* n o (cdr l)))))))

(define member*
  (lambda (m l)
    (cond
      ((null? l) #f)
      ((atom? (car l)) 
       (or 
	 (eq? m (car l))
	 (member* m (cdr l))))
      (else (or
	      (member* m (car l))
	      (member* m (cdr l)))))))

(define leftmost
  (lambda (l)
    (cond
      ((atom? (car l)) (car l))
      (else (leftmost (car l))))))

(define eqlat
  (lambda (al bl)
    (cond
      ((null? al) (null? bl))
      (else (cond
	      ((null? bl) #f)
	      (else (and (eq? (car al) (car bl)) (eqlat (cdr al) (cdr bl)))))))))

(define eqlist
  (lambda (al bl)
    (cond
      ((null? al) (null? bl))
      (else 
	(cond
	  ((null? bl) #f)
	  ((atom? (car al)) 
	   (cond 
	     ((atom? (car bl)) (and (eq? (car al) (car bl)) (eqlist (cdr al) (cdr bl))))
	     (else #f)))
	  (else 
	    (cond
	      ((atom? (car bl)) #f)
	      (else (and (eqlist (car al) (car bl)) (eqlist (cdr al) (cdr bl)))))))))))
