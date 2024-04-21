;(load "C:/Users/mpmatess/Downloads/Mike/METI/Algorithms for Encoding/encoder.lisp")
#|
Pick concept
;(find-all-rules concept)
(create-message 'length)
(hider message)
mapping
hid-lis
(setf code (copy-list hid-lis))
(check-all code)
|#

(defun create-message (target)
  (find-all-rules target))

(defun find-all-rules (post)
  (setf rule-lis nil)
  (setf pre-lis nil)
  (find-pre-rules post 0)
  (setf rule-lis (sort rule-lis '> :key 'cadr))
;  (format t "rules: ~a~&" rule-lis)
  (show-forms rule-lis))

(defun find-pre-rules (post lvl)
  (dolist (pp prepost)
    (cond ((and (member post (get-post pp))
		(not (member (get-rule pp) rule-lis)))
	   (cond ((null (get-pre pp))
		  (push (list (get-rule pp) lvl) rule-lis); :key 'car :test 'equal)
		  (return)))
	   (push (list (get-rule pp) lvl) rule-lis); :key 'car :test 'equal)
	   (dolist (pre (get-pre pp))
	     (find-pre-rules pre (1+ lvl)))))))

(defun show-forms (rules)
  (setf message nil)
  (let ((lis nil))
   (dolist (i rules)
     (cond ((not (member (car i) lis :key 'car))
	    (push i lis)
	    (dolist (j (get-forms (find-rule (car i))))
	      (push j message)
	      (format t "~a~&" j)))))
   (format t "rules: ~a~&" lis)
   (setf message (reverse message))
))

(defun find-rule (rule)
  (dolist (pp prepost)
    (if (equal (get-rule pp) rule)
	(return pp))))

(defun get-rule (pp)
  (first pp))

(defun get-pre (pp)
  (second pp))

(defun get-forms (pp)
  (third pp))

(defun get-post (pp)
  (fourth pp))
(setf prepost 
  '(
    (equality-rule
     nil
     ((1 = 1) (2 = 2))
     (equality))
    (counting-rule
     (equality)
     ((1 = [ o ]) (2 = [ o o ]) (3 = [ o o o ]) (4 = [ o o o o ]) 
      (5 = [ o o o o o ]) (6 = [ o o o o o o ]) (7 = [ o o o o o o o ])
      (8 = [ o o o o o o o o ]) (9 = [ o o o o o o o o o ]) (0 = [ ])
      )
     (numbers))
    (addition-rule
     (numbers equality)
     ((4 + 5 = 9))
     (addition))
    (subtraction-rule
     (numbers equality)
     ((9 - 4 = 5) (4 - 9 = - 5))
     (addition))
    (multiplication-rule
     (numbers equality addition)
     ((2 * 3 = 6))
     (multiplication))
    (division-rule
     (numbers equality addition)
     ((6 / 2 = 3))
     (division))
    (power-rule
     (numbers equality multiplication division) ; division maintains PEMDAS
     ((2 ^ 3 = 8))
     (power))
    (sqrt-rule
     (numbers equality multiplication division)
     ((sqrt [ 9 ] = 3))
     (power))
    (base10-rule
     (addition numbers equality)
     ((9 + 1 = 1 0) (0 "." 1 = 1 / 1 0) (3 "." 1 = 3 + 1 / 1 0))
     (base10))
    (decimal-approx-rule
     (numbers division base decimal)
     ((1 / 3 approx 0 "." 3 3) (1 / 3 approx 0 "." 3 3 3))
     (approx))
;    (whole-approx-rule
;     (base division numbers)
;     ((pi approx 3) (pi approx 2 2 / 7) (pi approx 3 3 3 / 1 0 6))
;     (approx))
;    (decimal-approx-rule
;     (numbers division base decimal)
;     ((pi approx 3) (pi approx 3 "." 1 4) (pi approx 3 "." 1 4 1 5 9))
;     (approx))
;    (alpha-ratio-rule
;     (approx base division numbers)
;     ((alpha approx 1 / 1 3 7))
;     (alpha))
    (mass-ratio-rule
     (numbers division base10)
     ((mass [ proton ] / mass [ electron ] approx 1 8 3 6))
     (mass proton electron))
    (alpha-coulomb-rule
     (power numbers multiplication division approx base10)
     (((e ^ 2) * k_e / ( c * hbar ) approx 1 / 1 3 7))
     (alpha e k_e))
    (planck-rule
     (equality numbers sqrt multiplication division power subtraction)
     ((planck [ length ] = sqrt [ [ hbar * G ] / [ c ^ 3 ] ])
      (planck [ mass ] = sqrt [ [ hbar * c ] / G ])
      (planck [ time ] = sqrt [ [ hbar * G ] / [ c ^ 5 ] ]))
     (planck length mass time))
;      (planck [ length ] = c ^ ( - 3 / 2 ) * G ^ ( 1 / 2 ) * hbar ^ ( 1 / 2 )))
;      (planck [ mass ] = c ^ ( 1 / 2 ) * G ^ ( - 1 / 2 ) * hbar ^ ( 1 / 2 )))
    (c-rule
     (equality sqrt multiplication power subtraction)
     ((c = [ planck [ length ] ] ^ 1 * [ planck [ mass ] ] ^ 0 * [ planck [ time ] ] ^ -1 ]))
     (planck c))
    (hbar-rule
     (equality sqrt multiplication power subtraction)
     ((hbar = [ planck [ length ] ] ^ 2 * [ planck [ mass ] ] ^ 1 * [ planck [ time ] ] ^ -1 ]))
     (planck hbar))
    (G-rule
     (equality sqrt multiplication power subtraction)
     ((G = [ planck [ length ] ] ^ 3 * [ planck [ mass ] ] ^ -1 * [ planck [ time ] ] ^ -2 ]))
     (planck G))
))

#|
((ADDITION-CHECK 3) (EQUALITY-CHECK 3) 
 (BASE-CHECK 2) (COUNTING-CHECK 2) 
 (WHOLE-APPROX-CHECK 1) (DIVISION-CHECK 1) (POWER-CHECK 1) 
 (ALPHA-COULOMB-CHECK 0))

((EQUALITY-CHECK 4) 
 (COUNTING-CHECK 3) 
 (ADDITION-CHECK 2) 
 (DIVISION-CHECK 1) (BASE-CHECK 1) 
 (WHOLE-APPROX-CHECK 0))
|#

#|
decoder.lisp
|#

(defun check-all (lis)
  (setf hyp-lis nil)
  (equal-check lis)
  (hyp-substitute lis)
;  (bracket-check lis)
;  (hyp-substitute lis)
  (count-check lis)
  (hyp-substitute lis)
  (count-check lis)
  (hyp-substitute lis)
  (fun-check lis)
  (hyp-substitute lis)
  (base-check lis)
  (hyp-substitute lis)
  (approx-check lis)
  (hyp-substitute lis)
  (constant-check lis)
  (hyp-substitute lis)
  (planck-check lis)
  (hyp-prune)
  (hyp-substitute lis)
;  (sort-hyp)
;  (dolist (i hyp-lis) (print i))
  (dolist (i code) (print i))
)

(defun sort-hyp ()
  (setf lis (copy-list hyp-lis))
  (setf lis (sort lis '< :key 's2num))
  (dolist (i lis)
    (format t "~a~&" i)))

(defun hyp-substitute (lis)
  (dolist (i (copy-list hyp-lis))
    (cond ((equal (length (second i)) 1)
	   (nsubst (first (second i)) (first i) lis :test 'equal)))))

(defun s2num (x)
  (let ((str (format nil "~a" (first x))))
    (read-from-string (subseq str 1 (length str)))))

;(s1 s2 s1) (s3 s2 s3)
(defun equal-check (lis)
  (dolist (ex lis)
    (cond ((and (equal (length ex) 3)
		(equal (first ex) (third ex)))
	   (add-hyp (second ex) '(=))))))

#|
;(s02 = s2 1 1 s3)
;(s03 = s2 1 1 1 s3)
(defun bracket-check (lis)
  (dolist (ex lis)
    (cond ((and (equal (nth 1 ex) '=)
		(nth 5 ex)
		(equal (nth 3 ex) (nth 4 ex))
		(not (equal (nth 4 ex) (nth 5 ex))))
	   (add-hyp (nth 2 ex) '([))
	   (add-hyp (nth 5 ex) '(])))
	  ((and (equal (nth 1 ex) '=)
		(nth 6 ex)
		(equal (nth 3 ex) (nth 4 ex))
		(equal (nth 4 ex) (nth 5 ex))
		(not (equal (nth 5 ex) (nth 6 ex))))
	   (add-hyp (nth 2 ex) '([))
	   (add-hyp (nth 6 ex) '(])))
)))
|#
    
;(s04 = [ 1 1 1 1 ])
(defun count-check (lis)
  (let ((c-start nil) (c-end nil) (c-lis nil))
    (dolist (ex lis)
(format t "EX: ~a~&" ex)
    (cond ((and (setf c-start (position '[ ex))
		  (setf c-end (position '] ex))
		  (equal (second ex) '=))
	     (cond ((equal c-end (1+ c-start))
		    (add-hyp (first ex) '(0)))
		   ((all-equal-p (setf c-lis (subseq ex (1+ (position '[ ex)) (position '] ex))))
		    (add-hyp (nth (1+ c-start) ex) '(o))
		    (add-hyp (first ex) (list (length c-lis))))))
	  ((and (equal (nth 1 ex) '=)
		(nth 5 ex)
		(equal (nth 3 ex) (nth 4 ex))
		(not (equal (nth 4 ex) (nth 5 ex))))
	   (add-hyp (nth 2 ex) '([))
	   (add-hyp (nth 5 ex) '(])))
	  ((and (equal (nth 1 ex) '=)
		(nth 6 ex)
		(equal (nth 3 ex) (nth 4 ex))
		(equal (nth 4 ex) (nth 5 ex))
		(not (equal (nth 5 ex) (nth 6 ex))))
	   (add-hyp (nth 2 ex) '([))
	   (add-hyp (nth 6 ex) '(])))
))))
		  
(defun all-equal-p (list)
  (or (null (rest list)) ;; singleton/empty
      (and (equalp (first list)
                   (second list))
           (all-equal-p (rest list)))))

(defun fun-check (lis)
  (dolist (ex lis)
;(6 s4 2 = 3) (6 s4 3 = 2)
    (cond ((and (equal (length ex) 5)
		(numberp (first ex))
		(numberp (third ex))
		(equal (fourth ex) '=))
;	   (add-hyp 'notation '(infix))
	   (fun-hyp (second ex) (first ex) (third ex) (fifth ex) ex))
;( s1 ( 1 & 1 ) = 2 )
	  ((and (equal (length ex) 8)
		(numberp (nth 2 ex))
		(numberp (nth 4 ex))
		(equal (nth 6 ex) '=)
		(equal (nth 3 ex) '&))
;	   (add-hyp 'notation '(prefix))
	   (fun-hyp (first ex) (nth 2 ex) (nth 4 ex) (nth 7 ex) ex))
;(S19 [ 9 ] = 3) 
	  ((and (equal (length ex) 6)
		(numberp (nth 2 ex))
		(numberp (nth 5 ex))
		(equal (nth 4 ex) '=)
		(equal (sqrt (nth 2 ex)) (nth 5 ex)))
	   (add-hyp (first ex) '(root)))
)))

(defun fun-hyp (fun-sym arg1 arg2 val ex)
  (cond ((not (setf fun-lis (second (first (member fun-sym hyp-lis :key 'first :test 'equal)))))
	 (add-hyp fun-sym '(+ - * / expt))
	 (setf fun-lis '(+ - * / expt))))
  (dolist (fun fun-lis)
    (cond ((or (and (equal fun '/) (zerop arg2))
	       (not (equal (eval `(,fun ,arg1 ,arg2)) val)))
	   (setf hyp-lis (substitute (list fun-sym (remove fun fun-lis)) 
				     (list fun-sym fun-lis)
				     hyp-lis :test 'equal))
	   (setf fun-lis (remove fun fun-lis))
;	   (format t "~a ~a ~a ~a ~a~&" ex #\tab #\tab #\tab (list fun-sym (remove fun fun-lis)) )
	   )))
;  (format t "~a ~a ~a~&" ex #\tab hyp-lis)
)

(defun base-check (lis)
  (dolist (ex lis)
;(9 + 1 = 1 0)
    (cond ((and (equal (length ex) 6)
		(equal (fourth ex) '=)
		(equal (sixth ex) 0)
		(or (and (equal (first ex) 1)
			 (add-hyp 'base (list (+ 1 (third ex))))
			 )
		    (and (equal (third ex) 1)
			 (add-hyp 'base (list (+ 1 (first ex))))
			 ))))
;(1 s5 0 = 1)
	  ((and (equal (length ex) 5)
		(equal (first ex) 1) (equal (third ex) 0)
		(equal (fourth ex) '=) (equal (fifth ex) 1))
	   (add-hyp (second ex) '(".")))
;(0 s5 1 = 1 / 1 0)
	  ((and (equal (length ex) 8)
		(equal (first ex) 0) (equal (third ex) 1)
		(equal (fourth ex) '=) (equal (cddddr ex) '(1 / 1 0)))
	   (add-hyp (second ex) '(".")))
)))

;(1 / 3 S21 0 "." 3 3 3) 
(defun approx-check (lis)
  (dolist (ex lis)
    (cond ((and (equal (nth 0 ex) 1)
		(equal (nth 1 ex) '/)
		(not (equal (nth 3 ex) '=))
		(not (zerop (nth 2 ex)))
		(equal (nth 5 ex) ".")
		(approx-p (/ 1 (nth 2 ex)) (lis2dec (cddddr ex))))
	   (add-hyp (nth 3 ex) '(approx))))))

(defun approx-p (x y)
  (< (abs (- x y)) 0.1))

;(0 "." 3 3 3) 
;(3 "." 1 4 1)
(defun lis2dec (lis)
  (read-from-string (format nil "~a.~a" (first lis) (lis2num (cddr lis)))))

;(1 8 3 6)
(defun lis2num (lis)
  (read-from-string (remove #\space (remove #\( (remove #\) (format nil "~a" lis))))))

(defun constant-check (lis)
  (dolist (ex lis)
(format t "~a~&" ex)
    (cond ((and (equal (second ex) 'approx)
		(equal (nth 3 ex) ".")
		(approx-p 3.14 (lis2dec (nthcdr 2 ex))))
	   (add-hyp (first ex) '(pi)))
	  ((and (equal (second ex) 'approx)
		(equal (nth 3 ex) ".")
		(approx-p 2.17 (lis2dec (nthcdr 2 ex))))
	   (add-hyp (first ex) '(euler)))
;(s1 [ s2 ] / [ s3 [ s4 ] ] approx 1 8 3 6)
	  ((and (equal (lis2num (nthcdr 12 ex)) 1836)
		(equal (nth 4 ex) '/))
	   (add-hyp (first ex) '(mass proton))
	   (add-hyp (third ex) '(mass proton))
	   (add-hyp (nth 6 ex) '(mass electron))
	   (add-hyp (nth 8 ex) '(mass electron)))
;(s5 approx 1 / 1 3 7)
	  ((and (equal (lis2num (nthcdr 4 ex)) 137)
		(equal (nth 2 ex) 1) (equal (nth 3 ex) '/))
	   (add-hyp (first ex) '(alpha)))
;(S26 = [ S27 EXPT 2 ] / [ 4 * S22 * S28 * S29 * S30 ])
;(s1 = [ s12 expt 2 ] / [ 4 * pi * s13 * s14 * s15 ])
	  ((and (equal (nth 4 ex) 'expt) (equal (nth 5 ex) 2)
		(member 4 (nthcdr 8 ex)) (member 'pi (nthcdr 8 ex)))
	   (add-hyp (first ex) '(alpha))
	   (add-hyp (fourth ex) '(e))
	   (add-hyp (nth 13 ex) '(e0 h-bar c)) 
	   (add-hyp (nth 15 ex) '(e0 h-bar c)) 
	   (add-hyp (nth 17 ex) '(e0 h-bar c)))
	  )
(hyp-substitute lis)
))

(defun planck-check (lis)
  (dolist (ex lis)
; (s16 [ s17 ] = sqrt [ [ s14 * s18 ] / [ s15 ^ 3 ] ])
    (cond ((and (equal (nth 4 ex) '=)
		(equal (nth 5 ex) 'root)
		(equal (nth 9 ex) '*)
		(equal (nth 12 ex) '/)
		(or (equal (nth 15 ex) 'expt)
		    (equal (nth 15 ex) '^))
		(equal (nth 16 ex) 3))
	   (add-hyp (nth 0 ex) '(planck length))
	   (add-hyp (nth 2 ex) '(planck length))
	   (add-hyp (nth 8 ex) '(h-bar G))
	   (add-hyp (nth 10 ex) '(h-bar G))
	   (add-hyp (nth 14 ex) '(c))
	   (hyp-substitute code))
;(S31 [ S23 ] = SQRT [ [ S29 * C ] / S33 ]) 
	  ((and (or (find 'planck (second (find (first ex) hyp-lis :key 'car)))
		    (find 'planck (second (find (third ex) hyp-lis :key 'car))))
		(equal (nth 4 ex) '=)
		(equal (nth 5 ex) 'root)
		(equal (nth 9 ex) '*)
		(equal (nth 12 ex) '/))
(format t "MASS~&")
	   (add-hyp (nth 0 ex) '(planck mass))
	   (add-hyp (nth 2 ex) '(planck mass))
	   (add-hyp (nth 8 ex) '(h-bar c))
	   (add-hyp (nth 10 ex) '(h-bar c))
	   (add-hyp (nth 13 ex) '(g))))))


(defun c-pos (ex)
  (setf c-lis nil)
  (dotimes (i (length ex))
    (cond ((member 'c (second (first (member (nth i ex) hyp-lis :key 'first :test 'equal))))
	   (push i c-lis))))
  (reverse c-lis))

;assumes all symbols start with S, otherwise real values put in
; so sqrt is root
(defun add-hyp (sym hyps)
  (cond ((equal (subseq (format nil "~a" sym) 0 1) "S")
  (setf mapy nil)
  (setf inter nil)
  (cond ((and (setf mapy (member sym hyp-lis :key 'first :test 'equal))
	      (setf inter (intersection (second (first mapy)) hyps)))
;	 (setf hyp-lis (substitute inter (first mapy) hyp-lis :test 'equal))
	 (setf hyp-lis (substitute (list (first (first mapy)) inter) (first mapy) hyp-lis :test 'equal))
;	 (format t "hyp: ~a~&" (substitute (list (first (first mapy)) inter) (first mapy) hyp-lis :test 'equal))
	 )
	(t
	 (push (list sym hyps) hyp-lis)
;	 (format t "hyp: ~a~&" (list sym hyps))
	 (setf hyp-lis (reverse hyp-lis)))))))

(setf planck-def '(
(planck [ length ] = sqrt [ [ h-bar * G ] / [ c ^ 3 ] ] )
(planck [ mass ] = sqrt [ [ h-bar * c ] / G ] )
(planck [ time ] = sqrt [ [ h-bar * G ] / [ c ^ 5 ] ] )
(planck [ charge ] = sqrt [ 4 * pi * e0 * h-bar * c ] ] )))

(setf planck-hid '(
(S1 [ S2 ] = SQRT [ [ S3 * S4 ] / [ C ^ 3 ] ]) 
(S1 [ S5 ] = SQRT [ [ S3 * C ] / S4 ]) 
(S1 [ S6 ] = SQRT [ [ S3 * S4 ] / [ C ^ 5 ] ]) 
(S1 [ S7 ] = SQRT [ 4 * S8 * S9 * S3 * C ] ]) ))

#|
(defun hider (lis)
  (setf mapping nil)
  (setf hid-ex nil)
  (setf hid-lis nil)
  (setf hidey nil)
  (setf mapy nil)
  (setf count 0)
  (dolist (ex lis)
    (format t "~a~&" ex) ;
    (setf hid-ex nil)
    (dolist (sym ex)
      (setf mapy (member sym mapping :key 'first :test 'equal))
      (format t "mappy: ~a~&" mapy) ;
      (cond ((or (member sym '([ ] = / * sqrt ^ c))
		 (numberp sym))
	     (push sym hid-ex))
	    (mapy
	     (push (second (first mapy)) hid-ex))
	    (t
	     (incf count)
	     (setf hidey (read-from-string (format nil "s~a" count)))
	     (push hidey hid-ex)
	     (push (list sym hidey) mapping)))
      )
    (setf hid-ex (reverse hid-ex))
    (format t "hide-ex: ~A~&" hid-ex )
    (push hid-ex hid-lis)
    (setf hid-ex nil)
    )
  (setf hid-lis (reverse hid-lis))
)
|#

(defun hider (lis)
  (setf mapping nil)
  (setf hid-ex nil)
  (setf hid-lis nil)
  (setf hidey nil)
  (setf mapy nil)
  (setf count 0)
  (dolist (ex lis)
;    (format t "~a~&" ex) ;
    (setf hid-ex nil)
    (dolist (sym ex)
      (setf mapy (member sym mapping :key 'first :test 'equal))
;      (format t "mappy: ~a~&" mapy) ;
      (cond (mapy
	     (push (second (first mapy)) hid-ex))
	    (t
	     (incf count)
	     (setf hidey (read-from-string (format nil "s~a" count)))
	     (push hidey hid-ex)
	     (push (list sym hidey) mapping)))
      )
    (setf hid-ex (reverse hid-ex))
 ;   (format t "hide-ex: ~A~&" hid-ex );
    (push hid-ex hid-lis)
    (setf hid-ex nil)
    )
  (setf hid-lis (reverse hid-lis))
)

;delete is destructive
(defun hyp-prune ()
  (setf lis hyp-lis)
  (dolist (map lis)
    (cond ((equal 1 (length (second map)))
	   (setf lis2 hyp-lis)
	   (dolist (map2 lis2)
	     (cond ((> (length (second map2)) 1)
		    (member (first (second map)) (second map2) :test 'equal)
		    (add-hyp (first map2) (remove (first (second map)) (second map2) :test 'equal))
)))))))

#|  
(defun fun-check (lis)
  (setf funlis '(+ - * /))
  (dolist (ex lis)
    (cond ((and (equal (length ex) 5)
		(equal (fourth ex) '=))
	   (fun1 (first ex) (third ex) (fifth ex))
    (format t "~a ~a=~a~&" ex (second ex) funlis)))))

(defun fun1 (x y z)
  (dolist (fun '(+ - * /))
    (cond (
	   nil)
	  ((or (and (equal fun '/) (zerop y))
	       (not (equal (eval `(,fun ,x ,y)) z)))
	   (setf funlis (remove fun funlis))))))
|#

(defun doit (n)
  (with-open-file (ifile "d:/Mike/METI/Test_Message_3p1_encoded.txt")
    (setf depth 0)
    (setf items 0)
    (setf fraction nil)
    (setf symbol-list nil)
    (setf mess nil)
    (dotimes (i n)
      (setf inp (read-line ifile nil))
      (dolist (j (read-from-string (format nil "(~a)" inp)))
	(setf mess (push j mess))
	(setf symbol-list (pushnew j symbol-list :test 'equal))))
    (setf mess (reverse mess))
    (setf symbol-list (sort symbol-list #'(lambda (x y) (>= (count x mess) (count y mess)))))
    (mapper)
    (printer)
))

(defun mapper ()
  (setf map nil)
  (setf what nil)
; most frequent symbol is (
  (setf map (push (list (car symbol-list) "(") map))
; next frequent symbol is )
  (setf map (push (list (cadr symbol-list) ")") map))
  (dotimes (i (length mess))
    (setf m (find (nth i mess) map :key 'car))
    (if m (push (cadr m) what)
	(push (nth i mess) what)))
  (setf what (reverse what))
  (dotimes (i (- (length what) 4))
    (format t "~a ~a ~a ~a~&" (nth i what) (nth (+ i 1) what)
	    (nth (+ i 2) what) (nth (+ i 3) what) (nth (+ i 4) what))
    (cond ((and (equal (nth i what) "(")
		(equal (nth (+ i 1) what) (nth (+ i 3) what))
		(equal (nth (+ i 4) what) ")"))
	   (push (list (nth (+ i 2) what) "=") map)
	   (return))))
  (setf map (reverse map))
)

(defun printer ()
  (dolist (i mess)
    (setf m (find i map :key 'car))
    (cond (m
	   (format t "~a " (cadr m)))
	  (t
	   (format t "~a " i)))
    (cond ((string= (cadr m) ")")
	   (format t "~&")))))

; 1 2 3 = 1
; 4 2 3 = 4, 3 is zero, 2 is + or -
;(defun infix-tester (s1 f s2 s3)
(defun it (s1 s2 s3 s4 s5 s6)
  (setf nums '(0 1 2 3))
  (setf lis2 nil)
  (setf lis3 nil)
  (setf lis4 nil)
  (setf lis5 nil)
  (setf lis6 nil)
  (setf funlis nil)

    (dolist (fun '(+ - *))
      (dolist (num1 nums)
	(cond ((equal s2 s1) (setf lis2 (list num1)))
	      (t (setf lis2 (remove num1 nums))))
	(dolist (num2 lis2)
	  (cond ((equal s3 s2) (setf lis3 (list num2)))
		((equal s3 s1) (setf lis3 (list num1)))
		(t (setf lis3 (remove num2 (remove num1 nums)))))
	  (dolist (num3 lis3)
	    (cond ((equal s4 s3) (setf lis4 (list num3)))
		  ((equal s4 s2) (setf lis4 (list num2)))
		  ((equal s4 s1) (setf lis4 (list num1)))
		  (t (setf lis4 (remove num3 (remove num2 (remove num1 nums))))))
	    (dolist (num4 lis4)
	      (cond ((equal s5 s4) (setf lis5 (list num4)))
		    ((equal s5 s3) (setf lis5 (list num3)))
		    ((equal s5 s2) (setf lis5 (list num2)))
		    ((equal s5 s1) (setf lis5 (list num1)))
		    (t (setf lis5 (remove num4 (remove num3 (remove num2 (remove num1 nums)))))))
	      (dolist (num5 lis5)
		(cond ((equal s6 s5) (setf lis6 (list num5)))
		      ((equal s6 s4) (setf lis6 (list num4)))
		      ((equal s6 s3) (setf lis6 (list num3)))
		      ((equal s6 s2) (setf lis6 (list num2)))
		      ((equal s6 s1) (setf lis6 (list num1)))
		      (t (setf lis6 (remove num5 (remove num4 (remove num3 (remove num2 (remove num1 nums))))))))
		(dolist (num6 lis6)
		  (cond ((and (equal (eval `(,fun ,num1 ,num2)) num3)
			      (equal (eval `(,fun ,num4 ,num5)) num6))
			 (pushnew fun funlis :test 'equal)
			 (format t "~a ~a ~a = ~a~&" num1 fun num2 num3)
			 (format t "~a ~a ~a = ~a~&---~&" num4 fun num5 num6)
			 )))))))))
    (format t "~a~&" funlis))

; 1 ? 0 = 0		  
; 1 2 3 = 1
; 4 2 3 = 4
(defun funy (s1 s2 s3 s4 s5 s6)
  (setf funlis nil)
  (dolist (fun '(+ - * /))
    (cond ((and (equal fun '/)
		(or (zerop s2) (zerop s5)))
	   nil)
	  ((and (equal (eval `(,fun ,s1 ,s2)) s3)
		(equal (eval `(,fun ,s4 ,s5)) s6))
	   (push fun funlis))))
  (format t "~a~&" funlis))


#|
(defun doit (n)
  (setf all nil)
  (setf some nil)
  (setf countlis nil)
  (setf map nil)
  (with-open-file (ifile "c:/Users/mpmatess/Downloads/Test_Message_3p1_encoded.txt")
    (setf depth 0)
    (setf items 0)
    (setf fraction nil)
    (dotimes (i n)
      (setf inp (read-line ifile nil))
      (dolist (j (read-from-string (format nil "(~a)" inp)))
	(push j all)
	(pushnew j some :test 'equal)
;	(format t "~a " j)
	)
      (format t "~&"))
    (setf all (reverse all))
    (dolist (j some)
      (push (list j (count j all)) countlis))
    (setf countlis (sort countlis '> :key 'cadr))
    (push (list (first (first countlis)) "(") map)
))
;    (setf countlis (sort countlis '< :key 'cadr))
;      (format t "~a ~a~&" j 
;;;;
	(cond (fraction
	       (setf fraction nil)
	       (format t "~a " (oct-fract j)))
	      ((equal j 00777700)
	       (if (equal depth 1) (incf items))
	       (incf depth)
	       (format t "( "))
	      ((equal j 00666600)
	       (decf depth)
	       (format t ") ")
	       (when (> depth 4)
		 (setf items 0)
		 (format t "~&"))
	       )
	      ((is-num j)
	       (format t "~a " (oct-num j)))
	      ((is-neg-num j)
	       (format t "-~a " (oct-num j)))
	      ((equal j 31033300)
	       (setf fraction t)
	       (format t ". "))
	      ((setf ans (cadr (assoc j map)))
	       (format t "~a " ans))
	      (t
	       (format t "~a " j)))
	(when (zerop depth)
	  (setf items 0)
	  (format t "~&"))
))))
|#

(defun do4 (w x y z)
  (read-from-string (format nil "#b~3,'0B~3,'0B~3,'0B~3,'0B" w x y z)))

(defun doit5 (v w x y z)
  (read-from-string (format nil "#b~3,'0B~3,'0B~3,'0B~3,'0B~3,'0B" v w x y z)))

(defun frac (str)
  (let ((ans 0.0))
    (dotimes (i 8)
      (setf ans (+ ans 
		   (/ (read-from-string (subseq str i (+ i 1)))
		      (expt 8 (1+ i))))))
    ans))

(defun front (str)
  (let ((ans 0.0))
    (setf str (reverse str))
    (dotimes (i 8)
      (setf ans (+ ans 
		   (* (read-from-string (subseq str i (+ i 1)))
		      (expt 8 (1+ i))))))
    ans))

(defun is-num (x)
  (equal (subseq (format nil "~a" x) 0 3) "100"))

(defun is-neg-num (x)
  (equal (subseq (format nil "~a" x) 0 3) "200"))

(defun oct-num (x)
  (setf str (format nil "~a" x))
  (setf str (reverse (subseq str 1 (length str))))
  (setf ans 0)
  (dotimes (i (length str))
    (incf ans (* (expt 8 i) (read-from-string (subseq str i (1+ i))))))
  (format nil "~a" ans))

(defun oct-fract (x)
  (setf str (format nil "~a" x))
  (setf ans 0)
  (dotimes (i (length str))
    (incf ans (* (expt 8 (- (1+ i))) (read-from-string (subseq str i (1+ i))))))
  (format nil "~1,5f" (float ans)))


(setf message 
" 1 = 1 
 2 = 2 
 3 = 3 
 1 = [ o ] 
 2 = [ o o ] 
 3 = [ o o o ] 
 4 = [ o o o o ] 
 5 = [ o o o o o ] 
 6 = [ o o o o o o ] 
 7 = [ o o o o o o o ] 
 8 = [ o o o o o o o o ] 
 9 = [ o o o o o o o o o ] 
 0 = [ ] 
 4 + 5 = 9 
 9 - 4 = 5 
 2 * 3 = 6 
 6 / 2 = 3 
 2 ^ 3 = 8 
 [ 1 + 3 ] = 4 
 2 * [ 1 + 3 ] = 8 
 9 / [ 1 + 2 ] = 3 
 sqrt [ 4 ] = 2 
 sqrt [ 9 ] = 3 
 8 + 1 = 9 
 9 + 1 = 1 0 
 9 + 2 = 1 1 
 1 0 * 1 0 = 1 0 0 
 1 . 0 = 1 
 0 . 1 = 1 / 1 0 
 0 . 0 1 = 1 / 1 0 0 
 3 . 1 = 3 + 1 / 1 0 
 1 / 3 approx 0 . 3 3 3 
 pi approx 3 . 1 4 1 5 9 
 mass [ proton ] / mass [ electron ] approx 1 8 3 6 
 alpha approx 1 / 1 3 7 
 alpha = [ e ^ 2 ] / [ 4 * pi * e0 * h-bar * c ] 
 planck [ length ] = sqrt [ [ h-bar * G ] / [ c ^ 3 ] ] 
 planck [ mass ] = sqrt [ [ h-bar * c ] / G ] 
")

(defun reset-code ()
(setf code (copy-list '(
(S01 S10 S01) 
(S02 S10 S02) 
(S03 S10 S03) 
(S01 S10 S11 S12 S13) 
(S02 S10 S11 S12 S12 S13) 
(S03 S10 S11 S12 S12 S12 S13) 
(S04 S10 S11 S12 S12 S12 S12 S13) 
(S05 S10 S11 S12 S12 S12 S12 S12 S13) 
(S06 S10 S11 S12 S12 S12 S12 S12 S12 S13) 
(S07 S10 S11 S12 S12 S12 S12 S12 S12 S12 S13) 
(S08 S10 S11 S12 S12 S12 S12 S12 S12 S12 S12 S13) 
(S09 S10 S11 S12 S12 S12 S12 S12 S12 S12 S12 S12 S13) 
(S00 S10 S11 S13) 
(S04 S14 S05 S10 S09) 
(S09 S15 S04 S10 S05) 
(S02 S16 S03 S10 S06) 
(S06 S17 S02 S10 S03) 
(S02 S18 S03 S10 S08) 
(S11 S01 S14 S03 S13 S10 S04) 
(S02 S16 S11 S01 S14 S03 S13 S10 S08) 
(S09 S17 S11 S01 S14 S02 S13 S10 S03) 
(S19 S11 S04 S13 S10 S02) 
(S19 S11 S09 S13 S10 S03) 
(S08 S14 S01 S10 S09) 
(S09 S14 S01 S10 S01 S00) 
(S09 S14 S02 S10 S01 S01) 
(S01 S00 S16 S01 S00 S10 S01 S00 S00) 
(S00 S20 S01 S10 S01 S17 S01 S00) 
(S00 S20 S00 S01 S10 S01 S17 S01 S00 S00) 
(S03 S20 S01 S10 S03 S14 S01 S17 S01 S00) 
(S01 S17 S03 S21 S00 S20 S03 S03 S03) 
(S22 S21 S03 S20 S01 S04 S01 S05 S09) 
(S23 S11 S24 S13 S17 S11 S23 S11 S25 S13 S13 S21 S01 S08 S03 S06) 
(S26 S21 S01 S17 S01 S03 S07) 
(S26 S10 S11 S27 S18 S02 S13 S17 S11 S04 S16 S22 S16 S28 S16 S29 S16 S30 S13) 
(S31 S11 S32 S13 S10 S19 S11 S11 S29 S16 S33 S13 S17 S11 S30 S18 S03 S13 S13) 
(S31 S11 S23 S13 S10 S19 S11 S11 S29 S16 S30 S13 S17 S33 S13) 
)))
)

(reset-code)
