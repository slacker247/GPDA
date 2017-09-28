;;; Tbox tests from Heinsohn et al., AI 68 (1994) pp367-397
(defun Heinsohn-tbox-1 ()
  "Tests incoherency caused by disjoint concepts"
  (init-tkb)
  (defprimconcept c)
  (defprimconcept d)
  (defprimrole r)
  (disjoint c d)
  (defprimconcept e3 c)
  (defprimconcept f d)
  (defprimconcept d1)
  (defprimconcept c1 d1)
  (disjoint c1 d1)
  ;(classify-tkb)

  (and
;;; (a)
   (not (satisfiable '(and c d)))
;;; (b)
   (not (satisfiable '(and (all r (and c d)) (some r *TOP*))))
;;; (c)
   (not (satisfiable '(and e3 f)))
;;; (d)
   (not (satisfiable 'c1))))

(defun Heinsohn-tbox-2 ()
  "Tests incoherency caused by number restrictions"
  (init-tkb)
  (defprimconcept c)
  (defprimconcept d)
  (defprimrole r)
  (disjoint c d)
  ;(classify-tkb)

  (and
;;; (a)
   (not (satisfiable '(and (at-least 2 r) (at-most 1 r))))
;;; (b)
   (not (satisfiable '(and (at-most 1 r) (some r c) (some r d))))))

(defun Heinsohn-tbox-3c ()
  "Tests incoherency caused by number restrictions and role hierarchy"
;;; test (c) - don't think we can represent it and not even sure it is correct!
;;; this is my best shot
  (init-tkb)
  (defprimconcept c)
  (defprimconcept d)
  (disjoint c d)
  (defprimconcept a (or c d))
  (defprimrole t1)
  (defprimrole tc :supers (t1))
  (defprimrole td :supers (t1))
  (implies :top (all tc c))
  (implies :top (all td d))
  (defprimrole r :supers (tc))
  (defprimrole s :supers (td))
  ;(classify-tkb)
  (not (satisfiable '(and (all t1 a) (at-least 3 t1) (at-most 1 r) (at-most 1 s)))))

(defun Heinsohn-tbox-3c-irh ()
  "Tests incoherency caused by number restrictions and role hierarchy"
;;; a modified version of test (c)
  (init-tkb)
  (defprimconcept c)
  (defprimconcept d)
  (disjoint c d)
  (defprimconcept a (or c d))
  (defprimrole tt)
  ;(classify-tkb)
  (not (satisfiable '(and (all tt a) (at-least 3 tt) 
		      (at-most 1 tt c) (at-most 1 tt d)))))
		      
(defun Heinsohn-tbox-3 ()
  "Tests incoherency caused by number restrictions and role hierarchy"
  (init-tkb)
  (defprimconcept c)
  (defprimconcept d)
  (defprimconcept e)
  (defprimrole r)
  (disjoint c d e)
  (defprimconcept a (or c d))
  (defprimrole r1 :supers (r))
  (defprimrole r2 :supers (r))
  (defprimrole r3 :supers (r))
  (defprimrole tt)
  (defprimrole t1 :supers (tt))
  (defprimrole t2 :supers (tt))
  (defprimrole t3 :supers (tt))
  ;(classify-tkb)
  
  (and
;;; (a)
   (subsumes '(at-least 2 r)
	     '(and (at-least 1 r) (some r c) (some r d)))
;;; (b)
   (subsumes '(and (at-most 1 r c) (at-most 1 r d))
	     '(and (at-most 2 r) (some r c) (some r d)))
;;; (d)
   (subsumes '(at-least 2 r d)
	     '(and (all r a) (at-least 3 r) (at-most 1 r c)))
;;; (e)
;;; this is Heinsohn's form, which doesn't seem right to me
;;;   (subsumes '(at-least 2 r)
;;;	     '(and (some r1 (and (at-most 2 tt) (some t1 c)))
;;;	       (some r2 (and (at-most 2 tt) (some t2 d)))
;;;	       (some r3 (and (at-most 2 tt) (some t3 3)))))
;;; this is my version with the atmost 2 changed to atmost 1
   (subsumes '(at-least 2 r)
	     '(and (some r1 (and (at-most 1 tt) (some t1 c)))
	       (some r2 (and (at-most 1 tt) (some t2 d)))
	       (some r3 (and (at-most 1 tt) (some t3 3)))))
;;; (c)
;;;   (HEINSOHN-TBOX-3C)
   (HEINSOHN-TBOX-3C-irh)))
		      
(defun Heinsohn-tbox-4 ()
  "Tests role restrictions"
  (init-tkb)
  (defprimconcept c)
  (defprimconcept d)
  (defprimconcept e)
  (defprimrole r)
  (defprimrole s)
  (disjoint c d)
  ;(classify-tkb)

  (and
;;; (a)
   (subsumes '(all r e) '(and (all r d) (all r (or (not d) e))))
;;; (b)
   (subsumes '(all r (at-most 1 s))
	     '(and (all r (or (not (at-least 2 s)) c)) (all r d)))))
  
(defun Heinsohn-tbox-7 ()
  "Tests inverse roles"
  (init-tkb)
  (defprimconcept a)
  (defprimrole r)
  ;(classify-tkb)

  (and
;;; (a)
   (subsumes 'a '(and (all r (all (inv r) a)) (some r :top)))))
  
;;; Other tests
(defun t1 ()
  (init-tkb)
  (defprimrole r)
  (defprimconcept p1 (not (or p2 p3 p4 p5)))
  (defprimconcept p2 (not (or p3 p4 p5)))
  (defprimconcept p3 (not (or p4 p5)))
  (defprimconcept p4 (not p5))
  (defprimconcept p5)
  ;(classify-tkb)

  (and
   (not (satisfiable '(and (some r p1) (some r p2) (some r p3) (at-most 2 r))))
   (satisfiable '(some (inv r) (and (some r p1) (at-most 1 r p1))))
   (not (satisfiable '(and p2 (some (inv r) (and (some r p1) (at-most 1 r))))))))

(defun t2 ()
  (init-tkb)
  (defprimrole f1)
  (defprimrole f2)
  (defprimrole r :supers (f1 f2))
  (implies :top (at-most 1 f1))
  (implies :top (at-most 1 f2))
  (defprimconcept p1 (not p2))
  (defprimconcept p2)
  ;(classify-tkb)

  (and (satisfiable '(and (some f1 p1) (some f2 p2)))
       (not (satisfiable '(and (some f1 p1) (some f2 p2) (some r :top))))))

(defun t3 ()
  (init-tkb)
  (defprimrole r)
  (defprimconcept p)
  (defprimconcept p1 (not (or p2 p3 p4 p5)))
  (defprimconcept p2 (not (or p3 p4 p5)))
  (defprimconcept p3 (not (or p4 p5)))
  (defprimconcept p4 (not p5))
  (defprimconcept p5)
  ;(classify-tkb)
;;; there are 90 possible partitions in the following satisfiable case
  (and
   (satisfiable '(and (some r p1) (some r p2) (some r p3)
		  (some r (and p1 p)) (some r (and p2 p)) (some r (and p3 p))
		  (at-most 3 r)))
;;; there are 301 possible partitions in the following unsatisfiable case
   (not (satisfiable '(and (some r p1) (some r p2) (some r p3) (some r p4)
		       (some r (and p1 p)) (some r (and p2 p)) (some r (and p3 p))
		       (at-most 3 r))))))


(defun t3a ()
  (init-tkb)
  (defprimrole r)
  (defprimconcept p)
  (defprimconcept p1 (not (or p2 p3 p4 p5)))
  (defprimconcept p2 (not (or p3 p4 p5)))
  (defprimconcept p3 (not (or p4 p5)))
  (defprimconcept p4 (not p5))
  (defprimconcept p5)
  ;(classify-tkb)
;;; there are 1,701 possible partitions in the following satisfiable case
  (and
   (time (satisfiable '(and (some r p1) (some r p2) (some r p3) (some r p4)
			(some r (and p1 p)) (some r (and p2 p)) (some r (and p3 p))
			(some r (and p4 p))
			(at-most 4 r))))
;;; there are 7,770 possible partitions in the following unsatisfiable case
   (not
    (time (satisfiable '(and (some r p1) (some r p2) (some r p3) (some r p4) (some r p5)
			 (some r (and p1 p)) (some r (and p2 p)) (some r (and p3 p))
			 (some r (and p4 p))
			 (at-most 4 r)))))
;;; there are 42,525 possible partitions in the following satisfiable case
    (time (satisfiable '(and (some r p1) (some r p2) (some r p3) (some r p4) (some r p5)
			 (some r (and p1 p)) (some r (and p2 p)) (some r (and p3 p))
			 (some r (and p4 p)) (some r (and p5 p))
			 (at-most 5 r))))))

(defun t4 ()
;;; Dynamic blocking example from paper
  (init-tkb)
  (defprimrole r)
  (defprimrole s)
  (defprimrole p :transitive t)
  (defprimconcept a)
  (defconcept c (all (inv r) (all (inv p) (all (inv s) (not a)))))
  ;(classify-tkb)
  (not
   (satisfiable 
    '(and a (some s (and 
		     (some r *top*) 
		     (some p *top*) 
		     (all r c) 
		     (all p (some r *top*)) 
		     (all p (some p *top*)) 
		     (all p (all r c))))))))

(defun t5 ()
;;; Non-finite model example from paper
  (init-tkb)
  (defprimrole r :transitive t)
  (defprimrole f :supers (r))
  (implies :top (at-most 1 f))
  (defprimconcept a)
  ;(classify-tkb)
;;; following concept should be coherent but has no finite model
  (satisfiable '(and (not a) (some (inv f) a) (all (inv r) (some (inv f) a)))))

(defun t5f ()
;;; Non-finite model example from paper
  (init-tkb)
  (defprimrole r :transitive t)
  (defprimattribute f :supers (r))
  (defprimconcept a)
  ;(classify-tkb)
;;; following concept should be coherent but has no finite model
  (satisfiable '(and (not a) (some (inv f) a) (all (inv r) (some (inv f) a)))))

(defun t6 ()
;;; Double blocking example from paper
  (init-tkb)
  (defprimrole r :transitive t)
  (defprimrole f :supers (r))
  (implies :top (at-most 1 f))
  (defprimconcept c)
  (defconcept d (and c (some f (not c))))
  ;(classify-tkb)
;;;; should be incoherent but needs double blocking
  (not
   (satisfiable '(and (not c) (some (inv f) d) (all (inv r) (some (inv f) d))))))

(defun t6f ()
;;; Double blocking example from paper
  (init-tkb)
  (defprimrole r :transitive t)
  (defprimattribute f :supers (r))
  (defprimconcept c)
  (defconcept d (and c (some f (not c))))
  ;(classify-tkb)
;;;; should be incoherent but needs double blocking
  (not
   (satisfiable '(and (not c) (some (inv f) d) (all (inv r) (some (inv f) d))))))

;;; Other tricky examples

(defun t7 ()
  (init-tkb)
  (defprimrole f)
  (implies :top (at-most 1 f))
  (defprimrole r :transitive t)
  (defprimconcept p1)
  ;(classify-tkb)
  (not (satisfiable '(and p1 (some r (some r (and p1 (all (inv r) (not p1))))))))
  (satisfiable '(and p1 
		 (some r (some r (and p1 (all (inv r) (or (not p1) (all r p1))))))))
  (not (satisfiable '(some f (and p1 (all (inv f) (some f (not p1))))))))

(defun t7f ()
  (init-tkb)
  (defprimattribute f)
  (defprimrole r :transitive t)
  (defprimconcept p1)
  ;(classify-tkb)
  (not (satisfiable '(and p1 (some r (some r (and p1 (all (inv r) (not p1))))))))
  (satisfiable '(and p1 
		 (some r (some r (and p1 (all (inv r) (or (not p1) (all r p1))))))))
  (not (satisfiable '(some f (and p1 (all (inv f) (some f (not p1))))))))

(defun t8 ()
  (init-tkb)
  (defprimrole r1)
  (defprimrole r)
  (defprimconcept p)
  ;(classify-tkb)
  (satisfiable '(and (some r (all (inv r) (all r1 p)))
		 (some r (all (inv r) (all r1 (not p))))))
  (not (satisfiable '(and (some r1 :top) (some r (all (inv r) (all r1 p)))
		      (some r (all (inv r) (all r1 (not p))))))))

(defun t9 ()
;;; Another infinite model example
  (init-tkb)
  (defprimrole descendant :transitive T)
  (defprimrole successor :supers (descendant) :i-functional T)
  (defprimconcept node)
  (defprimconcept root (not (some (inv successor) *top*)))
  (defprimconcept Infinite-Tree-Node (and node (some successor Infinite-Tree-Node)))
  (defprimconcept Infinite-Tree-Root (and Infinite-Tree-Node root))
  ;(classify-tkb)

  (and (satisfiable 'Infinite-Tree-Root)
       (not (satisfiable '(and Infinite-Tree-Root 
			   (all descendant (some (inv successor) root)))))))

(defun t10 ()
  (init-tkb)
  (defprimattribute f)
  (defprimattribute f1)
  (defprimattribute s :supers (f f1))
  (defprimconcept p)
  ;(classify-tkb)
  
  (and 
   (not (satisfiable '(and (not p) 
		      (some f (and (all (inv s) p) 
			       (all (inv f) (some s p)))))))
   (not (satisfiable '(and (all s (not p)) (some s (and p (some (inv s) p))))))
   (satisfiable '(and (some f p) (some f1 (not p))))
   (not (satisfiable '(and (some f p) (some s :top) (some f1 (not p)))))
   (not (satisfiable '(and (some f p) (some f1 (and (not p) (all (inv f1) (some s :top)))))))
  ))

(defun t11 ()
  (init-tkb)
  (defprimrole r)
  (defprimrole s :supers (r))
  (defprimconcept p)
  ;(classify-tkb)
  
  (not (satisfiable '(and (not p)
		      (atmost 1 r) (some r (all (inv s) p)) (some s p)))))

(defun t12 ()
  (init-tkb)
  (defprimrole r)
  (defprimrole s)
  (defprimconcept p)
  (defprimconcept q)
  ;(classify-tkb)
  
  (not (satisfiable '(and 
		      (some s (and (not p) (not q)))
		      (some r (and 
			       (atmost 1 (inv r)) 
			       (some (inv r) (all s p))))))))

(defun t13 ()
  (init-tkb)
  (defprimrole r)
  (defprimrole s)
  (defprimconcept c)
  (defprimconcept d)
  (defconcept a1 (some s (all (inv s) (all r c))))
  (defconcept a2 (some s (all (inv s) (all r (not c)))))
  (defconcept a3a (some s (all (inv s) (or (some r d) (some s d)))))
  (defconcept a3b (or (some r d) (some s d)))
  (defconcept a3c (or (some r d) d))
  (defconcept a3e (some r d))
  (and (satisfiable '(and a3a a2 a1))
       (satisfiable '(and a3b a2 a1))
       (satisfiable '(and a3c a2 a1))
       (not (satisfiable '(and a3e a2 a1)))
       (satisfiable '(and d a2 a1))
       (not (satisfiable '(and (and a3a a2 a1) (not (and a3b a2 a1)))))
       (not (satisfiable '(and (not (and a3a a2 a1)) (and a3b a2 a1))))
       (not (satisfiable '(and (and a3c a2 a1) (not (and a3c a2 a1)))))
       (not (satisfiable '(and (not (and a3c a2 a1)) (and a3c a2 a1))))))

(defun fact1 ()
  (init-tkb)
  (defprimconcept a)
  (defprimconcept b)
  (defprimconcept c)
  (disjoint a b c)
  ;(classify-tkb)
  
  (not (satisfiable '(or (and a b) (and a c) (and b c)))))

(defun fact2 ()
  (init-tkb)
  (defprimrole r)
  (defprimconcept c)
  (defprimconcept d)
  (implies c (all r c))
  (implies (all r c) d)
  ;(classify-tkb)
  
  (subsumes 'd 'c))

(defun fact3 ()
  (init-tkb)
  (defprimattribute f1)
  (defprimattribute f2)
  (defprimattribute f3 :parents (f1 f2))
  (defprimconcept p1)
  (defprimconcept p2)
  ;(classify-tkb)

  (not (satisfiable '(and (some f1 p1) (some f2 (not p1)) (some f3 p2)))))

(defun fact4 ()
  (init-tkb)
  (defprimattribute rx)
  (defprimrole rx1)
  (defprimrole rx2)
  (defprimattribute rx3 :parents (rx rx1))
  (defprimattribute rx4 :parents (rx rx2))
  (defprimrole rxa)
  (defprimrole rx1a)
  (defprimrole rx2a)
  (defprimattribute rx3a :parents (rxa rx1a))
  (defprimattribute rx4a :parents (rxa rx2a))
  ;(classify-tkb)
  
  (and
   (subsumes '(some rx3 (and c1 c2))
	     '(and (some rx3 c1) (some rx4 c2)))
   (not (subsumes '(some rx3a (and c1 c2))
		  '(and (some rx3a c1) (some rx4a c2))))))
  

(defconstant *fact-tests* '(fact1 fact2 fact3 fact4 bug-test1a bug-test1b bug-test2))
(defconstant *shiq-tests* '(t1 t2 t3 t4 t5 t5f t6 t6f t7 t7f t8 t9 t10 t11 t12 t13))
(defconstant *heinsohn-tests*'(Heinsohn-tbox-1 Heinsohn-tbox-2 Heinsohn-tbox-3
			       Heinsohn-tbox-4 Heinsohn-tbox-7))
(defconstant *all-tests* (append *heinsohn-tests* *shiq-tests* *fact-tests*))

(defun run-tests (&optional l)
  (if l
      (if (not (listp l)) (setf l (list l)))
    (setf l *all-tests*))
  (let ((passed t))
    (dolist (fn l)
      (if (funcall fn)
	  (format T "~&Test ~A OK~%" fn)
	(setf passed (format T "~&WARNING - Test ~A failed!~%" fn))))
    (if passed
	(format T "~&~%PASSED ALL TESTS~2%")
      (format T "~&~%!!PROBLEMS!!~%")))
  (values))

(defun run-shiq-tests ()
  (run-tests *shiq-tests*))

(defun run-heinsohn-tests (&optional l)
  (run-tests *heinsohn-tests*))

(defun run-fact-tests ()
  (run-tests *fact-tests*))

(defun run-all-tests ()
  (run-tests *all-tests*))

(defun bug-test1a ()
;;; Tests a bug caused by adding new non-absorbable axioms
  (init-tkb)
  (defprimrole r)
  (defprimconcept a)
  (defprimconcept b)
  (implies (some r a) (some r b))
  (classify-tkb)
;;; *universal-constraint* is now non-nil
  (defprimconcept p)
  (defprimconcept q)
  (defconcept w (and p q))
  (defprimconcept z (and p (some r p)))
  (implies (some r p) q)
  (classify-tkb)
;;; direct-supers of z should be (w), but bug causes w=bottom
  (and (eql (length (direct-supers 'z)) 1) 
       (eq (name (car (direct-supers 'z))) 'w)))

(defun bug-test1b ()
;;; Illustrates bug caused by setting definitions of incoherent concepts to
;;; bottom and their negations to top - this is NOT CORRECT!
  (init-tkb)
  (defprimconcept b)
  (defprimconcept d)
  (defconcept c (and a (not b)))
  (defprimconcept a (and d (not c)))
  (satisfiable '(and (not c) a (not b) d))
;;; answer is NIL because \neg c = (or (not a) b)
  (classify-tkb)
;;; c is incoherent so definition of \neg is set to top
;;; answer now becomes T
  (not (satisfiable '(and (not c) a (not b) d))))

(defun bug-test2 ()
;;; bug that Gary discovered - it was caused by synonyms: in extend-prop, a
;;; literal with a synonym wasn't added and information was thus lost.
;;; Gary:-
;;; in the first model:
;;;    because D and E are asserted equivalent, 
;;;    B and C are deduced to be equivalent. 
;;;    This is the result I expected.
  (and
   (progn
     (init-tkb)
     (implies c (and (not e) a))
     (implies e a)
     (implies a (or c e))
     (implies b (and (not d) a))
     (implies d a)
     (implies a (or b d))
     (implies b c)
     (implies c b)
     (classify-tkb)
     (equivalent-concepts 'd 'e))
;;; Gary:-
;;; so in the second model I was expecting similar results in that:
;;;    because B and C are asserted equivalent, 
;;;    D and E are deduced to be equivalent. 
;;;
;;;However, in the second model, FaCT only deduces that E subsumes D.
   (progn
     (init-tkb)
     (implies c (and (not e) a))
     (implies e a)
     (implies a (or c e))
     (implies b (and (not d) a))
     (implies d a)
     (implies a (or b d))
     (implies d e)
     (implies e d)
;;; FaCT always found the right answer if we didn't classify-tkb
     (equivalent-concepts 'b 'c)
     (classify-tkb)
     (equivalent-concepts 'b 'c))))

(defun bug-test3 ()
;;; From Volker Haarslev
  (init-tkb)
  (satisfiable 
   '(and (some r a)
     (at-least 3 r c)
     (at-least 3 r d)
     (at-least 2 r (and e (not (and c d))))
     (at-most 4 r)
     (at-most 2 r (and c d)))))