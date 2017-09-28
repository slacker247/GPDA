(defun t1 ()
  (init-tkb)
  (defprimrole r)
  (defprimconcept p1 (not (or p2 p3 p4 p5)))
  (defprimconcept p2 (not (or p3 p4 p5)))
  (defprimconcept p3 (not (or p4 p5)))
  (defprimconcept p4 (not p5))
  (defprimconcept p5)
  (classify-tkb)

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
  (classify-tkb)

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
  (classify-tkb)
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
  (classify-tkb)
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
  (classify-tkb)
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
  (classify-tkb)
;;; following concept should be coherent but has no finite model
  (satisfiable '(and (not a) (some (inv f) a) (all (inv r) (some (inv f) a)))))

(defun t5f ()
;;; Non-finite model example from paper
  (init-tkb)
  (defprimrole r :transitive t)
  (defprimattribute f :supers (r))
  (defprimconcept a)
  (classify-tkb)
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
  (classify-tkb)
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
  (classify-tkb)
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
  (classify-tkb)
  (not (satisfiable '(and p1 (some r (some r (and p1 (all (inv r) (not p1))))))))
  (satisfiable '(and p1 
		 (some r (some r (and p1 (all (inv r) (or (not p1) (all r p1))))))))
  (not (satisfiable '(some f (and p1 (all (inv f) (some f (not p1))))))))

(defun t7f ()
  (init-tkb)
  (defprimattribute f)
  (defprimrole r :transitive t)
  (defprimconcept p1)
  (classify-tkb)
  (not (satisfiable '(and p1 (some r (some r (and p1 (all (inv r) (not p1))))))))
  (satisfiable '(and p1 
		 (some r (some r (and p1 (all (inv r) (or (not p1) (all r p1))))))))
  (not (satisfiable '(some f (and p1 (all (inv f) (some f (not p1))))))))

(defun t8 ()
  (init-tkb)
  (defprimrole r1)
  (defprimrole r)
  (defprimconcept p)
  (classify-tkb)
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
  (classify-tkb)

  (and (satisfiable 'Infinite-Tree-Root)
       (not (satisfiable '(and Infinite-Tree-Root 
			   (all descendant (some (inv successor) root)))))))

(defun t10 ()
  (init-tkb)
  (defprimattribute f)
  (defprimattribute s :supers (f))
  (defprimconcept p)
  (classify-tkb)
  
  (and 
   (not (satisfiable '(and (not p) 
		      (some f (and (all (inv s) p) 
			       (all (inv f) (some s p)))))))
   (not (satisfiable '(and (all s (not p)) (some r (and p (some (inv s) p))))))
  ))

(defun t11 ()
  (init-tkb)
  (defprimrole r)
  (defprimrole s :supers (r))
  (defprimconcept p)
  (classify-tkb)
  
  (not (satisfiable '(and (not p)
		      (atmost 1 r) (some r (all (inv s) p)) (some s p)))))

(defun t12 ()
  (init-tkb)
  (defprimrole r)
  (defprimrole s)
  (defprimconcept p)
  (defprimconcept q)
  (classify-tkb)
  
  (not (satisfiable '(and 
		      (some s (and (not p) (not q)))
		      (some r (and 
			       (atmost 1 (inv r)) 
			       (some (inv r) (all s p))))))))

(defun run-tests (l)
  (let ((passed t))
    (dolist (fn l)
      (if (funcall fn)
	  (format T "~&Test ~A OK~%" fn)
	(setf passed (format T "~&WARNING - Test ~A failed!~%" fn))))
    (if passed
	(format T "~&~%PASSED ALL TESTS~2%")
      (format T "~&~%!!PROBLEMS!!~%")))
  (values))

(defun test (&optional l)
  (if l
      (if (listp l)
	  (run-tests l)
	(run-tests (list l)))
    (run-tests '(t1 t2 t3 t4 t5 t5f t6 t6f t7 t7f t8 t9 t10 t11))))