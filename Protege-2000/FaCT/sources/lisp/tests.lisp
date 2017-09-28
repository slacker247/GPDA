(init-tkb)
(defprimrole r)
(defprimconcept p1 (not (or p2 p3 p4 p5)))
(defprimconcept p2 (not (or p3 p4 p5)))
(defprimconcept p3 (not (or p4 p5)))
(defprimconcept p4 (not p5))
(defprimconcept p5)
(classify-tkb)

(satisfiable '(and (some r p1) (some r p2) (some r p3) (at-most 2 r)))
;;; nil
(satisfiable '(some (inv r) (and (some r p1) (at-most 1 r p1))))
;;; T
(satisfiable '(and p2 (some (inv r) (and (some r p1) (at-most 1 r)))))
;;; nil

(init-tkb)
(defprimrole f1)
(defprimrole f2)
(defprimrole r :supers (f1 f2))
(implies :top (at-most 1 f1))
(implies :top (at-most 1 f2))
(defprimconcept p1 (not p2))
(defprimconcept p2)
(classify-tkb)

(satisfiable '(and (some f1 p1) (some f2 p2)))
;;; T
(satisfiable '(and (some f1 p1) (some f2 p2) (some r :top)))
;;; nil


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
(time (satisfiable '(and (some r p1) (some r p2) (some r p3)
		     (some r (and p1 p)) (some r (and p2 p)) (some r (and p3 p))
		     (at-most 3 r))))
;;; there are 301 possible partitions in the following unsatisfiable case
(time (satisfiable '(and (some r p1) (some r p2) (some r p3) (some r p4)
		     (some r (and p1 p)) (some r (and p2 p)) (some r (and p3 p))
		     (at-most 3 r))))
;;; there are 1,701 possible partitions in the following satisfiable case
(time (satisfiable '(and (some r p1) (some r p2) (some r p3) (some r p4)
		     (some r (and p1 p)) (some r (and p2 p)) (some r (and p3 p))
		     (some r (and p4 p))
		     (at-most 4 r))))
;;; there are 7,770 possible partitions in the following unsatisfiable case
(time (satisfiable '(and (some r p1) (some r p2) (some r p3) (some r p4) (some r p5)
		     (some r (and p1 p)) (some r (and p2 p)) (some r (and p3 p))
		     (some r (and p4 p))
		     (at-most 4 r))))
;;; there are 42,525 possible partitions in the following satisfiable case
(time (satisfiable '(and (some r p1) (some r p2) (some r p3) (some r p4) (some r p5)
		     (some r (and p1 p)) (some r (and p2 p)) (some r (and p3 p))
		     (some r (and p4 p)) (some r (and p5 p))
		     (at-most 5 r))))

;;; Dynamic blocking example from paper
(init-tkb)
(defprimrole r)
(defprimrole s)
(defprimrole p :transitive t)
(defprimconcept a)
(defconcept c (all (inv r) (all (inv p) (all (inv s) (not a)))))
(classify-tkb)
(classify-concept '(and a (some s (and (some r *top*) (some p *top*) (all r c) (all p (some r *top*)) (all p (some p *top*)) (all p (all r c))))))
					; should be incoherent

;;; Non-finite model example from paper
(init-tkb)
(defprimrole r :transitive t)
(defprimrole f :supers (r))
(implies :top (at-most 1 f))
(defprimconcept a)
(classify-tkb)
(classify-concept '(and (not a) (some (inv f) a) (all (inv r) (some (inv f) a))))
					; should be coherent but has no finite model

;;; Double blocking example from paper
(init-tkb)
(defprimrole r :transitive t)
(defprimrole f :supers (r))
(implies :top (at-most 1 f))
(defprimconcept c)
(defconcept d (and c (some f (not c))))
(classify-tkb)
(classify-concept '(and (not c) (some (inv f) d) (all (inv r) (some (inv f) d))))
					; should be incoherent but needs double blocking

;;; Other tricky examples
(init-tkb)
(defprimrole f)
(implies :top (at-most 1 f))
(defprimrole r :transitive t)
(defprimconcept p1)
(classify-tkb)
(classify-concept '(and p1 (some r (some r (and p1 (all (inv r) (not p1)))))))
					; incoherent
(classify-concept '(and p1 (some r (some r (and p1 (all (inv r) (or (not p1) (all r p1))))))))
					; coherent
(classify-concept '(some f (and p1 (all (inv f) (some f (not p1))))))
					; incoherent

(init-tkb)
(defprimrole r1)
(defprimrole r)
(defprimconcept p)
(classify-tkb)
(satisfiable '(and (some r (all (inv r) (all r1 p)))
	       (some r (all (inv r) (all r1 (not p))))))
;;; T
(satisfiable '(and (some r1 :top) (some r (all (inv r) (all r1 p)))
	       (some r (all (inv r) (all r1 (not p))))))
;;; nil