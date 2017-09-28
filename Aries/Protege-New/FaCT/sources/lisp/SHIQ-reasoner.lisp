;;; -*- Mode: Lisp; package: FACT; Syntax: COMMON-LISP; Base: 10 -*-

;;; FaCT COPYRIGHT (C) 1997, 1998, 1999 IAN R. HORROCKS
;;; and THE UNIVERSITY OF MANCHESTER, horrocks@cs.man.ac.uk
;;; Time-stamp: Tue Jun 15 17:41:50 BST 1999

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                      ;;;
;;; FaCT description logic classifier                                    ;;;
;;; COPYRIGHT (C) 1997, 1998, 1999 IAN R. HORROCKS                       ;;;
;;; and THE UNIVERSITY OF MANCHESTER                                     ;;; 
;;;					                                 ;;;
;;; This program is free software; you can redistribute it and/or        ;;;
;;; modify it under the terms of the GNU General Public License          ;;;
;;; as published by the Free Software Foundation; either version 2       ;;;
;;; of the License, or (at your option) any later version.               ;;;
;;;                                                                      ;;;
;;; This program is distributed in the hope that it will be useful,      ;;;
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of       ;;;
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the        ;;;
;;; GNU General Public License for more details.                         ;;;
;;;                                                                      ;;;
;;; You should have received a copy of the GNU General Public License    ;;;
;;; along with this program; if not, write to the Free Software          ;;;
;;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA            ;;;
;;; 02111-1307, USA.                                                     ;;;
;;;                                                                      ;;;
;;; Enquiries about FaCT should be directed to:                          ;;;
;;;                                                                      ;;;
;;; email: horrocks@cs.man.ac.uk                                         ;;;
;;; www:   http://www.cs.man.ac.uk/~horrocks                             ;;;
;;; smail: Ian Horrocks, Department of Computer Science, Oxford Road,    ;;;
;;;       Manchester 467M13 9PL, United Kingdom                          ;;;
;;;                                                                      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;(in-package "USER")
(in-package "FACT")

(defconstant *reasoner* "SHIQ")
(defconstant *reasoner-version-number* "8")

;;; ************** DATA STRUCTURES **************

(defstruct constraints
  "structure of a costraint system for a single individual"
  (c nil)
  (props nil)
  (all-rc nil)
  (all-r+c nil)
  (r-y nil)
  (f-y nil)
  (or-clauses nil)
  (bcp-cand nil)
;;;  (some-fc-ux nil)
  (some-rc-ux nil)
  (atleast nil)
  (atleast-ux nil)
  (atmost nil)
  (c-def-ux nil)
  (c-ux nil)
  (parent nil)
  (children nil)
  (or-level 0)
  (stack nil)
  (neq-list nil)
  (dep nil)
  (individual nil)
  (some-rc))

;;; ************** GLOBAL VARIABLES **************

;;; Variables controlling classifier features
(defparameter *inverse-roles* T
  "When T (NIL) enables (disables) inverse roles; default=T (for SHIQ)")
(defparameter *double-blocking* T
  "When T (NIL) enables (disables) double blocking; default=T (for SHIQ)")
(defparameter *subset-s-equivalent* T
  "When T (NIL) enables (disables) enhanced subset blocking strategy; default=nil (for SHIQ)")
(defparameter *top-level-caching* T
  "When T (NIL) enables (disables) top-level caching in subsumption tests; default=T")
(defparameter *full-caching* nil
  "When T (NIL) enables (disables) caching in satisfiability tests; default=NIL (for SHIQ)")
(defparameter *def-q-succ-neg-first* T
  "When T (NIL) selects negative (positive) literal first when defining Q-successors; default=T")

(defparameter *MAY-NEED-EXPANDING* nil)
(defparameter *global-state* nil)

;;; ************** SUBSUMPTION TESTING **************

(defun my-union (s1 s2)
  "Union that preserves s1 if s2 is already a subset of s1"
  (mapc #'(lambda (e) (pushnew e s1)) s2)
  s1)

(defun m-g-t-i (r s)
  "Returns most general transitive role that subsumes r and is subsumed by s"
;;; Deal quickly with easy case where r is transitive
  (if (r-trans-across r) r
    (let ((mgts nil))
      (mapc #'(lambda (r1)
;;; If r1 is transitive and is subsumed by s
		(if (and (r-trans-across r1)
			 (r-subsumes s r1))
;;; then If mgts not yet set or if r1 subsumes mgts
		    (if (or (not mgts) (r-subsumes r1 mgts))
;;; then set mgts to r1
			(setf mgts r1))))
	    (r-ancestors r))
;;; Return value of mgts
      mgts)))

(defun add-all-r+c (c-s r c ddb-label)
  "Propogates transitive (ALL R C) constraints"
;;; IN-FaCT
  (goto-c-s c-s *or-level*)
  (let ((r-c (list r c)))
    (cond
;;; if r-c already in all-r+c do nothing
     ((member r-c (constraints-all-r+c c-s) :test #'equal :key #'first))
     (T
;;; add new r+c constraint
      (push (list r-c ddb-label) (constraints-all-r+c c-s))))))

(defun expand-all-rc (c-s r c ddb-label trans p-list)
  "expands x:(:all R C) + xRy constraints"
  (and
;;; add regular value restriction y:C 
   (add-constraint c-s c ddb-label)
   (if *transitivity*
;;; if triggered by x:(:all+ R C) add y:(:all+ R C)
       (if trans (add-all-r+c c-s r c ddb-label)
;;; otherwise, add y:(:all+ S C) for each S s.t. S is transitive, R subsumes S,
;;; and S subsumes P, where P is the role connecting x and y
	 (every #'(lambda (s)
		    (if (and (r-trans-across s) (member r (r-ancestors s)))
			(add-all-r+c c-s s c ddb-label)
		      T))
		p-list))
     T)))

(defun already-there (c-s c)
  (or (member c (constraints-c c-s))
      (member c (constraints-c-ux c-s) :key #'first)))

(defun new-all-arc (r-c-s concept level)
  "Check if new (All R.C) concept is triggered by r-c-s"
  (let ((r (second concept)) (c (third concept)))
;;; if R in (all R C) is an ancestor of some S in list of roles
;;; connecting current node to c-s
    (if (some #'(lambda (s) (member r (r-ancestors s))) (car r-c-s))
	(let ((c-s (cdr r-c-s)))
;;; add C to unexpanded list in c-s (if its not there already)
	  (when (not (already-there c-s c))
	    (goto-c-s c-s *or-level*)
	    (push (list c level)
		  (constraints-c-ux c-s)))
;	  (pushnew (list c level) (constraints-c-ux c-s) :key #'first)
	  (if *transitivity*
;;; for every R1 in list of roles connecting current node to c-s
	      (mapc #'(lambda (r1)
;;; Set S to the most general trans role subsuming r1 and subsumed by r
			(let ((s (m-g-t-i r1 r)))
;;; If such an S was found
			  (if s
;;; then add an encoded (all S C) concept to unexpanded list in c-s
;;; (if its not there already)
			      (let ((new-c (neg-con (install-concept
						     (list :some s (neg-con c))))))
				(when (not (already-there c-s new-c))
				  (goto-c-s c-s *or-level*)
				  (push (list new-c level) (constraints-c-ux c-s)))))))
		    (car r-c-s)))))))
			
(defun new-all-rc (c-s concept level)
;;; add (all R C) constraint to c-s
  (push (list (cdr concept) level) (constraints-all-rc c-s))
;;; apply constraint to parent
;;; union dependencies with dep of edge (stored in c-s)
  (new-all-arc (constraints-parent c-s) concept (my-union level (constraints-dep c-s)))
;;; apply constraint to all children
;;; union dependencies with dep of edge (stored in child)
  (mapc #'(lambda (r-c-s) (new-all-arc r-c-s concept 
				       (my-union level (constraints-dep (cdr r-c-s))))) 
	(constraints-children c-s))
  c-s)

(defun clashing-atleast (c-s concept level)
  "True if adding atleast concept causes an immediate clash"
  (let ((clash 
	 (member-if #'(lambda (c)
			(let ((nrc (first c)))
			  (and (or (= (third nrc) *top*)
				   (eql (third nrc) (fourth concept)))
			       (r-subsumes (second nrc) (third concept))
			       (< (first nrc) (second concept)))))
		    (constraints-atmost c-s))))
    (when clash
      (dbg :clash "Clash[>=]")
      (setf *clash-level* (my-union level (second (first clash))))
      T)))

(defun redundant-atleast (c-s concept)
  "True if new atleast concept is redundant in c-s"
  (some #'(lambda (c)
	    (let ((nrc (c-definition (first c))))
	      (and (or (= (fourth concept) *top*)
		       (eql (fourth nrc) (fourth concept)))
		   (r-subsumes (third concept) (third nrc))
		   (>= (second nrc) (second concept)))))
	(constraints-atleast c-s)))

(defun new-atleast-nrc (c-s concept level)
;;; add (at-least n R C) constraint to c-s
  (if (clashing-atleast c-s concept level) nil
    (if (redundant-atleast c-s concept) c-s
      (let ((c-l (list (system-name concept) level)))
	(push c-l (constraints-atleast c-s))
	(push c-l (constraints-atleast-ux c-s))
	c-s))))

(defun clashing-atmost (c-s concept level)
  "True if adding atmost concept causes an immediate clash"
  (let ((clash 
	 (member-if #'(lambda (c)
			(let ((nrc (c-definition (first c))))
			  (and (or (= (fourth concept) *top*)
				   (eql (fourth nrc) (fourth concept)))
			       (r-subsumes (third concept) (third nrc))
			       (< (second concept) (second nrc)))))
		    (constraints-atleast c-s))))
    (when clash
      (dbg :clash "Clash[<=]")
      (setf *clash-level* (my-union level (second (first clash))))
      T)))

(defun redundant-atmost (c-s concept)
  "True if new atmost concept is redundant in c-s"
  (some #'(lambda (c)
	    (let ((nrc (first c)))
	      (and (or (= (third nrc) *top*)
		       (eql (third nrc) (fourth concept)))
		   (r-subsumes (second nrc) (third concept))
		   (<= (first nrc) (second concept)))))
	(constraints-atmost c-s)))

(defun new-atmost-nrc (c-s concept level)
;;; add (at-least n R C) constraint to c-s
  (if (clashing-atmost c-s concept level) nil
    (if (redundant-atmost c-s concept) c-s
      (progn
	(push (list (cdr concept) level) (constraints-atmost c-s))
	c-s))))

(defun add-constraint (c-s concept level)
  "Add a new constraint (concept level) to S; return nil if there is a clash."
  (dbg :addc-entry "~&   ADD-C ~S ~S" concept level)
;;; IN-FaCT
  (goto-c-s c-s *or-level*)
  (cond
;;; if concept is an atom...
   ((atom concept)
;;; if the concept we are about to add is an individual
    (if (c-individual concept)
;;; if this node is already an individual
	(if (constraints-individual c-s)
;;; if they are not the same individual
	    (if (not (eql concept (first (constraints-individual c-s))))
		(progn
;;; set the clash level
		  (dbg :clash "Clash[ind]")
		  (setf *clash-level* (my-union level (second (constraints-individual c-s))))
;;; return nil
		  nil)
;;; else if they are the same individual, add the concept
;;; could probably just return T here, but let's be cautious
	      (add-literal c-s concept level))
;;; else if this node is not already an individual
	  (progn
;;; mark it as an individual
	    (setf (constraints-individual c-s) (list concept level))
;;; add the concept
	    (add-literal c-s concept level)))
;;; else if the concept we are about to add is not an individual
      (add-literal c-s concept level)))
;;; if concept = (:and c1 ... cn)...
   ((eq (car concept) :and)
    (every #'(lambda (c)
;;; add c constraints for every c in the :and list
	       (add-constraint c-s c level))
;;; constraint is the :and list of concepts
	   (cdr concept)))

;;; if concept = (:or c1 ... cn)...
   ((eq (car concept) :or)
;;;   add or clause
    (add-or-clause c-s (cdr concept) level))

;;; if concept = (:all r c)...
   ((eq (car concept) :all)
;;;   expand :all constraint
    (new-all-rc c-s concept level))
;;;if concept = (:at-least n r c)
   ((eq (car concept) :at-least)
    (new-atleast-nrc c-s concept level))
;;;if concept = (:at-least n r c)
   ((eq (car concept) :at-most)
    (new-atmost-nrc c-s concept level))

;;; otherwise
   (T
;;;  its and ERROR - a bad concept expression
    (error-handler 'add-constraint :bad-concept-expansion))))

(defun add-xry (c-s r y-c-s level)
  "Check if (All S.C) concepts triggered by r-labeled edge to y-c-s"
  (let* ((r-a (r-ancestors r)))
    (and
;;; check if any (:all s c) constraints are triggered - s in ancestors of r.
     (every #'(lambda (s-c)
		(if (member (first (constraint s-c)) r-a)
		    (expand-all-rc y-c-s (first (constraint s-c)) (second (constraint s-c))
				   (my-union level (level s-c)) nil r-a)
		  t))
	    (constraints-all-rc c-s))
;;; check if any (:all s+ c) constraints are triggered - s in ancestors of r.
     (every #'(lambda (s-c)
		(if (member (first (constraint s-c)) r-a)
		    (expand-all-rc y-c-s (first (constraint s-c)) (second (constraint s-c))
				   (my-union level (level s-c)) t r-a)
		  t))
	    (constraints-all-r+c c-s)))))

(defun set-equal (s1 s2 &key (test #'eql))
  "returns T if the two sets s1 and s2 are equal"
  (and
   (= (length s1) (length s2))
   (every #'(lambda (e) (member (first e) s2 :test test :key #'first)) s1)))

(defun sat-in-parent (x-c s)
  (let ((r (first (constraint s))) (c (second (constraint s))))
;;;    if there is some p in list of roles connecting x-c to its parent s.t. p -> r
    (if (some #'(lambda (p) (member r (r-ancestors p)))
	      (car (constraints-parent x-c)))
;;;    then 
	(and
;;;      c in the label of the parent of x-c
	 (member c (constraints-c (cdr (constraints-parent x-c))))
;;;      and for every p1 in the list of roles connecting x-c to its parent
	 (every #'(lambda (p1)
;;;        for every p2 s.t. p1 -> p2
		    (every #'(lambda (p2)
;;;          if p2 is transitive and p2 -> r
			       (if (and (r-trans-across p2) 
					(member r (r-ancestors p2)))
;;;            then there is an (p2 c) constraint in the all-r+c list or
;;;            the all-rc list of x-c's parent
				   (or
				    (member (list p2 c) 
					    (constraints-all-r+c (cdr (constraints-parent x-c))) 
					    :test #'equal :key #'first)
				    (member (list p2 c) 
					    (constraints-all-rc (cdr (constraints-parent x-c))) 
					    :test #'equal :key #'first))
				 T))
			   (r-ancestors p1)))
		(car (constraints-parent x-c))))
      T)))

(defun no-functional-interaction (rl1 rl2)
  (notany #'(lambda (r)
	      (functional-interaction rl2 r))
	  rl1))

(defun s-equivalent (x-c y-c)
  "True if x and y are s-equivalent"
  (if *subset-s-equivalent*
;;; constraints on x-c are a subset of those on y-c
      (and (subsetp (constraints-c x-c) (constraints-c y-c))
	   (every #'(lambda (s)
		      (or
		       (member (constraint s) (constraints-all-r+c y-c) 
			       :test #'equal :key #'first)
		       (member (constraint s) (constraints-all-rc y-c) 
			       :test #'equal :key #'first)))
		  (constraints-all-r+c x-c))
;;; commented out in favour of more general test above	   
;;;	   (subsetp (constraints-all-r+c x-c) (constraints-all-r+c y-c)
;;;		    :test #'equal :key #'first)
;;; and if we are double blocking
	   (if *double-blocking*
;;; then if:
	       (if (and
;;; 1. every all-rc constraint on y-c must also be satisfied by x-c
		    (every #'(lambda (s)
			       (sat-in-parent x-c s))
			   (constraints-all-rc y-c))
;;; 2. every all-r+c constraint on y-c must also be satisfied by x-c
		    (every #'(lambda (s)
			       (sat-in-parent x-c s))
			   (constraints-all-r+c y-c))
;;; Then, either...
		    (or 
;;; a. we can make a cyclical model if it doesn't break atmost constraints on y-c....
		     (and
;;; a1. every atmost-nrc constraint on y-c must still be satisfied when cycle is added
		      (every #'(lambda (s)
				 (let ((nrc (first s)))
				   (let ((r (second nrc)) (c (third nrc)))
;;;   it is satisfied if atmost 0 r c sat in parent of x-c
;;;   i.e., either parent isn't an r-neighbout or (not c) in label of parent
;;;				     (or 
				     (atmost-n-r-c-succ (list (constraints-parent x-c)) 0 r c))))
;;;   The following weaker condition wasn't valid because y-c could c-block several
;;;   nodes with the result that the accumulated cyclical edges violated the
;;;   atmost-nrc constraint.
;;;   satisfied if atmost n-1 r c sat in y-c
;;;					 (atmost-n-r-c y-c (1- n) r c)))))
			     (constraints-atmost y-c))
;;; a2. every atmost-nrc constraint on parent of x-c must still be satisfied when cycle added
;;;     i.e., y-c can't be responsible for (partly) satisfying such a constraint, because it
;;;     could block several siblings of x-c.
		      (let ((parent-x-c (cdr (constraints-parent x-c)))
			    (parent-edge-label (car (constraints-parent x-c))))
			(every #'(lambda (s)
;;;     get n-r-c definition of constraint
				   (let ((nrc (c-definition (first s))))
;;;     get inverse of role, because we are working with label of edge from x-c to parent
				     (let ((r (r-inverse-f (third nrc))))
;;;     either there is no p in list of roles connecting x-c to its parent s.t. p -> r in nrc
				       (or 
					(notany #'(lambda (p) (member r (r-ancestors p)))
						parent-edge-label)
;;;     or (not c) from nrc is in label of x-c
					(member (neg-con (fourth nrc)) (constraints-c x-c))))))
			       (constraints-atleast parent-x-c)))
;;;   also need to check on (lack of) functional interactions
		      (no-functional-interaction (first (constraints-parent x-c)) (first (constraints-parent y-c)))
		      (every #'(lambda (s)
				  (no-functional-interaction (first (constraints-parent x-c)) (first s)))
			      (constraints-children y-c)))
		     (and
;;; b. we can build an infinite model if we can substitute y-c for x-c....
;;; b1. every atleast-nrc constraint on y-c must also be satisfied when y-c's parent replaced by x-c's parent
;;;   trivially satisfied if y-c has no parent (they must all be satisfied in children of y-c)
		      (or (endp (constraints-parent y-c))
;;;   otherwise need to check each constraint
			  (every #'(lambda (s)
				     (let ((nrc (c-definition (first s))))
				       (let ((n (second nrc)) (r (third nrc)) (c (fourth nrc)))
;;;   trivially satisfied if n is zero
					 (or (zerop n)
;;;   trivially satisfied if atleast 1 r c not sat in parent of y-c or sat in parent of x-c (count can only increase)
					     (not (atleast-n-r-c-succ (list (constraints-parent y-c)) 1 r c))
					     (atleast-n-r-c-succ (list (constraints-parent x-c)) 1 r c)
;;;   satisfied if atleast n r c sat in children of y-c
					     (atleast-n-r-c-succ (constraints-children y-c) n r c)))))
				 (constraints-atleast y-c)))
;;; b2. every atmost-nrc constraint on y-c must also be satisfied when y-c's parent replaced by x-c's parent
		      (every #'(lambda (s)
				 (let ((nrc (first s)))
				   (let ((n (first nrc)) (r (second nrc)) (c (third nrc)))
;;;   trivially satisfied if atmost 0 r c not sat in parent of y-c or sat in parent of x-c (count can only decrease)
				     (or (and (constraints-parent y-c)
					      (not (atmost-n-r-c-succ (list (constraints-parent y-c)) 0 r c)))
					 (atmost-n-r-c-succ (list (constraints-parent x-c)) 0 r c)
;;;   satisfied if atmost n-1 r c sat in children of y-c
					 (atmost-n-r-c-succ (constraints-children y-c) (1- n) r c)))))
			     (constraints-atmost y-c))
;;; b3. every some-rc constraint on y-c must also be satisfied when y-c's parent replaced by x-c's parent
;;;   trivially satisfied if y-c has no parent (they must all be satisfied in children of y-c)
		      (or (endp (constraints-parent y-c))
;;;   otherwise need to check each constraint
			  (every #'(lambda (s)
				     (let ((r (first s)) (c (second s)))
;;;   trivially satisfied if atleast 1 r c not sat in parent of y-c or sat in parent of x-c
				       (or (not (atleast-n-r-c-succ (list (constraints-parent y-c)) 1 r c))
					   (atleast-n-r-c-succ (list (constraints-parent x-c)) 1 r c)
;;;   satisfied if atleast 1 r c sat in children of y-c
					   (atleast-n-r-c-succ (constraints-children y-c) 1 r c))))
				 (constraints-some-rc y-c)))
;;; b4. also need to check on (lack of) functional interactions
		      (every #'(lambda (s)
				 (no-functional-interaction (first (constraints-parent x-c)) (first s)))
			     (constraints-children y-c)))))
;;;  FINALLY! - if the block is valid, then set *cycle* to T (and return T) 
		   (setf *cycle* T)
;;;  otherwise do nothing and return nil
		 )
;;;  if we were not double blocking, then we have already found a block, so set *cycle* to T (and return T)
	     (setf *cycle* T)
;;;  otherwise do nothing and return nil
	     ))
;;;  if we were not using subset blocking, then....
    (and (endp (set-exclusive-or (constraints-c x-c) (constraints-c y-c)))
	 (set-equal (constraints-all-r+c x-c) (constraints-all-r+c y-c) :test #'equal)
;;; IN-FaCT - also equal parents & parent role
	 (if *double-blocking*
	     (let ((x-c-p (cdr (constraints-parent x-c))) (y-c-p (cdr (constraints-parent y-c))))
	       (and
		(endp (set-exclusive-or (car (constraints-parent x-c))
					(car (constraints-parent y-c))))
		(endp (set-exclusive-or (constraints-c x-c-p) (constraints-c y-c-p)))
		(set-equal (constraints-all-r+c x-c-p) (constraints-all-r+c y-c-p) :test #'equal)))
	   T)
	 (setf *cycle* T))))

(defun directly-blocked (c-s)
  (when *blocking*
    (do ((y-c-s (cdr (constraints-parent c-s)) (cdr (constraints-parent y-c-s))))
	((or (null y-c-s) (s-equivalent c-s y-c-s)) y-c-s))))

(defun indirectly-blocked (c-s)
  (and (constraints-parent c-s)
       (blocked (cdr (constraints-parent c-s)))))

(defun blocked (c-s)
  (or (directly-blocked c-s)
      (indirectly-blocked c-s)))

(defun not-fully-solved (c-s)
  (some #'(lambda (c) (plusp (car c))) (constraints-or-clauses c-s)))

(defun not-expanded (c-s)
  "returns T if the constraint system c-s is not expanded"
;;; return logical or of the unexpanded constraint lists
  (or 
;;;   (constraints-some-fc-ux c-s)
   (constraints-some-rc-ux c-s)
   (constraints-c-def-ux c-s)
   (constraints-c-ux c-s)
   (constraints-atleast-ux c-s)
;;;   (some #'(lambda (c-d)
;;;	     (let ((c (c-definition (first c-d))))
;;;	       (not (atleast-n-r-c c-s (second c) (third c) (fourth c)))))
;;;	 (constraints-atleast-ux c-s))
   (not-fully-solved c-s)
   (applicable-atmost c-s)))

;;; NOT USED IN SHIQ
;;;(defun needs-expanding (c-s)
;;;  (and (not-expanded c-s)
;;;       (not (find-s-equivalent c-s))
;;;       (not (mergable-constraints c-s))))

(defun sorted-successors (s-list)
  (if *sort-lists*
      (stable-sort (reverse s-list)
	    #'(lambda (x y)
		(< (if (cdr x) (reduce #'max (cdr x)) -1)
		   (if (cdr y) (reduce #'max (cdr y)) -1))))
    (reverse s-list)))

(defun atleast-n-r-c-succ (c-c n r c)
  "T if already n r-successors from c-c that satisfy c"
  (if (zerop n) T
    (let ((s-c-c
;;; set s-c-c to sublist of c-c whose head is a c satisfying r-successor in c-c list
	   (member-if #'(lambda (s)
;;; r-successor if r is ancestor of some role in edge label
			  (and (some #'(lambda (r1)
					 (member r (r-ancestors r1))) (first s))
;;; c satisfying if c in constraints-props of successor
			       (or (eql *top* c)
				   (member c (constraints-c (cdr s))))))
		      c-c)))
;;; if s-c-c non-nil, recursive call with n-1 and tail of s-c-c
      (if s-c-c (atleast-n-r-c-succ (cdr s-c-c) (1- n) r c)))))

(defun atleast-n-r-c (c-s n r c)
  "T if c-s already has n r-neighbours (including parent) that satisfy c"
  (if (zerop n) T
;;; parent is r-neighbour if r is ancestor of some role in edge label
    (if (and (some #'(lambda (r1)
		       (member r (r-ancestors r1)))
		   (first (constraints-parent c-s)))
;;; and c satisfying if c in constraints-props of parent
	     (or (eql *top* c)
		 (member c (constraints-c (cdr (constraints-parent c-s))))))
	(atleast-n-r-c-succ (constraints-children c-s) (1- n) r c)
      (atleast-n-r-c-succ (constraints-children c-s) n r c))))

(defun atmost-n-r-c-succ (c-c n r c)
  "T if at most n r-successors from c-c that don't satisfy (not c)"
  (if (minusp n) nil
    (let ((s-c-c
;;; set s-c-c to sublist of c-c whose head is a c satisfying r-successor in c-c list
	   (member-if #'(lambda (s)
;;; r-successor if r is ancestor of some role in edge label
			  (and (some #'(lambda (r1)
					 (member r (r-ancestors r1))) (first s))
;;; c satisfying if c in constraints-props of successor
			       (not (eql *bottom* c))
			       (not (member (neg-con c)
					    (constraints-c (cdr s))))
			       (not (member (neg-con c)
					    (constraints-c-ux (cdr s)) :key #'first))))
		      c-c)))
;;; if s-c-c non-nil, recursive call with n-1 and tail of s-c-c
      (if s-c-c (atmost-n-r-c-succ (cdr s-c-c) (1- n) r c) T))))

(defun atmost-n-r-c (c-s n r c)
  "T if c-s has at most n r-neighbours (including parent) that don't satisfy (not c)"
;;; parent is r-neighbour if r is ancestor of some role in edge label
  (if (and (some #'(lambda (r1)
		     (member r (r-ancestors r1)))
		 (first (constraints-parent c-s)))
;;; and not (not c) satisfying if (not c) not in constraints-props of parent
	   (not (eql *bottom* c))
	   (not (member (neg-con c)
			(constraints-c (cdr (constraints-parent c-s)))))
	   (not (member (neg-con c)
			(constraints-c-ux (cdr (constraints-parent c-s)))
			:key #'first)))
      (atmost-n-r-c-succ (constraints-children c-s) (1- n) r c)
    (atmost-n-r-c-succ (constraints-children c-s) n r c)))

(defun applicable-atmost (c-s)
  "Returns first at-most concept making at-most-rule applicable to c-s"
  (first
   (member-if #'(lambda (nrc)
		  (let ((c (car nrc)))
		    (not (atmost-n-r-c c-s (first c) (second c) (third c)))))
	      (constraints-atmost c-s))))

(defun undef-q-succ (c-s r c)
  "Returns first r-neighbour where c is not defined"
  (first
;;; set s-c-c to sublist of c-c whose head is a c satisfying r-successor in c-c list
   (member-if #'(lambda (s)
;;; r-successor if r is ancestor of some role in edge label
		  (and (some #'(lambda (r1)
				 (member r (r-ancestors r1))) (first s))
;;; c satisfying if c in constraints-props of successor
		       (not (eql *bottom* c))
		       (not (eql *top* c))
		       (not (member c (constraints-c (cdr s))))
		       (not (member (neg-con c) (constraints-c (cdr s))))
		       (not (member c (constraints-c-ux (cdr s)) :key #'first))
		       (not (member (neg-con c)
				    (constraints-c-ux (cdr s)) :key #'first))))
	      (if (constraints-parent c-s)
		  (cons (constraints-parent c-s) (constraints-children c-s))
		(constraints-children c-s)))))

(defun q-succs (c-s r c)
  "Returns list of r-c neighbours"
  (mapcan
;;; set s-c-c to sublist of c-c whose head is a c satisfying r-successor in c-c list
   #'(lambda (s)
;;; r-successor if r is ancestor of some role in edge label
       (if (and (some #'(lambda (r1)
			  (member r (r-ancestors r1))) (first s))
;;; c satisfying if c in constraints-props of successor
		(or (eql *top* c)
		    (member c (constraints-c (cdr s)))
		    (member c (constraints-c-ux (cdr s)) :key #'first)))
	   (list s)))
   (if (constraints-parent c-s)
       (cons (constraints-parent c-s) (constraints-children c-s))
     (constraints-children c-s))))

(defun m-g-f-s (r)
  "Returns the most general functional role that subsumes r"
  (let ((mgfs (first (r-f-ancestors r))))
    (dolist (s (cdr (r-f-ancestors r)) mgfs)
      (if (member s (r-f-ancestors mgfs)) (setf mgfs s)))))

(defun functional-atmost (c-s r)
  (if (r-f-ancestors r)
      (new-atmost-nrc c-s (list :at-most 1 (m-g-f-s r) *top*) nil)
    T))
;;; Only need atmost restriction for most general functional subsumer of r
;;;  (every #'(lambda (s)
;;;	     (new-atmost-nrc c-s (list :at-most 1 s *top*) nil))
;;;	 (r-f-ancestors r)))
	      
(defun gen-succ (c-s r c dep neq-list)
  (let ((y-cs (make-constraints :parent (cons (list (r-inverse-f r)) c-s)
				:or-level *or-level*
				:neq-list neq-list
				:dep dep)))
;;; increment model-size and set max size
    (when (> (incf *model-size*) *max-model-size*)
      (setf *max-model-size* *model-size*))
;;; unless constraining concept is added ok, return nil - clash
    (if (and (add-constraint y-cs c dep)
;;; add the universal constraint due to GCIs
	     (if *universal-constraint*
		 (add-constraint y-cs *universal-constraint* nil)
	       T)
	     (functional-atmost c-s r)
	     (add-xry c-s r y-cs dep))
;;; add r-cs to list
	(push (cons (list r) y-cs) (constraints-children c-s)))))

(defun functional-interaction (r-list s)
  "Returns T if s and one of r-list have common functional ancestor"
  (some #'(lambda (r)
	    (some #'(lambda (r1)
		      (member r1 (r-f-ancestors r)))
		  (r-f-ancestors s)))
	r-list))

(defun find-child-edge (c-s child-c-s)
  "Returns edge from children list in c-s that points to child-c-s"
  (first (member-if #'(lambda (n) (eq child-c-s (cdr n))) 
		    (constraints-children c-s))))

(defun new-edge-label-check (r y-c-s dep all-rc)
  "Check if adding r to edge label triggers any (All S.C) concepts in all-rc"
;;; (All S.C) concept triggered if S subsumes r (i.e., r implies S)
;;; Check every (All S.C) concept in all-rc
  (every 
   #'(lambda (s-c)
       (let ((s (first (constraint s-c))))
	 (if (r-subsumes s r)
;;; Add C to unexpanded list in y-c-s (if its not there already)
	     (let ((new-dep (my-union dep (level s-c)))
		   (c (second (constraint s-c))))
	       (add-ux y-c-s c new-dep)
	       (if *transitivity*
;;; Find most general transitive subsumer of r that also subsumes s
		   (let ((r-trans (m-g-t-i r s)))
		     (if r-trans
			 (add-ux y-c-s
				 (neg-con (install-concept 
					   (list :some r-trans (neg-con c))))
				 new-dep)
		       T))
		 T))
	   T)))
   all-rc))
	      
(defun new-edge-label (c-s r y-c-s dep)
  "Check if adding r to edge label triggers any (All S.C) concepts in c-s"
;;; Have to check both all-rc and all-r+c concepts
  (and
   (new-edge-label-check r y-c-s dep (constraints-all-rc c-s))
   (new-edge-label-check r y-c-s dep (constraints-all-r+c c-s))))

(defun add-role (c-s neighbour r dep)
  "Add a new role to the label of an edge"
;;; Only take action in new role not already implied by some existing role
  (if (notany #'(lambda (s) (r-subsumes r s)) (first neighbour))
      (progn
;;; Save state at both ends of edge
	(goto-c-s c-s *or-level*)
	(goto-c-s (cdr neighbour) *or-level*)
	(let
;;; If neighbour is parent node
	    ((inv-neighbour (if (eq neighbour (constraints-parent c-s))
;;; then inverse edge is one of the children from the neighbour
				(find-child-edge (cdr neighbour) c-s)
;;; else inverse edge is parent from neighbour
			      (constraints-parent (cdr neighbour))))
;;; Update dependency set of edge (which is stored in successor node),
;;; and store value for future use
	     (new-dep (if (eq neighbour (constraints-parent c-s))
			  (setf (constraints-dep c-s)
			    (my-union (constraints-dep c-s) dep))
			(setf (constraints-dep (cdr neighbour))
			  (my-union (constraints-dep (cdr neighbour)) dep)))))
;;; Add r to edge label (in both directions)
	  (push r (first neighbour))
	  (push (inv-r r) (first inv-neighbour))
;;; Check if new edge triggers any (All R.C) concepts in node and neighbour
	  (and
	   (new-edge-label c-s r (cdr neighbour) new-dep)
	   (new-edge-label (cdr neighbour) (inv-r r) c-s new-dep))))
    T))

(defun gen-func-succ (c-s r c dep neq-list)
;;; check if parent is s-neighbour s.t. s subsumes r or r subsumes s
  (if (functional-interaction (first (constraints-parent c-s)) r)
      (and
       (add-role c-s (constraints-parent c-s) r dep)
       (goto-c-s (cdr (constraints-parent c-s)) *or-level*)
       (add-ux (cdr (constraints-parent c-s)) c (my-union dep (constraints-dep c-s))))
    (let ((f-succ (first (member-if #'(lambda (s) 
					(functional-interaction (first s) r))
				    (constraints-children c-s)))))
      (if f-succ
	  (and
	   (add-role c-s f-succ r dep)
	   (goto-c-s (cdr f-succ) *or-level*)
	   (add-constraint (cdr f-succ) c (my-union dep (constraints-dep (cdr f-succ)))))
	(gen-succ c-s r c dep neq-list)))))
      
(defun generate-successors (c-s)
  "Generate successors required by :some and :at-least constraints on c-s"
;;; IN-FaCT
  (goto-c-s c-s *or-level*)
;;; loop through rc-ux list until empty - reverse it so as to process oldest first
  (let ((s-s (constraints-some-rc-ux c-s)))
    (setf (constraints-some-rc-ux c-s) nil)
    (dolist (r-c s-s c-s)
      (let ((r (caar r-c)) (c (cadar r-c)))
	(when (and
;;; r-c not satisfied by any successor
	       (notany #'(lambda (r-cs)
			   (and 
			    (some #'(lambda (rc) (member r (r-ancestors rc)))
				  (car r-cs))
			    (member c (constraints-c (cdr r-cs)))))
		       (constraints-children c-s))
;;; and r-c not satisfied by parent
	       (not
		(and
		 (some #'(lambda (r-p)
			   (member r (r-ancestors r-p)))
		       (car (constraints-parent c-s)))
		 (member c (constraints-c (cdr (constraints-parent c-s)))))))
;;; then:
;;; 1: remove (r c) from list constraints satisfied in parent
;;;	  (setf (constraints-some-rc c-s) 
;;;	    (remove (list r c) (constraints-some-rc c-s) :test #'equal))
;;; 2: generate a successor
	  (unless 
	      (if (r-functional r)
		  (gen-func-succ c-s r c (cdr r-c) nil)
		(gen-succ c-s r c (cdr r-c) nil))
	    (return-from generate-successors nil)))))
;;; Delete FULLY expanded >=nrc constraints
    (let ((uxal (constraints-atleast-ux c-s)))
      (setf (constraints-atleast-ux c-s) nil)
      (dolist (c-d uxal c-s)
	(let ((c (c-definition (first c-d))) (dep (cadr c-d)))
	  (let ((n (second c)) (r (third c)) (c (fourth c)))
	    (if (not (atleast-n-r-c c-s n r c))
		(dotimes (i n c-s)
		  (unless (gen-succ c-s r c dep (list (first c-d)))
		    (return-from generate-successors nil))))))))))

;;; IN-FaCT
(defun sat-mnx (c-s)
  (if *may-need-expanding*
      (let ((x-c-s (car *may-need-expanding*)))
	(progv '(*may-need-expanding*) (list (cdr *may-need-expanding*))
	  (if (sat x-c-s) c-s)))
    c-s))

(defun sat-children (c-s)
  (if (constraints-children c-s)
      (progv
	  '(*may-need-expanding*)
	  (dolist (c (cdr (constraints-children c-s)) (list *may-need-expanding*))
;;; Add child to list if its connecting role isn't nil
	    (if (first c) (push (cdr c) *may-need-expanding*)))
	(if (sat (cdr (first (constraints-children c-s)))) c-s))
    (sat-mnx c-s)))

(defun parent-needs-expanding (c-s)
  (and (constraints-parent c-s)
       (constraints-c-ux (cdr (constraints-parent c-s)))))

(defun descendent-node (c-s-1 c-s-2)
  "True if c-s-1 is a descendent node of c-s-2"
  (or (eq c-s-1 c-s-2)
      (some #'(lambda (c) (descendent-node c-s-1 (cdr c)))
	    (constraints-children c-s-2))))

(defun sat-parent (c-s)
  (let ((parent (cdr (constraints-parent c-s))))
    (progv
	'(*may-need-expanding*)
	(list (remove-if #'(lambda (c) (descendent-node c parent))
			 *may-need-expanding*))
      (sat parent))))

(defun sat (c-s)
  "Returns T if constraint system is satisfiable, nil otherwise"
;;; Expand parent if it is required
  (if (parent-needs-expanding c-s)
      (sat-parent c-s)
;;; If local deterministic expansion performed OK
    (if (and (expand-ux-concepts c-s)
	     (expand-defined-concepts c-s))
;;; Expand parent if it is required
	(if (parent-needs-expanding c-s)
	    (sat-parent c-s)
;;; If some non-deterministic expansion to be done, then do that
	  (if (not-fully-solved c-s)
	      (add-ux-or-constraints c-s)
	    (if (applicable-atmost c-s)
		(apply-atmost c-s)
;;; If not directly blocked
;;; NOTE can use directly blocked (which is faster to compute) because
;;; we always work forwards from fully expanded and non-blocked nodes 
;;; (so ancestor node can never become blocked)
	      (if (directly-blocked c-s)
		  (sat-mnx c-s)
		(if (generate-successors c-s)
		    (if (parent-needs-expanding c-s)
			(sat-parent c-s)
		      (if (applicable-atmost c-s)
			  (apply-atmost c-s)
			(sat-children c-s)))))))))))
	  
(defun full-sat (c-s)
  (setf *may-need-expanding* nil)
  (and
;;; add the universal constraint due to GCIs
   (if *universal-constraint*
	 (add-constraint c-s *universal-constraint* nil)
     T)
   (sat c-s)))

(defun simple-sat (c-s)
  (setf *may-need-expanding* nil)
  (progv '(*universal-constraint*) `(nil)
	 (sat c-s)))

(defun test-sat (exp)
  (dbg :test-sat "~&Test sat: ~S~%" (decode-concept exp))
  (incf *total-sat-tests*)
  (let ((c-s (make-constraints))
	sat-result recursive-run-time mmd mms
	(s-s *search-space*) (c-a *cache-accesses*) (c-h *cache-hits*))
    (setq *search-space* 0 *cache-accesses* 0 *cache-hits* 0)
;;; set *or-level* to 0 using progv so we can handle recursive calls to test-sat
    (progv '(*global-state* *MAY-NEED-EXPANDING* 
	     *or-level* *model-size* *max-model-size*
	     *model-depth* *max-model-depth*
	     *start-time* *cycle*)
	`(nil nil 0 1 1 1 1 ,(get-internal-run-time) nil)
	   (setf recursive-run-time *start-time*)
	   (setf sat-result 
		 (and
		  (add-constraint c-s exp nil)
		  (if *concept-eqn* (full-sat c-s) (simple-sat c-s))))
	   (when (> *profiling* 2)
;;; time taken by this test EXCLUDING any recursive caching tests
		 (setf *run-time* (- (get-internal-run-time) *start-time*))
		 (profile-out exp (not (null sat-result))))
	   (setf mmd *max-model-depth*)
	   (setf mms *max-model-size*)
;;; time taken by this test INCLUDING any recursive caching tests
	   (setf recursive-run-time (+ *run-time* (- *start-time* recursive-run-time))))
    (setf *max-model-size* (max *max-model-size* (+ *model-size* mms)))
    (setf *max-model-depth* (max *max-model-depth* (+ *model-depth* mmd)))
    (incf *search-space* s-s)
    (incf *cache-accesses* c-a)
    (incf *cache-hits* c-h)
    (incf *start-time* recursive-run-time)
    sat-result))

(defun test-and-cache (c)
  "Check if c has a model and, if so, store relevant details in c-model"
  (dbg :test-and-cache "~&~ATest&cache ~S"
       (make-string *tsd* :initial-element #\Space) c)
;;; set model to *BOTTOM* (no model) to prevent infinite recursive calls
  (setf (c-model c) *BOTTOM*)
;;; NOTE *tsd* is a recursion depth counter for debugging
  (incf *tsd*)
;;; calls test-sat to return model of c
  (let ((c-s (test-sat c)))
    (decf *tsd*)
;;; if c is satisfiable
    (if c-s
;;;  keep a note of relevant parts of top node of model
	(setf (c-model c) (make-constraints
;;; keep a note of all x:C constraints
			   :c (constraints-c c-s)
;;; keep a note of all Rs in all R.C constraints
			   :all-rc (delete-duplicates (mapcar #'caar (constraints-all-rc c-s)))
;;; keep a note of all Rs in children list
			   :r-y (delete-duplicates
				 (mapcan #'first 
					 (if (constraints-parent c-s)
					     (cons (constraints-parent c-s)
						   (constraints-children c-s))
					   (constraints-children c-s))))
;;; keep a note of all fs in xfy constraints
			   :atmost (mapcar #'cadar (constraints-atmost c-s))
;;; keep a note of any individual concept
			   :individual (first (constraints-individual c-s))))
;;; otherwise if c isn't satisfiable
      (progn
	(dbg :test-and-cache-incoherent "~&~AINCOHERENT ~S"
	     (make-string *tsd* :initial-element #\Space) c)
	(verbosity :notes "~&!!NOTE!! ~A is INCOHERENT~%" (decode-concept c))
;;; set c to a synonym for :bottom and NOT c to a synonym for :top
	(when (c-grail-name c)
	      (setf (system-name (c-grail-name c)) *BOTTOM*))
	(setf (c-synonym c) *BOTTOM*)
	(setf (c-asserted-supers c) (list *BOTTOM*))
	(when (c-grail-name (neg-con c))
	      (setf (system-name (c-grail-name (neg-con c))) *TOP*))
	(setf (c-synonym (neg-con c)) *TOP*)
	(setf (c-asserted-supers c) nil)
;;; BUG FIX 18/1/01: Setting definition in this way is dangerous when there is
;;; a *universal-constraint* as we might remove part of its definition.
;;; In fact I now realise that it is ALWAYS DANGEROUS (see tests.cl, bug-test1b).
;;; Disable behaviour for now (the setting to *BOTTOM* part may still be OK
;;; but it needs more thought.
	(unless T			; *universal-constraint*
	  (setf (c-definition c) *BOTTOM*)
	  (setf (c-primitive c) nil)
	  (setf (c-definition (neg-con c)) *TOP*)
	  (setf (c-primitive (neg-con c)) nil))
;;; return nil
	nil))))

(defun get-cached-model (c)
  (cond
;   ((eql c *TOP*) T)
   ((eql c *BOTTOM*) nil)
   (T
    (let ((m (c-model c)))
      (if m (unless (eql m *BOTTOM*) m)
	(test-and-cache c))))))

(defun notany-member (l1 l2)
  (notany #'(lambda (c)
	      (member c l2)) l1))

(defun notany-neg-member (l1 l2)
  (notany #'(lambda (c)
	      (member (neg-con c) l2)) l1))

(defun mergable (c1 c2)
  (let ((mc1 (get-cached-model c1)) (mc2 (get-cached-model c2)))
    (and
     mc1 mc2
     (not (and (constraints-individual mc1) 
	       (constraints-individual mc2) 
	       (not (eql (constraints-individual mc1) (constraints-individual mc2)))))
     (notany-neg-member (constraints-c mc1) (constraints-c mc2))
     (every #'(lambda (r)
		(notany-member (r-ancestors r) (constraints-all-rc mc2)))
	    (constraints-r-y mc1))
     (every #'(lambda (r)
		(notany-member (r-ancestors r) (constraints-all-rc mc1)))
	    (constraints-r-y mc2))
     (every #'(lambda (r)
		(notany-member (r-ancestors r) (constraints-atmost mc2)))
	    (constraints-r-y mc1))
     (every #'(lambda (r)
		(notany-member (r-ancestors r) (constraints-atmost mc1)))
	    (constraints-r-y mc2)))))

(defun no-possible-clash (c1 all-r+c c-list)
  (let ((mc1 (get-cached-model c1)))
    (when mc1
	  (let ((r-y-mc1 (constraints-r-y mc1))
		(all-rc-mc1 (constraints-all-rc mc1)))
	    (and
	     (every #'(lambda (r)
			(notany #'(lambda (s)
				    (member s all-r+c :key #'caar))
				(r-ancestors r)))
		    r-y-mc1)
	     (every #'(lambda (c2)
			(let* ((mc2 (get-cached-model c2)))
			  (and
			   mc2
			   (let ((r-y-mc2 (constraints-r-y mc2))
				 (all-rc-mc2 (constraints-all-rc mc2)))
			     (and
			      (notany-neg-member (constraints-c mc1) (constraints-c mc2))
			      (every #'(lambda (r)
					 (notany-member (r-ancestors r) all-rc-mc2))
				     r-y-mc1)
			      (every #'(lambda (r)
					 (notany-member (r-ancestors r) all-rc-mc1))
				     r-y-mc2)
			      (notany-member (constraints-f-y mc1) (constraints-f-y mc2)))))))
		    c-list))))))

(defun mergable-constraints (c-s)
  "Check if models of constraints in c-s can be merged"
  (when *full-caching*
	(let ((s-t *total-sat-tests*) (c-s-t *caching-sat-tests*)
	      (all-r+c (constraints-all-r+c c-s)))
	  (incf *cache-accesses*)
	  (dbg :cache-merging "~&Merge ~S? - "
	       (constraints-c c-s))
	  (cond
	   ((do ((c-list (constraints-c c-s) (cdr c-list))
		 (res T (no-possible-clash (car c-list) all-r+c (cdr c-list))))
		((or (endp c-list) (not res)) res))
	    (dbg :cache-merging "YES~%")
	    (setf *caching-sat-tests* (+ c-s-t (- *total-sat-tests* s-t)))
	    (incf *cache-hits*))
	   (T
	    (dbg :cache-merging "NO~%")
	    (setf *caching-sat-tests* (+ c-s-t (- *total-sat-tests* s-t)))
	    nil)))))

(defun obvious-non-subs (c1 c2)
;;; Spots obvious non-subsumptions via easily satisfiable cases of (C2 & not(C1)).
  (when (and *top-level-caching*
	     (or
;;; One such case is if C1 is :bottom and C2 has a model...
	      (and (eql c1 *BOTTOM*) (get-cached-model c2))
;;; another case is if C2 is :top and (:not C1) has a model (i.e. C1/=top)
	      (and (eql c2 *TOP*) (get-cached-model (neg-con c1)))
;;; another case is if c2 and (:not c1) both have models & are mergeable
	      (mergable c2 (neg-con c1))))
	(dbg :found-obvious-non-subs "~&OBVIOUS NON-SUBSUMPTION - not(~S > ~S)" c1 c2)
	T))

(defun obvious-subs (c1 c2)
;;; obvious subsumption if c1=top or c2=bottom
;;; or (:not c1)=incoherent (i.e. c1=top) or c2=incoherent (i.e. c2=bottom)
  (when *obvious-subs*
	(or (eql c1 *TOP*) (eql c2 *BOTTOM*) (not (get-cached-model (neg-con c1)))
	    (not (get-cached-model c2)))))

(defun test-subsumes (c1 c2)
  (incf *total-subs-tests*)
  (dbg :subs-entry-1 "~&     ~S > ~S ?" c1 c2)
  (dbg :subs-entry-2 ".")
  (when (> *profiling* 1)
	(if *profile-file*
	    (format *profile-file* "~%(~S ~S ("
		    (decode-concept c1) (decode-concept c2))
	  (format T "~&Subsumption test - ~S > ~S ?"
		  (decode-concept c1) (decode-concept c2))))
;;; c1 > c2 if c2 and not c1 is not satisfiable. If c1 is :bottom and c2 has a model -
;;; so c2 is known to be coherent - can just return nil
  (let ((subs-result
	 (if (obvious-subs c1 c2) T
	   (unless (obvious-non-subs c1 c2)
		   (setq *model-size* 1 *max-model-size* 1
			 *model-depth* 1 *max-model-depth* 1)
		   (not (test-sat `(:and ,c2 ,(neg-con c1))))))))
    (when (and (> *profiling* 1) *profile-file*)
	  (format *profile-file* "))"))
    subs-result))

(defun expand-defined-concepts (c-s)
  "Expand defined concepts in c-s"
;;; IN-FaCT
  (goto-c-s c-s *or-level*)
;;; expand all defined concepts
;;; need outer loop because new ones might be added by inner loop
  (loop
;;; get local copy of unexpanded defined concepts lisp
   (let ((def-ux (constraints-c-def-ux c-s)))
;;; clear the list in c-s
     (setf (constraints-c-def-ux c-s) nil)
;;; return nil unless all the definitions are added OK
     (unless (every #'(lambda (constraint)
			(add-constraint c-s (c-definition (first constraint))
					(second constraint)))
		    def-ux)
	     (return nil)))
;;; exit loop with T if no more to be processed
   (when (endp (constraints-c-def-ux c-s)) (return T))))

(defun expand-ux-concepts (c-s)
  "Expand unexpanded concepts in c-s"
;;; IN-FaCT
  (goto-c-s c-s *or-level*)
;;; expand all defined concepts
;;; need outer loop because new ones might be added by inner loop
  (loop
;;; get local copy of unexpanded defined concepts lisp
   (let ((def-ux (constraints-c-ux c-s)))
;;; clear the list in c-s
     (setf (constraints-c-ux c-s) nil)
;;; return nil unless all the definitions are added OK
     (unless (every #'(lambda (constraint)
			(add-constraint c-s (first constraint)
					(second constraint)))
		    def-ux)
	     (return nil)))
;;; exit loop with T if no more to be processed
   (when (endp (constraints-c-ux c-s)) (return T))))


;;; ************** POSIT code ************** 

(defun pos-lit (p)
  (logxor (logior p 1) 1))

(defun pos-lit-p (p)
  (zerop (logand p 1)))

(defun shift-left (i dist)
  (dpb i (byte 16 dist) 0))

(defun alpha (pos-p neg-p)
  (+ (shift-left (* pos-p neg-p) 16) pos-p neg-p))

(defun jw-weight (clause-len max-len)
  (shift-left 1 (- max-len clause-len)))

(defun dependencies (lit-list)
  "Return a dependency set from lit-list"
;;; d-set is the union of all the d-sets from propositions corresponding to literals in lit-list
  (let ((d-set (fifth (second (car lit-list)))))
    (dolist (lit (cdr lit-list) d-set)
	    (setf d-set (my-union d-set (fifth (second lit)))))))

(defun add-or-clause (c-s clause d-set)
  "Add disjunctive clause to constraint system"
;;; IN-FaCT
  (goto-c-s c-s *or-level*)
;;; initialise open-literal-count, list of literals in clause and list of new
;;; proposition entries
  (let ((o-l-c 0) lit-list new-props)
;;; loop through literals in clause
    (dolist (lit clause)
;;; set p=proposition (positive literal); p-entry=corresponding entry in (constraints-props c-s)
	    (let* ((p (pos-lit lit))
		   (p-entry (car (member p (constraints-props c-s) :key #'first))))
;;; if an entry in (constraints-props c-s) was found
	      (if p-entry
		  (progn
;;; if lit already satisfied bomb out with T
		    (when (eql lit (fourth p-entry))
			  (return-from add-or-clause T))
;;; if lit is open then increment open literal count
		    (unless (fourth p-entry)
;;; increment open literal count
			    (incf o-l-c))
;;; add list (literal p-entry) to lit-list
		    (push (list lit p-entry) lit-list))
;;; if there isn't an existing p-entry, make a new one
;;; p-entry = (proposition pos-lits neg-lits theta ddb-set) where:
;;;    proposition = name of proposition (a positive literal)
;;;    pos-lits = list of clauses containing positive proposition
;;;    neg-lits = list of clauses containing negative proposition
;;;    theta = +p, -p or nil if p is open
;;;    d-set = dependency set; should be nil if theta is nil
		(let ((new-p (list p nil nil nil nil)))
;;; add new-p to list of new propositions - don't add directly to (constraints-props c-s) because
;;; clause may be satisfied already
		  (push new-p new-props)
;;; add list (literal new-p) to lit-list
		  (push (list lit new-p) lit-list)
;;; increment open literal count
		  (incf o-l-c)))))
;;; if there are no open literals then its a clash
    (when (zerop o-l-c)
;;; set *clash-set* to union of d-sets from clause and all its literals
      (dbg :clash "Clash[or]")
      (setf *clash-level* (my-union d-set (dependencies lit-list)))
;;; return nil
      (return-from add-or-clause nil))
;;; add new-props to (constraints-props c-s)
    (setf (constraints-props c-s) (nconc new-props (constraints-props c-s)))
;;; build new clause record = (olc lits d-set) where:
;;;    olc = number of open literals or -1 if clause satisfied
;;;    lits = list of (lit p-entry) pairs for each literal in the clause
;;;    d-set = dependency set
    (let ((clause-rec (list o-l-c lit-list d-set)))
;;; for each literal in lit-list
      (dolist (p lit-list)
;;; add clause-rec to pos-lit or neg-lit lists in p-entry
	      (if (pos-lit-p (car p))
		  (push clause-rec (second (second p)))
	      (push clause-rec (third (second p)))))
;;; add clause record to (constraints-or-clauses c-s)
      (push clause-rec (constraints-or-clauses c-s))
;;; if open lit count is 1, add clause record to bcp candidates list
      (when (eql o-l-c 1)
	    (push clause-rec (constraints-bcp-cand c-s)))))
;;; return T - clause was added without causing a clash
  T)

(defun extend-prop (c-s p-entry lit d-set)
  "Extend the solution by setting truth value of p-entry to lit"
;;; set proposition truth val to lit
  (setf (fourth p-entry) lit)
;;; set proposition dependency set to d-set
  (setf (fifth p-entry) d-set)
;;; when lit is *BOTTOM* set *clash-level* and bomb out
  (when (eql lit *BOTTOM*)
    (dbg :clash "Clash[bot]")
    (setf *clash-level* d-set)
    (return-from extend-prop nil))
;;; add lit to constraints-c list for use in blocking etc
  (push lit (constraints-c c-s))
;;; if its a positive literal
  (if (pos-lit-p lit)
      (progn
;;; for each clause in neg-lits list
	(dolist (clause (third p-entry))
;;; decrement open literals count
		(decf (car clause))
;;; if open literals count is zero, its a clash
		(when (zerop (car clause))
;;; set *clash-set* to union of d-sets from clause and all its literals
		  (dbg :clash "Clash[or]")
		      (setf *clash-level* (my-union (third clause) (dependencies (second clause))))
;;; return nil
		      (return-from extend-prop nil))
;;; if open literals count is one, clause is a candidate for BCP
		(when (eql (car clause) 1)
		      (push clause (constraints-bcp-cand c-s))))
;;; for each clause in pos-lits list, set open literals count to -1 = satisfied
	(dolist (clause (second p-entry))
		(setf (car clause) -1)))
;;; if its a negative literal
      (progn
;;; for each clause in pos-lits list
	(dolist (clause (second p-entry))
;;; decrement open literals count
		(decf (car clause))
;;; if open literals count is zero, its a clash
		(when (zerop (car clause))
;;; set *clash-set* to union of d-sets from clause and all its literals
		  (dbg :clash "Clash[or]")
		      (setf *clash-level* (my-union (third clause) (dependencies (second clause))))
;;; return nil
		      (return-from extend-prop nil))
;;; if open literals count is one, clause is a candidate for BCP
		(when (eql (car clause) 1)
		      (push clause (constraints-bcp-cand c-s))))
;;; for each clause in neg-lits list, set open literals count to -1 = satisfied
	(dolist (clause (third p-entry))
		(setf (car clause) -1))))
;;; if lit is a synonym
;;; Bug fix - don't use the synonym value - see tests.cl bug-test2
  (if nil				;(/= lit (c-synonym lit))
;;;   add synonym literal and return result
      (add-literal c-s (c-synonym lit) d-set)
;;;   otherwise get the literals definition
    (let ((c-def (c-definition lit)))
;;;   and when it has a definition add it to some-rc-ux or c-def-ux for future expansion
      (when c-def
	    (if (and (listp c-def) (eq (car c-def) :some))
;;;		(if (r-functional (second c-def))
;;;		    (push (cons (cdr c-def) d-set) (constraints-some-fc-ux c-s))
		(progn
;;; add (R C) to list of R-C constraints that may be (partly) satisfied in parent
;;; and so must be considered in (double) blocking
		  (pushnew (cdr c-def) (constraints-some-rc c-s) :test #'equal)
		  (push (cons (cdr c-def) d-set) (constraints-some-rc-ux c-s)))
;;;		  )
	      (push (list lit d-set) (constraints-c-def-ux c-s))))
;;; return T - there was no clash
      T)))

(defun add-literal (c-s lit d-set)
  "Add literal to constraint system"
;;; IN-FaCT
  (goto-c-s c-s *or-level*)
;;; p-entry := proposition record in constraints-props
  (let ((p-entry (car (member (pos-lit lit) (constraints-props c-s) :key #'first))))
;;; if there was an entry
    (if p-entry
;;; the fourth item in the entry is its truth-val = lit, not lit or nil
	(let ((truth-val (fourth p-entry)))
;;; if lit=truth-val nothing to be done - return T
	  (when (eql lit truth-val)
		(return-from add-literal T))
;;; if truth val is non-nil it must be not lit -> clash: set ddb set & return nil
	  (when truth-val
	    (dbg :clash "Clash[simple]")
		(setf *clash-level* (my-union d-set (fifth p-entry)))
		(return-from add-literal nil))
;;; return the result of calling call extend-prop to assign truth value
	  (and (extend-prop c-s p-entry lit d-set)
;;;   followed by b-c-p
	       (b-c-p c-s)))
;;; if there was no p-entry, make a new one
      (let ((new-p (list (pos-lit lit) nil nil nil nil)))
;;; add it to constraints-props
	(push new-p (constraints-props c-s))
;;; and set its truth value
	(extend-prop c-s new-p lit d-set)))))

(defun b-c-p (c-s)
  "Expand disjunctions with only one open literal"
;;; IN-FaCT
  (goto-c-s c-s *or-level*)
;;; loop until candidates stack is empty or there is a clash
  (loop
;;; return T when candidates stack is empty
   (when (endp (constraints-bcp-cand c-s))
	 (return-from b-c-p T))
;;; pop next candidate off the stack - its a clause record = (olc lits d-set)
   (let ((candidate (pop (constraints-bcp-cand c-s))))
;;; when olc still = 1
     (when (eql (car candidate) 1)
;;; look through lits list to find open lit - fourth elt in p-entry is nil
	   (let ((lit (find-if #'(lambda (l) (not (fourth (second l)))) (second candidate))))
;;; its an error if we didn't find it
	     (unless lit
		     (error "~&!!ERROR!! - open literal not found in BCP~%"))
;;; extend c-s by setting literal in p-entry; dependencies = union of d-sets from other lits
	     (unless (extend-prop c-s (second lit) (first lit)
				  (my-union (third candidate) (dependencies (second candidate))))
;;; return nil if extend-prop caused a clash
		     (return-from b-c-p nil)))))))

(defun pure-lit-candidates (c-s)
  "Return sorted list of propositions which appear in binary clauses and their priorities"
  (let (candidates (max-priority -1))
;;; loop through propositions list and return candidates
    (dolist (prop (constraints-props c-s) candidates)
;;; only consider open propositions
	    (when (and (not (fourth prop)) (or (endp (second prop)) (endp (third prop))))
		  (let ((pos-clauses (length (second prop)))
			(neg-clauses (length (third prop))))
		    (let ((priority (max pos-clauses neg-clauses)))
		      (when (>= priority max-priority)
			    (when (> priority max-priority)
				  (setf max-priority priority)
				  (setf candidates nil)))
;;; add to candidates a list of (prop pos-occurences neg-occurences priority)
		      (push (list prop pos-clauses neg-clauses) candidates)))))))

(defun mom-candidates (c-s)
  "Return sorted list of propositions which appear in binary clauses and their priorities"
  (let (candidates (max-priority -1))
;;; loop through propositions list and return candidates
    (dolist (prop (constraints-props c-s) candidates)
;;; only consider open propositions
	    (when (not (fourth prop))
		  (let ((pos-bin 0) (neg-bin 0))
;;; count the number of times the proposition appears positively in binary clauses
		    (dolist (clause (second prop))
			    (when (eql (car clause) 2) (incf pos-bin)))
;;; count the number of times the proposition appears negatively in binary clauses
		    (dolist (clause (third prop))
			    (when (eql (car clause) 2) (incf neg-bin)))
;;; unless both are zero
		    (unless (and (zerop pos-bin) (zerop neg-bin))
			    (let ((priority (alpha pos-bin neg-bin)))
			      (when (>= priority max-priority)
				    (when (> priority max-priority)
					  (setf max-priority priority)
					  (setf candidates nil)))
;;; add to candidates a list of (prop pos-occurences neg-occurences priority)
			      (push (list prop pos-bin neg-bin) candidates))))))))

(defun clause-lengths (c-s)
  "Return a dotted pair (min-clause-len . max-clause-len)"
  (let ((min-len most-positive-fixnum) (max-len 0))
    (dolist (clause (constraints-or-clauses c-s))
	    (let ((c-len (car clause)))
	      (when (plusp c-len)
		    (when (> c-len max-len)
			  (setf max-len c-len))
		    (when (< c-len min-len)
			  (setf min-len c-len)))))
    (cons min-len max-len)))

(defun prop-jw-weights (p max-clause-len)
  "Return a dotted pair (pos-jw-weight . neg-jw-weight) for proposition p"
  (let ((pos-wt 0) (neg-wt 0))
;;; calculate weights from clauses where the proposition appears positively
    (dolist (clause (second p))
	    (incf pos-wt (jw-weight (car clause) max-clause-len)))
;;; calculate weights from clauses where the proposition appears negatively
    (dolist (clause (third p))
	    (incf neg-wt (jw-weight (car clause) max-clause-len)))
    (cons pos-wt neg-wt)))

(defun jw-candidates (c-s)
  "Return sorted list of propositions and their Jereslow & Wang priorities"
  (let ((max-clause-len (cdr (clause-lengths c-s))) candidates (max-priority -1))
;;; loop through propositions list
    (dolist (prop (constraints-props c-s) candidates)
;;; only consider open propositions
	    (when (not (fourth prop))
;;; calculate j&w weights
		  (let* ((jww (prop-jw-weights prop max-clause-len))
			 (priority (alpha (car jww) (cdr jww))))
		    (when (>= priority max-priority)
			  (when (> priority max-priority)
				(setf max-priority priority)
				(setf candidates nil)))
;;; add to candidates a list of (prop pos-wt neg-wt priority)
		    (push (list prop (car jww) (cdr jww)) candidates))))))

(defun full-moms-heuristic (c-s)
  (let ((candidates (mom-candidates c-s)))
;;; if candidates from binary clauses were found
    (if candidates
;;; if there is more than one candidate (with the same priority)
	(if (cdr candidates)
	    (let (final-candidate (max-priority -1)
				  (max-clause-len (cdr (clause-lengths c-s))))
;;; loop through candidates and return final-candidate
	      (dolist (c candidates final-candidate)
;;; evaluate Jereslow & Wang positive and negative weightings for candidate and its total priority
		      (let* ((jww (prop-jw-weights (car c) max-clause-len))
			     (priority (alpha (car jww) (cdr jww))))
;;; when the priority is >= the max-priority
			(when (>= priority max-priority)
;;; if the priority = max-priority
			      (if (= priority max-priority)
;;; if positive occurences outnumber negative occurences
				  (if (> (car jww) (cdr jww))
;;;   and when positive literals are preferred
				      (when *prefer-pos-lits*
;;;   set the final candidate to the new candidate
					  (setf final-candidate
						(list (car c) (car jww) (cdr jww))))
;;; if positive occurences didn't outnumber negative occurences,
;;; and positive literals are not prefered
				    (unless *prefer-pos-lits*
;;;   set the final candidate to the new candidate
					  (setf final-candidate
						(list (car c) (car jww) (cdr jww)))))
;;; when priority > max-priority, set the max-priority and the final candidate
				(progn
				  (setf max-priority priority)
				  (setf final-candidate
					(list (car c) (car jww) (cdr jww)))))))))
;;; return first member of original candidates list if there was only one candidate
	  (car candidates))
;;; if there were no mom candidates use J & W on all propositions
      (progn
	(setf candidates (jw-candidates c-s))
;;; if there are any candidates
	(if candidates
;;; if there is more than one candidate
	    (if (cdr candidates)
;;; look for a candidate where...
		(let ((pos-cand
		       (find-if #'(lambda (c)
;;; positive literals are preferred and positive weight is greater than negative
				    (or (and *prefer-pos-lits* (> (second c) (third c)))
;;; or positive literals are nnot preferred and negative weight is greater than positive
					(and (not *prefer-pos-lits*) (> (second c) (third c)))))
				candidates)))
;;; if one was found return it, otherwise just return first candidate
		  (if pos-cand pos-cand (car candidates)))
;;; if there was only one candidate return it
	    (car candidates))
;;; otherwise its an error
	  (error "~&!!ERROR!! no candidate for SAT expansion~%"))))))

(defun max-clash (clash-list)
  (if clash-list (reduce #'max clash-list) 0))

(defun select-disjunction (c-s)
  (if (= 2 *moms-heuristic*)
      (let ((max-dep most-positive-fixnum) oldest)
	(dolist (dj (constraints-or-clauses c-s) oldest)
	  (when (plusp (first dj))
	    (let ((md (if (third dj) (max (third dj)) -1)))
	      (cond ((< md max-dep)
		     (setf oldest (second dj))
		     (setf max-dep md))
		    ((= md max-dep)
		     (dolist (c (second dj))
		       (unless (member c oldest) (setf oldest (cons c oldest))))))))))
    (second (car (member-if #'(lambda (c) (plusp (car c))) 
		    (constraints-or-clauses c-s))))))

(defun simple-moms-heuristic (c-s)
  (let* ((uxc (select-disjunction c-s))
	 (max-clause-len (cdr (clause-lengths c-s))) candidate (max-priority -1))
    (dolist (c uxc candidate)
	    (when (not (fourth (second c)))
		  (let* ((jww (prop-jw-weights (second c) max-clause-len))
			 (priority (alpha (car jww) (cdr jww))))
		    (when (> priority max-priority)
			  (setf max-priority priority)
			  (setf candidate (list (second c) (car jww) (cdr jww)))))))))

(defun add-ux-or-constraints (c-s)
  "Expand disjunction clauses by assigning truth values to their propositions"
;;; Chose a candidate proposition according to some heuristic
;;; IN-FaCT
  (goto-c-s c-s *or-level*)
  (let ((candidate (if (= 1 *moms-heuristic*) (full-moms-heuristic c-s) (simple-moms-heuristic c-s))))
;;; prop:=p-entry in constraints-props list
    (let ((prop (car candidate))
;;; split-lit:=litteral to add; add -ve first if pos-weight>neg-weight
	  (split-lit (if (> (second candidate) (third candidate))
			 (if *minimise-clashes* (caar candidate) (neg-con (caar candidate)))
		       (if *minimise-clashes* (neg-con (caar candidate)) (caar candidate)))))
;;; For debugging purpouses:-
      (setf *already-backtracking* nil)
      (dbg :split-lit "~A~A/~A " (if (pos-lit-p split-lit) "+" "-")
	   (pos-lit split-lit) *model-depth*)
;;; save the constraint system state
;;; IN-FaCT
      (push-state)
      (push-c-s c-s (incf *or-level*))
      (let* (
	     ;;(saved-cs (save-c-s c-s))
;;; res is true if adding split-lit to c-s is OK and the resulting c-s was satisfiable
;;; note that dependency level *or-level* is incremented
	     (res (and (extend-prop c-s prop split-lit (list *or-level*))
		       (b-c-p c-s)
		       (sat c-s))))
	(debug-if :backtrack
		  (and (not res) *backjumping* (not *already-backtracking*)
		       (< (max-clash *clash-level*) *or-level*))
		  "BT1 ~A->~A;" *or-level* (max-clash *clash-level*))
	(setf *already-backtracking* T)
;;; decrement dependency level
	(decf *or-level*)
	(pop-state)
;;; unless we already have a solution or we can backjump
	(if (or res (and *backjumping*
			 (not (member (1+ *or-level*) *clash-level*))))
	    res
	  (progn
;;; restore the constraint system state
	    (backto-c-s c-s *or-level*)
;;; IN-FaCT don't bother to save the dependency set from the first clash
	    (dbg :split-lit "<~A~A/~A "
		 (if (pos-lit-p split-lit) "-" "+")
		 (pos-lit split-lit)  *model-depth*)
	    (incf *search-space*)
;;; res is true if adding (not split-lit) to c-s is OK and the resulting c-s was satisfiable
	    (and (extend-prop c-s prop (neg-con split-lit)
			      (remove (1+ *or-level*) *clash-level*))
		 (b-c-p c-s)
		 (sat c-s))))))))


;;; ************** QUALIFIED NUMBER RESTRICTIONS **************

;;; Non-deterministically make explicit C or (not C) in R successors
(defun define-q-succ (c-s r-c-s c)
;;;  (format T "DQS+[~A]" c)
  (goto-c-s r-c-s *or-level*)
;;; For debugging purpouses:-
  (setf *already-backtracking* nil)
  (dbg :split-lit "[~A/~A/~A] " (if *def-q-succ-neg-first* (neg-con c) c) *or-level* *model-depth*)
;;; save the constraint system state
  (push-state)
  (push-c-s r-c-s (incf *or-level*))
  (push (list (if *def-q-succ-neg-first* (neg-con c) c) (list *or-level*)) 
	(constraints-c-ux r-c-s))
;;; res is true if adding (not c) to c-s is OK and the resulting c-s was 
;;; satisfiable. Note that dependency level *or-level* is incremented
  (let* ((res (sat c-s)))
    (debug-if :backtrack
	      (and (not res) *backjumping* (not *already-backtracking*)
		   (< (max-clash *clash-level*) *or-level*))
	      "BT2 ~A->~A;" *or-level* (max-clash *clash-level*))
    (setf *already-backtracking* T)
;;; decrement dependency level
    (decf *or-level*)
    (pop-state)
;;; if we already have a solution or we can backjump
    (if (or res (and *backjumping*
		     (not (member (1+ *or-level*) *clash-level*))))
;;; return res
	res
;;; otherwise explore other branch
      (progn
;;; restore the constraint system state
	(backto-c-s r-c-s *or-level*)
;;;  (format T "DQS-[~A]" c)
	(dbg :split-lit "~A/~A " (if *def-q-succ-neg-first* c (neg-con c)) 
	     *model-depth*)
	(incf *search-space*)
;;; res is true if adding c to c-s is OK and the resulting c-s was satisfiable
;;; dependecy set is the one returned from first branch minus *or-level*
	(push (list (if *def-q-succ-neg-first* c (neg-con c)) 
;;;		    (list *or-level*))
		    (remove (1+ *or-level*) *clash-level*))
	      (constraints-c-ux r-c-s))
	(sat c-s)))))

(defun add-ux (c-s c dep)
;;; add C to unexpanded list in c-s (if its not there already)
  (if (not (already-there c-s c))
;;; only do state save once in merge-nodes
;;;    (goto-c-s c-s *or-level*)
      (push (list c dep) (constraints-c-ux c-s))
    T))

(defun merge-nodes (c-s-1 c-s-2 dep)
  "Merge node c-s-2 into node c-s-1 adding dependencies dep"
  (let ((n1 (cdr c-s-1)) (n2 (cdr c-s-2)))
;;; following shouldn't be required because every concept in a node should
;;; already have the node's constraints-dep in its list
;;;	(setf dep (my-union (constraints-dep (cdr c-s-2)) dep))
    (if (and
;;; Merge list of roles; n2 must be a successor so "root" c-s is its parent node
;;; Note that this will also update the dependency list for the "edge"
	 (let ((c-s (cdr (constraints-parent n2)))
	       (new-dep (my-union dep (constraints-dep n2))))
	   (every #'(lambda (r)
		      (add-role c-s c-s-1 r new-dep))
		  (first c-s-2)))
	 (every #'(lambda (c)
		    (if (fourth c)
;;; Change to using add-ux because it is safer, particularly for parent
			(add-ux n1 (fourth c) (my-union (fifth c) dep))
;;;			(add-constraint n1 (fourth c)
;;;					(my-union (fifth c) dep))
		      T))
		(constraints-props n2))
	 (every #'(lambda (c)
		    (add-ux n1 (first c) (my-union (second c) dep)))
;;;		    (add-constraint n1 (first c)
;;;				    (my-union (second c) dep)))
		(constraints-c-def-ux n2))
	 (every #'(lambda (c)
		    (add-ux n1 (first c) (my-union (second c) dep)))
;;;		    (add-constraint n1 (first c)
;;;				    (my-union (second c) dep)))
		(constraints-c-ux n2)))
	(progn
;;; Merge neq lists - can use append because they must be disjoint
	  (setf (constraints-neq-list n1)
	    (append (constraints-neq-list n2) (constraints-neq-list n1)))
;;; Block 2nd node by seting role list to nil
	  (setf (first c-s-2) nil)
	  c-s-1)
      nil)))

(defun merge-part-elts (c-s p dep)
  "Merge list of nodes in p"
;;; Only need to do anything if p has >1 element
  (if (cdr p)
;;; Allways merge into parent if it is in p
      (let ((m-n (first (member-if #'(lambda (n)
				       (eq n (constraints-parent c-s))) p))))
;;; otherwise merge into first node in p
	(if (not m-n) (setf m-n (first p)))
;;; save state of m-n
	(goto-c-s (cdr m-n) *or-level*)
;;; Merge every other node in p into m-n
	(every #'(lambda (pe) (if (eq pe m-n) T (merge-nodes m-n pe dep))) p))
    T))

(defun merge-partition (c-s p dep)
  "Merge every partition list in p"
;;;  (goto-c-s c-s (1- *or-level*))
;;;  (push-c-s c-s *or-level*)
;;;  (format T "~&Try to merge~%")
  (every #'(lambda (p-e) (merge-part-elts c-s p-e dep)) p))

(defun k-partitions (s k)
  "Return list of k-partitions of set s (nil if |s| < k)"
  (let ((n (length s)))
    (cond
     ((< n k) nil)                      ; base case when n<k
     ((eql n k) `(,(mapcar #'list s)))  ; base case when n=k - note that (k-p nil 0) = (nil)
     ((zerop k) nil)                    ; special case when n>k and k=0
     ((eql k 1) `((,s)))                ; base case when k=1
     (T                                 ; recursive cases when n > k
      (let ((fs (first s)) (lfs (list (first s))))
	(nconc
	 (mapcar #'(lambda (p) (cons lfs p)) (k-partitions (rest s) (1- k)))
	 (mapcan #'(lambda (p)
		     (let (h)
		       (maplist #'(lambda (s1)
				    (setf h (cons (first s1) h))
				    (append (rest h) (cons (cons fs (first s1))
							   (rest s1))))
				p)))
		 (k-partitions (rest s) k))))))))

(defun not-neq (s)
  "True if set of (R).C-S elts s don't violate any neq constraints"
  (if (cdr s)
;;; neql set to list of neq constraints in 1st c-s
      (let ((neql (constraints-neq-list (cdr (first s)))))
	(and
;;; There is no (R).C-S elt in (cdr s) s.t.
	 (notany #'(lambda (e)
;;; If there is some neq constraint in neq-list of (R).C-S elt e that is in neql
		     (if (some #'(lambda (neq) (member neq neql))
			       (constraints-neq-list (cdr e)))
;;; then set clash level and return T
			 (progn
			   (setf *clash-level*
			     (my-union (constraints-dep (cdr (first s)))
				    (constraints-dep (cdr e))))
;;;			   (dbg :clash "Clash-neq[~A/~A]" *or-level* *clash-level*)
			   T)))
		 (cdr s))
;;; and also true for rest of s
	 (not-neq (cdr s))))
    T))

(defun mergable-partition (p dep)
  "True if merging partition p doesn't violate neq constraints"
  (if (every #'not-neq p) T
    (progn
      (setf *clash-level* (my-union *clash-level* dep))
      (dbg :clash "Clash-mergable[~A/~A]" *or-level* *clash-level*)
      nil)))

(defun try-to-merge (c-s p)
  (setf *already-backtracking* nil)
  (dbg :split-lit "{~A/~A} " *model-depth* *or-level*)
  (push-state)
  (incf *or-level*)
  (let ((res (and
	      (mergable-partition p (list *or-level*))
	      (progn
		(goto-c-s c-s (1- *or-level*))
		(push-c-s c-s *or-level*)
		(merge-partition c-s p (list *or-level*)))
	      (sat c-s))))
    (debug-if :backtrack
	      (and (not res) *backjumping* (not *already-backtracking*)
		   (< (max-clash *clash-level*) *or-level*))
	      "BT3 ~A->~A;" *or-level* (max-clash *clash-level*))
    (decf *or-level*)
    (pop-state)
    res))

(defun dep-q-succs (l c)
;;; get dependencies due to list of q-succs needing to be merged
;;; i.e., existence of each node (constraints-dep) and existence of c in label of each node
  (let (dep)
    (dolist (s l dep)
      (let ((c-s (cdr s)))
	(setf dep (my-union dep (constraints-dep c-s)))
;;; p-entry := proposition record in constraints-props
	(let ((p-entry (car (member (pos-lit c) (constraints-props c-s) :key #'first))))
;;; if there is a p-entry, then the dep-set is the fifth element
	  (if p-entry
	      (setf dep (my-union dep (fifth p-entry)))
;;; otherwise c must be in constraints-c-ux, and the dep set is the 2nd element
	    (setf dep (my-union dep 
				(second (first (member c (constraints-c-ux c-s) :key #'first)))))))))
;;;    (format T "DQS~S" dep)
    dep
    ))

(defun apply-atmost (c-s)
  (let* ((aam (applicable-atmost c-s))
	 (uqs (undef-q-succ c-s (second (first aam)) (third (first aam)))))
;;;  (format T "~&Apply at-most ~D ~A ~A~%" (first (first aam)) (second (first aam)) (third (first aam)))
;;; If there is some q-succ for which c from aam-constraint isn't defined
;;; then define it
    (if uqs (define-q-succ c-s (cdr uqs) (third (first aam)))
;;; Else we have to merge. q-succ-list is set to the list of all q successors.
;;; kp is set to list of possible partitionings
      (let ((q-succ-list (q-succs c-s (second (first aam)) (third (first aam)))))
	(let ((kp (k-partitions q-succ-list (first (first aam))))
	      (clashes '(nil nil)))
	  (or
;;; Either find a partition in (cdr kp) that is mergeable
	   (some #'(lambda (p)
		     (let ((res (try-to-merge c-s p)))
		       (debug-if :backtrack
				 (and (not res) *backjumping* (not *already-backtracking*)
				      (< (max-clash *clash-level*) *or-level*))
				 "BT4 ~A->~A;" *or-level* (max-clash *clash-level*))
		       (setf *already-backtracking* T)
		       (if res res
			 (if (and *backjumping*
				  (not (member (1+ *or-level*) *clash-level*)))
			     (return-from apply-atmost nil)
			   (progn
			     (push (remove (1+ *or-level*) *clash-level*) clashes)
			     (backto-c-s c-s *or-level*)
			     nil)))))
		 (cdr kp))
;;; Or merge first partition in kp
;;; dep is equal to union of aam dependencies and all other clashes and
;;; dependencies due to list of q-succs needing to be merged
	   (let ((dep (my-union 
		       (my-union (second aam) 
				 (reduce #'my-union clashes)) 
		       (dep-q-succs q-succ-list (third (first aam))))))
;;;  (format T "~&Deterministic at-most (dep = ~A)~%" dep)
	     (and (mergable-partition (first kp) dep)
		  (merge-partition c-s (first kp) dep)
		  (sat c-s)))))))))

(defun push-state ()
  (push (list *MAY-NEED-EXPANDING*) *global-state*))

(defun pop-state ()
  (let ((state (pop *global-state*)))
    (setf *MAY-NEED-EXPANDING* (first state))))
	 
(defun save-c-s (c-s)
  "Save the state of the constraint system"
  (let ((y (copy-constraints c-s)))
    (setf (constraints-props y)
      (cons (constraints-props y) (mapcar #'copy-list (constraints-props c-s))))
    (setf (constraints-or-clauses y)
      (cons (constraints-or-clauses y) (mapcar #'car (constraints-or-clauses c-s))))
    (setf (constraints-parent y) (copy-list (constraints-parent c-s)))
    (setf (constraints-children y)
      (cons (constraints-children y)
	    (mapcar #'copy-list  (constraints-children c-s))))
    y))

(defun restore-c-s (c-s saved-c-s)
  (setf (constraints-c c-s) (constraints-c saved-c-s))
  (setf (constraints-all-rc c-s) (constraints-all-rc saved-c-s))
  (setf (constraints-all-r+c c-s) (constraints-all-r+c saved-c-s))
  (setf (constraints-bcp-cand c-s) (constraints-bcp-cand saved-c-s))
;;;  (setf (constraints-some-fc-ux c-s) (constraints-some-fc-ux saved-c-s))
  (setf (constraints-some-rc-ux c-s) (constraints-some-rc-ux saved-c-s))
  (setf (constraints-atleast c-s) (constraints-atleast saved-c-s))
  (setf (constraints-atleast-ux c-s) (constraints-atleast-ux saved-c-s))
  (setf (constraints-atmost c-s) (constraints-atmost saved-c-s))
  (setf (constraints-c-def-ux c-s) (constraints-c-def-ux saved-c-s))
  (setf (constraints-c-ux c-s) (constraints-c-ux saved-c-s))
  (setf (constraints-props c-s) (car (constraints-props saved-c-s)))
  (map nil #'(lambda (p1 p2)
	       (setf (second p1) (second p2))
	       (setf (third p1) (third p2))
	       (setf (fourth p1) (fourth p2))
	       (setf (fifth p1) (fifth p2)))
       (constraints-props c-s) (cdr (constraints-props saved-c-s)))
  (setf (constraints-neq-list c-s) (constraints-neq-list saved-c-s))
  (setf (constraints-dep c-s) (constraints-dep saved-c-s))
  (setf (constraints-or-clauses c-s) (car (constraints-or-clauses saved-c-s)))
  (map nil #'(lambda (p1 p2)
	       (setf (first p1) p2))
       (constraints-or-clauses c-s) (cdr (constraints-or-clauses saved-c-s)))
  (setf (constraints-parent c-s) (constraints-parent saved-c-s))
  (setf (constraints-children c-s) (car (constraints-children saved-c-s)))
  (mapc #'(lambda (p1 p2)
	    (setf (first p1) (first p2))
	    (setf (cdr p1) (cdr p2)))
	(constraints-children c-s) (cdr (constraints-children saved-c-s)))
  (setf (constraints-individual c-s) (constraints-individual saved-c-s))
  (setf (constraints-some-rc c-s) (constraints-some-rc saved-c-s))
  )
;;;  (setf (constraints-children c-s) (constraints-children saved-c-s)))

;;; IN-FaCT
(defun push-c-s (c-s o-l)
   "Save the state of the constraint system"
  (let ((y (save-c-s c-s)))
    (setf (constraints-stack y) nil)
    (push y (constraints-stack c-s))
    (setf (constraints-or-level c-s) o-l)
    c-s))

;;; IN-FaCT
(defun backto-c-s (c-s o-l)
;;;  (break)
;;;  (format t "~A->~A:" (constraints-or-level c-s) o-l)
  (when (< o-l (constraints-or-level c-s))
    (setf (constraints-stack c-s)
      (member-if #'(lambda (c) (>= o-l (constraints-or-level c)))
		 (constraints-stack c-s)))
    (let ((saved-c-s (pop (constraints-stack c-s))))
      (restore-c-s c-s saved-c-s)
      (setf (constraints-or-level c-s) (constraints-or-level saved-c-s)))
    (if (constraints-parent c-s) (backto-c-s (cdr (constraints-parent c-s)) o-l))
    (mapc #'(lambda (c) (backto-c-s (cdr c) o-l)) (constraints-children c-s))))

;;; IN-FaCT
(defun goto-c-s (c-s o-l)
  (cond 
   ((= o-l (constraints-or-level c-s)) c-s)
   ((> o-l (constraints-or-level c-s))
    (push-c-s c-s o-l))
   (T
    (backto-c-s c-s o-l)
    (if (> o-l (constraints-or-level c-s))
	(push-c-s c-s o-l)))))
