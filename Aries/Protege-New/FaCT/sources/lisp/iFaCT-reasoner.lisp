;;; -*- Mode: Lisp; package: FACT; Syntax: COMMON-LISP; Base: 10 -*-

;;; FaCT and iFaCT COPYRIGHT (C) 1997, 1998, 1999 IAN R. HORROCKS
;;; and THE UNIVERSITY OF MANCHESTER, horrocks@cs.man.ac.uk
;;; Time-stamp: Tue Jun 15 17:41:50 BST 1999

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                      ;;;
;;; FaCT and iFaCT description logic classifiers                         ;;;
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
;;; Enquiries about FaCT and iFaCT should be directed to:                ;;;
;;;                                                                      ;;;
;;; email: horrocks@cs.man.ac.uk                                         ;;;
;;; www:   http://www.cs.man.ac.uk/~horrocks                             ;;;
;;; smail: Ian Horrocks, Department of Computer Science, Oxford Road,    ;;;
;;;       Manchester 467M13 9PL, United Kingdom                          ;;;
;;;                                                                      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;(in-package "USER")
(in-package "FACT")

(defconstant *reasoner* "iFaCT")
(defconstant *version-number* "1.63")

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
  (some-fc-ux nil)
  (some-rc-ux nil)
  (c-def-ux nil)
  (c-ux nil)
  (parent nil)
  (children nil)
  (or-level 0)
  (stack nil)
  (dep nil))

;;; ************** GLOBAL VARIABLES **************

;;; Variables controlling classifier features
(defparameter *inverse-roles* T
  "When T (NIL) enables (disables) inverse roles; default=T (for iFaCT)")
(defparameter *double-blocking* T
  "When T (NIL) enables (disables) double blocking; default=T (for iFaCT)")
(defparameter *subset-s-equivalent* nil
  "When T (NIL) enables (disables) enhanced subset blocking strategy; default=nil (for iFaCT)")
(defparameter *top-level-caching* T
  "When T (NIL) enables (disables) top-level caching in subsumption tests; default=T")
(defparameter *full-caching* nil
  "When T (NIL) enables (disables) caching in satisfiability tests; default=NIL (for iFaCT)")

;;; ************** SUBSUMPTION TESTING **************
    
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

(defun new-all-arc (r-c-s concept level)
  (let ((r (second concept)) (c (third concept)))
;;; if R in (all R C) is an ancestor of some S in list or roles connecting current node to c-s
    (if (some #'(lambda (s) (member r (r-ancestors s))) (car r-c-s))
	(let ((c-s (cdr r-c-s)))
;;; all C to unexpanded list in c-s (if its not there already)
	  (when (not (member c (constraints-c-ux c-s) :key #'first))
	    (goto-c-s c-s *or-level*)
	    (push (list c (union level (constraints-dep c-s)))
		  (constraints-c-ux c-s)))
;	  (pushnew (list c level) (constraints-c-ux c-s) :key #'first)
	  (if *transitivity*
;;; for every R1 in list of roles connecting current node to c-s
	      (mapc #'(lambda (r1)
;;; for every ancestor S of R1
			(mapc #'(lambda (s)
;;; if S is transitive and R in (all R C) is an ancestor of S
				  (if (and (r-trans-across s) (member r (r-ancestors s)))
;;; add encoded concept (all S C) to unexpanded list in c-s
;;; all C to unexpanded list in c-s (if its not there already)
				      (let ((new-c (neg-con (install-concept
							     (list :some s (neg-con c))))))
					(when (not (member new-c (constraints-c-ux c-s)
							   :key #'first))
					  (goto-c-s c-s *or-level*)
					  (push (list new-c (union level (constraints-dep c-s))) (constraints-c-ux c-s))))))
			      (r-ancestors r1)))
		    (car r-c-s)))))))
	
(defun new-all-rc (c-s concept level)
;;; add (all R C) constraint to c-s
  (push (list (cdr concept) level) (constraints-all-rc c-s))
;;; apply constraint to parent
  (new-all-arc (constraints-parent c-s) concept level)
;;; apply constraint to all children
  (mapc #'(lambda (r-c-s) (new-all-arc r-c-s concept level)) (constraints-children c-s))
  c-s)

(defun add-constraint (c-s concept level)
  "Add a new constraint (concept level) to S; return nil if there is a clash."
  (dbg :addc-entry "~&   ADD-C ~S ~S" concept level)
;;; IN-FaCT
  (goto-c-s c-s *or-level*)
  (cond
;;; if concept is an atom...
   ((atom concept)
    (add-literal c-s concept level))

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

;;; otherwise
   (T
;;;  its and ERROR - a bad concept expression
    (error-handler 'add-constraint :bad-concept-expansion))))

(defun add-xry (c-s r y-c-s level)
  "Add xry to x-r-y - check all-rc constraints and any partial expansions."
;;; build a new r-y constraint
  (let* ((r-a (r-ancestors r)))
    (and
;;; check if any (:all s c) constraints are triggered - s in ancestors of r.
     (every #'(lambda (s-c)
		(if (member (first (constraint s-c)) r-a)
		    (expand-all-rc y-c-s (first (constraint s-c)) (second (constraint s-c))
				   (union level (level s-c)) nil r-a)
		  t))
	    (constraints-all-rc c-s))
;;; check if any (:all s+ c) constraints are triggered - s in ancestors of r.
     (every #'(lambda (s-c)
		(if (member (first (constraint s-c)) r-a)
		    (expand-all-rc y-c-s (first (constraint s-c)) (second (constraint s-c))
				   (union level (level s-c)) t r-a)
		  t))
	    (constraints-all-r+c c-s)))))

(defun set-equal (s1 s2 &key (test #'eql))
  "returns T if the two sets s1 and s2 are equal"
  (and
   (= (length s1) (length s2))
   (every #'(lambda (e) (member (first e) s2 :test test :key #'first)) s1)))

;;; IN-FaCT - NOTE could make blocking much more sophisticated
(defun s-equivalent (x-c y-c)
  "True if x and y are s-equivalent"
  (if *subset-s-equivalent*
      (and (subsetp (constraints-c x-c) (constraints-c y-c))
	   (subsetp (constraints-all-r+c x-c) (constraints-all-r+c y-c)
		    :test #'equal :key #'first)
	   (setf *cycle* T))
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

(defun find-s-equivalent (c-s)
  "Finds and returns an s-equivalent variable to x if one exists"
;;; can switch off if blocking is not required - e.g. for alc
  (when *blocking*
    (let ((s-equiv
	   (or
	    (do ((y-c-s (cdr (constraints-parent c-s)) (cdr (constraints-parent y-c-s))))
		((or (null y-c-s) (s-equivalent c-s y-c-s)) y-c-s))
	    (and (cdr (constraints-parent c-s))
		 (find-s-equivalent (cdr (constraints-parent c-s)))))))
      (when s-equiv
	(dbg :sat-sequiv "~&     S-equivalent ~S = ~S" c-s s-equiv))
      s-equiv)))

(defun not-fully-solved (c-s)
  (some #'(lambda (c) (plusp (car c))) (constraints-or-clauses c-s)))

(defun not-expanded (c-s)
  "returns T if the constraint system c-s is not expanded"
;;; return logical or of the unexpanded constraint lists
  (or 
   (constraints-some-fc-ux c-s)
   (constraints-some-rc-ux c-s)
   (constraints-c-def-ux c-s)
   (constraints-c-ux c-s)
   (not-fully-solved c-s)))

(defun needs-expanding (c-s)
  (and (not-expanded c-s)
       (not (find-s-equivalent c-s))
       (not (mergable-constraints c-s))))

(defun sorted-successors (s-list)
  (if *sort-lists*
      (stable-sort (reverse s-list)
	    #'(lambda (x y)
		(< (if (cdr x) (reduce #'max (cdr x)) -1)
		   (if (cdr y) (reduce #'max (cdr y)) -1))))
    (reverse s-list)))

;;; IN-FaCT - new incremental style expand-f-roles
(defun increment-f-role (c-s r-c-s r c d-set)
  (let ((y-cs (cdr r-c-s)))
;;; if R not already an ancestor of some r-p in parent roles
    (if (not (some #'(lambda (r-p) (member r (r-ancestors r-p)))
		   (car r-c-s)))
;;; then add it to list
	(push r (car r-c-s)))
;;; if C already satisfied in y-cs then just return y-cs
    (if (member c (constraints-c y-cs))
	T
;;; otherwise add c to y-cs
      (and (add-constraint y-cs c d-set)
	   (add-xry c-s r y-cs d-set)
;;;	   (if (needs-expanding y-cs)
	       (sat y-cs)
;;;	     T)
	   ))))

(defun expand-f-roles (c-s)
  "Expand (:some f C) constraints on c-s; return nil if there was a clash"
;;; IN-FaCT
  (goto-c-s c-s *or-level*)
;;; loop through rc-ux list until empty - reverse it so as to process oldest first
  (let ((s-s (sorted-successors (constraints-some-fc-ux c-s))))
    (setf (constraints-some-fc-ux c-s) nil)
    (dolist (r-c s-s c-s)
      (let ((r (caar r-c)) (c (cadar r-c)))
;;; if for some r-p from parent roles
	(if (some #'(lambda (r-p)
;;; either R from (some R C) is ancestor of r-p or r-p is an ancestor of R
		      (or (member r (r-ancestors r-p)) (member r-p (r-ancestors r))))
		  (car (constraints-parent c-s)))
;;; then try to MERGE WITH PARENT
	    (unless (increment-f-role c-s (constraints-parent c-s) r c (cdr r-c))
	      (return-from expand-f-roles nil))
;;; otherwise try to find some child such that...
	  (let ((f-y (car (member-if #'(lambda (s)
;;; for some r-s from one of successor roles
					 (some #'(lambda (r-s)
;;; either R from (some R C) is ancestor of r-s or r-s is an ancestor of R
						   (or (member r (r-ancestors r-s))
						       (member r-s (r-ancestors r))))
					       (car s)))
				     (constraints-children c-s)))))
;;; if a suitable child was found
	    (if f-y
;;; then try to MERGE WITH CHILD
		(unless (increment-f-role c-s f-y r c (cdr r-c))
		  (return-from expand-f-roles nil))
;;; OTHERWISE try to make NEW CHILD ROLE
	      (let ((y-cs (make-constraints :parent (cons (list (r-inverse-f r)) c-s)
					    :or-level *or-level*
					    :dep (cdr r-c)))
		    (d-set (cdr r-c)))
;;; increment model-size and set max size
		(when (> (incf *model-size*) *max-model-size*)
		  (setf *max-model-size* *model-size*))
;;; unless constraining concept is added ok, return nil - clash
		(unless (and (add-constraint y-cs c d-set)
;;; add the universal constraint due to GCIs
			     (if *universal-constraint*
				 (add-constraint y-cs *universal-constraint* nil)
			       T)
			     (add-xry c-s r y-cs d-set))
		  (return-from expand-f-roles nil))
;;; when y-cs needs expanding
;;;		(when (needs-expanding y-cs)
;;; unless its expanded ok return nil
		  (unless (sat y-cs)
		    (return-from expand-f-roles nil))
;;;		  )
;;; add r-cs to list
		(push (cons (list r) y-cs) (constraints-children c-s))))))))))

;;; IN-FaCT - new incremental style expand-r-roles
(defun expand-r-roles (c-s)
  "Expand (:some R C) constraints on c-s; return nil if there was a clash"
;;; IN-FaCT
  (goto-c-s c-s *or-level*)
;;; loop through rc-ux list until empty - reverse it so as to process oldest first
  (let ((s-s (sorted-successors (constraints-some-rc-ux c-s))))
    (setf (constraints-some-rc-ux c-s) nil)
    (dolist (r-c s-s c-s)
      (let ((r (caar r-c)) (c (cadar r-c)))
	(when (and
;;; r-c not satisfied by any successor
	       (notany #'(lambda (r-cs)
			   (and 
			    (some #'(lambda (rc) (member r (r-ancestors rc))) (car r-cs))
			    (member c (constraints-c (cdr r-cs)))))
		       (constraints-children c-s))
;;; IN-FaCT: and r-c not satisfied by parent
	       (not
		(and
		 (some #'(lambda (r-p)
			   (member r (r-ancestors r-p)))
		       (car (constraints-parent c-s)))
		 (member c (constraints-c (cdr (constraints-parent c-s)))))))
;;; IN-FaCT: make parent ((R^-1) . C-S)
	  (let ((y-cs (make-constraints :parent (cons (list (r-inverse-f r)) c-s)
					:or-level *or-level*
					:dep (cdr r-c)))
		(d-set (cdr r-c)))
;;; increment model-size and set max size
	    (when (> (incf *model-size*) *max-model-size*)
	      (setf *max-model-size* *model-size*))
;;; unless constraining concept is added ok, return nil - clash
	    (unless (and (add-constraint y-cs c d-set)
;;; add the universal constraint due to GCIs
			 (if *universal-constraint*
			     (add-constraint y-cs *universal-constraint* nil)
			   T)
			 (add-xry c-s r y-cs d-set))
	      (return-from expand-r-roles nil))
;;; when y-cs needs expanding
;;;	    (when (needs-expanding y-cs)
;;; unless its expanded ok return nil
	      (unless (sat y-cs)
		(return-from expand-r-roles nil))
;;;	      )
;;; add r-cs to list
	    (push (cons (list r) y-cs) (constraints-children c-s))))))))

;;; IN-FaCT
(defun re-check-sat (c-s)
  (if (and (sat c-s)
	   (every #'(lambda (r-y) (sat (cdr r-y))) (constraints-children c-s)))
      c-s))

(defun check-child-sat (c-s)
  (every #'(lambda (r-y)
	     (if (needs-expanding (cdr r-y))
		 (sat (cdr r-y))
	       (cdr r-y)))
	 (constraints-children c-s)))

(defun sat (c-s)
  "Returns T if constraint system is satisfiable, nil otherwise"
  (unless (and (expand-ux-concepts c-s)
	       (expand-defined-concepts c-s)
;;;	       (check-child-sat c-s)
	       )
    (return-from sat nil))
  (if (and (constraints-parent c-s)
	   (constraints-c-ux (cdr (constraints-parent c-s))))
      (sat (cdr (constraints-parent c-s)))
;;; IN-FaCT test blocking after expanding ux and defined concepts
  (if (needs-expanding c-s)
      (if (not-fully-solved c-s)
	  (add-ux-or-constraints c-s)
;;; preserve model depth and size
	(progv
	    '(*model-depth* *model-size*) `(,*model-depth* ,*model-size*)
	  (when (and (or (constraints-some-rc-ux c-s) (constraints-some-fc-ux c-s))
		     (> (incf *model-depth*) *max-model-depth*))
	    (setf *max-model-depth* *model-depth*))
	  (and 
;;;(every #'(lambda (r-y) (sat (cdr r-y))) (constraints-children c-s))
	       (expand-f-roles c-s)
	       (expand-r-roles c-s)
	       (if (constraints-c-ux c-s) (sat c-s) c-s))))
    c-s))
;;;	   (if (needs-expanding c-s) (re-check-sat c-s) c-s)))))

(defun full-sat (c-s)
  (and
;;; add the universal constraint due to GCIs
   (if *universal-constraint*
	 (add-constraint c-s *universal-constraint* nil)
     T)
   (sat c-s)))

(defun simple-sat (c-s)
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
    (progv '(*or-level* *model-size* *max-model-size*
			 *model-depth* *max-model-depth*
			 *start-time* *cycle*)
	   `(0 1 1 1 1 ,(get-internal-run-time) nil)
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
  (let ((c-s (test-sat c)) f-succ)
    (decf *tsd*)
;;; if c is satisfiable
    (if c-s
;;;  keep a note of relevant parts of top node of model
	(setf (c-model c) (make-constraints
;;; keep a note of all x:C constraints
			   :c (constraints-c c-s)
;;; keep a note of all Rs in all R.C constraints
			   :all-rc (delete-duplicates (mapcar #'caar (constraints-all-rc c-s)))
;;; IN-FaCT get R and F successors from children list
;;; keep a note of all Rs in xRy constraints
			   :r-y (delete-duplicates
				 (mapcan #'(lambda (r-cs)
					     (dolist (f (car r-cs) (car r-cs))
					       (if (r-functional f) (pushnew f f-succ))))
					 (if (constraints-parent c-s)
					     (cons (constraints-parent c-s)
						   (constraints-children c-s))
					   (constraints-children c-s))))
;;; keep a note of all fs in xfy constraints
			   :f-y f-succ))
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
	(setf (c-definition c) *BOTTOM*)
	(setf (c-primitive c) nil)
	(when (c-grail-name (neg-con c))
	      (setf (system-name (c-grail-name (neg-con c))) *TOP*))
;;; If there is a  *universal-constraint* we have to be careful not to clear its
;;; definition when we discover that (not *universal-constraint*) = *BOTTOM*
	     (unless (eql (neg-con c) *universal-constraint*)
		     (setf (c-synonym (neg-con c)) *TOP*)
		     (setf (c-asserted-supers c) nil)
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
     (notany-neg-member (constraints-c mc1) (constraints-c mc2))
     (every #'(lambda (r)
		(notany-member (r-ancestors r) (constraints-all-rc mc2)))
	    (constraints-r-y mc1))
     (every #'(lambda (r)
		(notany-member (r-ancestors r) (constraints-all-rc mc1)))
	    (constraints-r-y mc2))
     (notany-member (constraints-f-y mc1) (constraints-f-y mc2)))))

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
	    (setf d-set (union d-set (fifth (second lit)))))))

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
	  (setf *clash-level* (union d-set (dependencies lit-list)))
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
	(setf *clash-level* d-set)
	(return-from extend-prop nil))
;;; add lit to constraints-c list for use in blocking etc
  (push lit (constraints-c c-s))
;;; if lit is a synonym
  (if (/= lit (c-synonym lit))
;;;   unless synonym literal is added OK
      (unless (add-literal c-s (c-synonym lit) d-set)
;;;     bomb out returning nil (clash)
	      (return-from extend-prop nil))
;;;   otherwise get the literals definition
    (let ((c-def (c-definition lit)))
;;;   and when it has a definition add it to some-rc-ux or c-def-ux for future expansion
      (when c-def
	    (if (and (listp c-def) (eq (car c-def) :some))
		(if (r-functional (second c-def))
		    (push (cons (cdr c-def) d-set) (constraints-some-fc-ux c-s))
		  (push (cons (cdr c-def) d-set) (constraints-some-rc-ux c-s)))
	      (push (list lit d-set) (constraints-c-def-ux c-s))))))
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
		      (setf *clash-level* (union (third clause) (dependencies (second clause))))
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
		      (setf *clash-level* (union (third clause) (dependencies (second clause))))
;;; return nil
		      (return-from extend-prop nil))
;;; if open literals count is one, clause is a candidate for BCP
		(when (eql (car clause) 1)
		      (push clause (constraints-bcp-cand c-s))))
;;; for each clause in neg-lits list, set open literals count to -1 = satisfied
	(dolist (clause (third p-entry))
		(setf (car clause) -1))))
;;; return T - there was no clash
  T)

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
		(setf *clash-level* (union d-set (fifth p-entry)))
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
				  (union (third candidate) (dependencies (second candidate))))
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
		  "BT ~A->~A;" *or-level* (max-clash *clash-level*))
	(setf *already-backtracking* T)
;;; decrement dependency level
	(decf *or-level*)
;;; if we already have a solution or we can backjump
	(if (or res (and *backjumping*
			 (not (member (1+ *or-level*) *clash-level*))))
;;; return res
	    res
	  (progn
;;; restore the constraint system state
	    (backto-c-s c-s *or-level*)
	    (dbg :split-lit "<~A~A/~A "
		 (if (pos-lit-p split-lit) "-" "+")
		 (pos-lit split-lit)  *model-depth*)
	    (incf *search-space*)
;;; res is true if adding (not split-lit) to c-s is OK and the resulting 
;;; c-s was satisfiable
	    (and (extend-prop c-s prop (neg-con split-lit)
			      (remove (1+ *or-level*) *clash-level*))
		 (b-c-p c-s)
		 (sat c-s))))))))

(defun save-c-s (c-s)
  "Save the state of the constraint system"
  (let ((y (copy-constraints c-s)))
    (setf (constraints-props y)
      (cons (constraints-props y) (mapcar #'copy-list (constraints-props c-s))))
    (setf (constraints-or-clauses y)
      (cons (constraints-or-clauses y) (mapcar #'car (constraints-or-clauses c-s))))
    (setf (constraints-parent y) (copy-list (constraints-parent c-s)))
    (setf (constraints-children y)
      (mapcar #'copy-list  (constraints-children c-s)))
    y))

(defun restore-c-s (c-s saved-c-s)
  (setf (constraints-c c-s) (constraints-c saved-c-s))
  (setf (constraints-all-rc c-s) (constraints-all-rc saved-c-s))
  (setf (constraints-all-r+c c-s) (constraints-all-r+c saved-c-s))
  (setf (constraints-bcp-cand c-s) (constraints-bcp-cand saved-c-s))
  (setf (constraints-some-fc-ux c-s) (constraints-some-fc-ux saved-c-s))
  (setf (constraints-some-rc-ux c-s) (constraints-some-rc-ux saved-c-s))
  (setf (constraints-c-def-ux c-s) (constraints-c-def-ux saved-c-s))
  (setf (constraints-c-ux c-s) (constraints-c-ux saved-c-s))
  (setf (constraints-props c-s) (car (constraints-props saved-c-s)))
  (map nil #'(lambda (p1 p2)
	       (setf (second p1) (second p2))
	       (setf (third p1) (third p2))
	       (setf (fourth p1) (fourth p2))
	       (setf (fifth p1) (fifth p2)))
       (constraints-props c-s) (cdr (constraints-props saved-c-s)))
  (setf (constraints-or-clauses c-s) (car (constraints-or-clauses saved-c-s)))
  (map nil #'(lambda (p1 p2)
	       (setf (first p1) p2))
       (constraints-or-clauses c-s) (cdr (constraints-or-clauses saved-c-s)))
  (setf (constraints-parent c-s) (constraints-parent saved-c-s))
  (setf (constraints-children c-s) (constraints-children saved-c-s)))

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
