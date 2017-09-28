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

(export
 '(defprimrole implies_r equal_r transitive functional
   classify-tkb init-tkb
   all_subs_r direct_subs_r all_supers_r direct_supers_r
   defprimconcept implies_c equal_c
   satisfiable_c subsumes_c equivalent_c
   all_subs_c direct_subs_c all_supers_c direct_supers_c taxonomy_position
   defconcept defprimattribute implies))

;;;(defconstant *CORBA-version-number* "1.1")
(defparameter *symbol-concept-names* T
  "When T (NIL) all c_names are of type symbol (grail-concept)")
(setf *auto-install-primitives* T)

;;; ***************** CORBA INTERFACE ****************

(defun implies_r-f (r s)
  "Add assertion r -> s"
  (let ((r-t (translate-role r)) (s-t (translate-role s)))
    (r-check-installed r-t)
    (pushnew s-t (r-parents r-t)))
  (values))

(defun equal_r-f (r s)
  "Add assertion r <-> s"
  (implies_r-f r s)
  (implies_r-f s r))

(defun transitive-f (r)
  (let ((r-t (translate-role r)))
    (r-check-installed r-t)
    (setf *transitive-roles* T)
    (if (r-used-in-nr r)
	(verbosity 
	 :warnings
	 "~&!!WARNING!! TRANSITIVE ROLE ~A USED IN AT-LEAST/MOST CONCEPT~%"
	 r))
    (setf (r-trans-across r-t) (list r-t))
    (setf (r-trans-across (r-inverse r-t)) (list (r-inverse r-t)))
    (setf (r-processed r-t) nil)
    (setf (r-processed (r-inverse r-t)) nil))
  (values))

(defun functional-f (r)
  (let ((r-t (translate-role r)))
    (r-check-installed r-t)
    (setf (r-functional r-t) T)
    (setf (r-processed r-t) nil))
  (values))

(defun eq-partition (r-set eq-f)
  (let (p-set)
    (dolist (r r-set p-set)
      (if (not (some #'(lambda (s) (member r s)) p-set))
	  (push (funcall eq-f r) p-set)))))

(defun r-inv-out (r-set)
  (mapcar #'(lambda (l) (mapcar #'untranslate-role l)) r-set))
    
(defun get_all_subs_r-f (r)
  (let ((r-t (translate-role r)))
    (r-check-installed r-t)
    (let (subs)
      (dolist (s *r-definitions* 
		(delete-if #'(lambda (r-set) (member r-t r-set))
			   (eq-partition subs #'r-get-equivalents)))
	(if (member r-t (r-ancestors s)) (pushnew s subs))
	(if (member r-t (r-ancestors (r-inverse s))) 
	    (pushnew (r-inverse s) subs))))))
    
(defun all_subs_r-f (r)
  (r-inv-out (get_all_subs_r-f r)))

(defun direct_subs_r-f (r)
  (let ((r-t (translate-role r)))
    (r-check-installed r-t)
    (let ((a-s-r (get_all_subs_r-f r-t)))
      (r-inv-out
       (mapcan #'(lambda (r-set)
		   (if (not (some #'(lambda (s-set)
				      (and (not (eq r-set s-set))
					   (member (car s-set) 
						   (r-ancestors (car r-set)))))
				  a-s-r))
		       (list r-set)))
	       a-s-r)))))

(defun get_all_supers_r-f (r)
  (let ((r-t (translate-role r)))
    (r-check-installed r-t)
    (delete-if #'(lambda (r-set) (member r-t r-set))
	       (eq-partition (r-ancestors r-t) #'r-get-equivalents))))

(defun all_supers_r-f (r)
  (r-inv-out (get_all_supers_r-f r)))

(defun direct_supers_r-f (r)
  (let ((r-t (translate-role r)))
    (r-check-installed r-t)
  (let ((a-s-r (get_all_supers_r-f r-t)))
    (r-inv-out
     (mapcan #'(lambda (r-set)
		 (if (not (some #'(lambda (s-set)
				    (and (not (eq r-set s-set))
					 (member (car r-set) 
						 (r-ancestors (car s-set)))))
				a-s-r))
		     (list r-set)))
	     a-s-r)))))

;;; NOTE full functionality not required but gives compatibility with basic FaCT KBs
(defun defprimconcept-f (n &optional d)
  (if d (grail-define-primconcept n (translate-concept d))
;;; adding the following slows things down considerably because absorption works 
;;; less well, but without it n may not be classified in hierarchy
    (if (not (g-concept n)) (grail-define-primconcept n))
    ))

(defun implies_c-f (c1 c2)
  (grail-define-implication (translate-concept c1) (translate-concept c2))
  (values))

(defun equal_c-f (c1 c2)
  (if (and (atom c1) (not (g-concept c1)))
      (grail-define-concept c1 (translate-concept c2))
    (progn
      (implies_c-f c1 c2)
      (implies_c-f c2 c1))))

(defun satisfiable_c-f (c)
  (when (test-sat (encode-concept-term (translate-concept c))) T))

(defun subsumes_c-f (c1 c2)
  (if 
      (c-subsumes c1 c2)
;;;      (not (test-sat (encode-concept-term 
;;;		      (translate-concept `(and ,c2 (not ,c1)))))) 
    T))

(defun equivalent_c-f (c1 c2)
  (and (subsumes_c-f c1 c2) (subsumes_c-f c2 c1)))

(defun c-eq-partition (cnames)
  (eq-partition cnames #'(lambda (c1) (cons c1 (equivalences c1)))))

(defun exclusive-eq-partition (c cnames)
    (delete-if #'(lambda (c-set) (member c c-set))
	       (c-eq-partition cnames)))

(defun symbol-concept-names (l)
  (if *symbol-concept-names*
      (if (listp l) (mapcar #'symbol-concept-names l) (grail-concept-name l))
    l))

(defun all_supers_c-f (c)
  (symbol-concept-names (exclusive-eq-partition c (c-all-supers c))))

(defun direct_supers_c-f (c)
  (symbol-concept-names (exclusive-eq-partition c (c-direct-supers c))))

(defun all_subs_c-f (c)
  (symbol-concept-names (exclusive-eq-partition c (c-all-subs c))))

(defun direct_subs_c-f (c)
  (symbol-concept-names (exclusive-eq-partition c (c-direct-subs c))))

(defun taxonomy_position-f (c)
  (multiple-value-bind (supers subs equivs) (c-taxonomy-position c)
    (symbol-concept-names (list (c-eq-partition supers)
				(c-eq-partition subs)
				(if (g-concept c)
				    (pushnew (g-concept c) equivs)
				  equivs)))))

(defun classify-tkb ()
  (progv '(*verbosity*) '(nil)
    (classify-all-concepts))
  (values))

(defun init-tkb ()
  (clear-kb)
  (values))


;;; ************** LISP COMPATIBILITY **************
;;; functions and macros that allow lisp style FaCT KBs to be loaded

;;; OLD VERSION
;;;;;; NOTE full functionality only required for compatibility with basic FaCT KBs
;;;(defun defprimrole-f (r &key (parents nil) (supers nil) 
;;;			     (transitive nil) (i-functional nil))
;;;  (let ((i-r (if *inverse-roles* (inv-r r) r)))
;;;    (setf parents (union parents supers))
;;;    (when (and parents (not (listp parents)))
;;;      (error "~&!!ERROR!! BAD :PARENTS LIST - ~S~%" parents))
;;;    (grail-define-relation r i-r
;;;			   parents nil (and *inverse-roles* i-functional))
;;;    (when (eq transitive T)
;;;      (grail-redefine-relation r :transitive-across `(,r))
;;;      (when *inverse-roles* 
;;;	(grail-redefine-relation i-r :transitive-across `(,i-r))))
;;;    (car (multiple-value-list (r-defined r)))))
;;;;;; Minimal version
;;;;;; (defun defprimrole-f (r) (r-check-installed r))

(defun defprimrole-f (r &key (parents nil) (supers nil) (transitive nil) (i-functional nil))
  (r-check-installed (translate-role r))
  (if (listp parents)
      (dolist (p parents) (implies_r-f r p))
    (error "~&!!ERROR!! BAD :PARENTS LIST - ~S~%" parents))
  (if (listp supers)
      (dolist (p supers) (implies_r-f r p))
    (error "~&!!ERROR!! BAD :SUPERS LIST - ~S~%" supers))
  (when (eq transitive T) (transitive-f r))
  (when (eq i-functional T) (functional-f (list :inv r))))

(defun defprimattribute-f (r &key (parents nil) (supers nil) (i-functional nil))
  (r-check-installed (translate-role r))
  (functional-f r)
  (if (listp parents)
      (dolist (p parents) (implies_r-f r p))
    (error "~&!!ERROR!! BAD :PARENTS LIST - ~S~%" parents))
  (if (listp supers)
      (dolist (p supers) (implies_r-f r p))
    (error "~&!!ERROR!! BAD :SUPERS LIST - ~S~%" supers))
  (when (eq i-functional T) (functional-f (list :inv r))))

(defmacro implies (c1 c2) `(implies_c-f ',c1 ',c2))
(defmacro defprimrole (r &key (parents nil) (supers nil) (transitive nil) 
			 (i-functional nil))
  `(defprimrole-f ',r :parents ',parents :supers ',supers :transitive ,transitive
     :i-functional ,i-functional))
;;; Minimal version
;;;(defmacro defprimrole (r) `(defprimrole-f ',r)
(defmacro defprimattribute (r &key (parents nil) (supers nil) (i-functional nil))
  `(defprimattribute-f ',r :parents ',parents :supers ',supers 
     :i-functional ,i-functional))
(defmacro defconcept (n d)
  `(grail-define-concept ',n ',(translate-concept d)))


;;; ************** MACRO INTERFACE **************

(defmacro implies_r (r s) `(implies_r-f ',r ',s))
(defmacro equal_r (r s) `(equal_r-f ',r ',s))
(defmacro transitive (r) `(transitive-f ',r))
(defmacro functional (r) `(functional-f ',r))

(defmacro all_subs_r (r) `(all_subs_r-f ',r))
(defmacro direct_subs_r (r) `(direct_subs_r-f ',r))
(defmacro all_supers_r (r) `(all_supers_r-f ',r))
(defmacro direct_supers_r (r) `(direct_supers_r-f ',r))

(defmacro defprimconcept (n &optional d) `(defprimconcept-f ',n ',d))
(defmacro implies_c (c1 c2) `(implies_c-f ',c1 ',c2))
(defmacro equal_c (c1 c2) `(equal_c-f ',c1 ',c2))

(defmacro satisfiable_c (c) `(satisfiable_c-f ',c))
(defmacro subsumes_c (c1 c2) `(subsumes_c-f ',c1 ',c2))
(defmacro equivalent_c (c1 c2) `(equivalent_c-f ',c1 ',c2))

(defmacro all_subs_c (c) `(all_subs_c-f ',c))
(defmacro direct_subs_c (c) `(direct_subs_c-f ',c))
(defmacro all_supers_c (c) `(all_supers_c-f ',c))
(defmacro direct_supers_c (c) `(direct_supers_c-f ',c))
(defmacro taxonomy_position (c) `(taxonomy_position-f ',c))
