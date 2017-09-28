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
 '(alc-concept-coherent alc-test
   defconcept-f defconcept defprimconcept-f defprimconcept
   defprimrole-f defprimrole
   defprimattribute-f defprimattribute implies-f implies disjoint-f disjoint
   load-tkb classify-tkb init-tkb
   direct-supers all-supers direct-subs all-subs
   equivalences satisfiable subsumes 
   equivalent-concepts disjoint-concepts 
   add-concept-f add-concept classify-concept 
   get-concept get-role get-all-concepts get-all-roles classified-tkb? 
   what-is? is-primitive? is-concept? is-role? is-feature? name description
   define-concept-f define-concept define-primitive-concept-f
   define-primitive-concept define-primitive-role-f define-primitive-role
   define-primitive-attribute-f define-primitive-attribute))

;;; ************** KRIS INTERFACE **************

(defun defconcept-f (n d)
  (grail-define-concept n (translate-concept d)))

(defmacro defconcept (n d)
  `(grail-define-concept ',n ',(translate-concept d)))

(defun defprimconcept-f (n &optional d)
  (grail-define-primconcept n (when d (translate-concept d))))

(defmacro defprimconcept (n &optional d)
  `(grail-define-primconcept ',n ',(when d (translate-concept d))))

(defun defprimrole-f (r &key (parents nil) (supers nil) (transitive nil) (i-functional nil))
  (setf r (translate-role r))
  (let ((i-r (if *inverse-roles* (inv-r r) r)))
    (setf parents (union parents supers))
    (when (and parents (not (listp parents)))
      (error "~&!!ERROR!! BAD :PARENTS LIST - ~S~%" parents))
    (setf parents (mapcar #'translate-role parents))
    (grail-define-relation r i-r
			   parents nil (and *inverse-roles* i-functional))
    (when (eq transitive T)
      (grail-redefine-relation r :transitive-across `(,r))
      (when *inverse-roles* (grail-redefine-relation i-r :transitive-across `(,i-r))))
    (car (multiple-value-list (r-defined r)))))

(defmacro defprimrole (r &key (parents nil) (supers nil) (transitive nil) (i-functional nil))
  `(defprimrole-f ',r :parents ',parents :supers ',supers :transitive ,transitive
		  :i-functional ,i-functional))

(defun defprimattribute-f (r &key (parents nil) (supers nil) (i-functional nil))
  (setf r (translate-role r))
  (setf parents (union parents supers))
  (when (and parents (not (listp parents)))
	(error "~&!!ERROR!! BAD :PARENTS LIST - ~S~%" parents))
  (setf parents (mapcar #'translate-role parents))
  (grail-define-relation r (if *inverse-roles* (inv-r r) r)
;			 (intern (format nil "I*~A" r))
			 parents T (and *inverse-roles* i-functional)))

(defmacro defprimattribute (r &key (parents nil) (supers nil) (i-functional nil))
  `(defprimattribute-f ',r :parents ',parents :supers ',supers :i-functional ,i-functional))

(defun implies-f (a c)
  (grail-define-implication (translate-concept a) (translate-concept c))
  (kris-concept a))

(defmacro implies (a c)
  `(implies-f ',a ',c))

(defun disjoint-f (&rest c-list)
  (do ((d-list (mapcar #'translate-concept c-list) (cdr d-list)))
      ((endp (cdr d-list)))
      (grail-define-implication (first d-list) (list :not (cons :or (cdr d-list))))))

(defmacro disjoint (&rest c-list)
  `(apply #'disjoint-f ',c-list))

(defun load-tkb (filename &key (verbose t) (overwrite nil))
  (progv '(*verbosity*) '((:warnings))
	 (when verbose (set-verbosity :classify-1))
	 (when overwrite (clear-kb))
	 (let ((f (probe-file filename)))
	   (if f
	       (progn
		 (push (namestring f) *kb-file-names*)
		 (load f))
	     (progn
	       (verbosity :warnings "~&!!WARNING!! file not found - ~S~%" filename)
	       nil)))))

(defun classify-tkb (&key (mode :nothing))
  (progv '(*verbosity*) '((:warnings :synonyms))
	 (case mode
	       (:stars (set-verbosity :classify-1))
	       (:names (set-verbosity :classify-2))
	       (:count (set-verbosity :test-counts))
	       (T))
	 (classify-all-concepts)))

(defun init-tkb ()
  (clear-kb))

(defun direct-supers (concept)
  (auto-classify)
  (c-direct-supers concept))

(defun all-supers (concept)
  (auto-classify)
  (c-all-supers concept))

(defun direct-subs (concept)
  (auto-classify)
  (c-direct-subs concept))

(defun all-subs (concept)
  (auto-classify)
  (c-all-subs concept))

(defun satisfiable (concept)
;;;  (auto-classify)
  (progv '(*auto-install-primitives*) '(T)
    (when (auto-pre-process 
	   `(test-sat (encode-concept-term (translate-concept ',concept))))
      T)))

(defun subsumes (c1 c2)
  (progv '(*auto-install-primitives*) '(T)
    (if
	(c-subsumes c1 c2)
;;;	(not (auto-pre-process
;;;	      `(test-sat (encode-concept-term 
;;;			  (translate-concept '(and ,c2 (not ,c1))))))) 
	T)))

(defun equivalent-concepts (c1 c2)
  (and (subsumes c1 c2) (subsumes c2 c1)))

(defun disjoint-concepts (c1 c2)
  (not (satisfiable `(and ,c1 ,c2))))

(defun add-concept-f (c-name definition &key (primitive nil))
  (if (eq primitive t)
      (defprimconcept-f c-name definition)
    (defconcept-f c-name definition))
  (classify-tkb)
  (car (multiple-value-list (g-concept c-name))))

(defmacro add-concept (c-name definition &key (primitive nil))
  `(add-concept-f ',c-name ',definition :primitive ,primitive))

(defun classify-concept (concept)
  (auto-classify)
  (c-taxonomy-position concept))

(defun get-concept (name)
  (setf name (translate-concept name))
  (car (multiple-value-list (g-concept name))))

(defun get-role (name)
  (car (multiple-value-list (r-defined name))))

(defun get-all-concepts ()
  (mapcar #'get-concept (list* :top :bottom *c-definitions*)))

(defun get-all-roles ()
  (mapcar #'get-role *r-definitions*))

(defun classified-tkb? ()
  "Return T if the TBox is fully processed and classified; NIL otherwise."
  (when (tkb-classified)
    T))

(defun what-is? (obj)
  (typecase obj
	    (grail-concept
	     (if (grail-concept-primitive obj) 'primitive 'concept))
	    (role
	     (if (role-functional obj) 'feature 'role))))

(defun is-primitive? (c)
  (and (typep c 'grail-concept) (grail-concept-primitive c)))

(defun is-concept? (c)
  (and (typep c 'grail-concept) (not (grail-concept-primitive c))))

(defun is-role? (r)
  (and (typep r 'role) (not (role-functional r))))

(defun is-feature? (r)
  (and (typep r 'role) (role-functional r)))

(defun name (obj)
  (typecase obj
	    (grail-concept (grail-concept-name obj))
	    (role (role-grail-name obj))))

(defun description (obj)
  (typecase obj
	    (grail-concept
	     (let ((d (grail-concept-definition obj)))
	       (if (grail-concept-primitive obj)
		   (if d
		       (kris-concept
			(flatten (list :and
				       (intern (format nil "~A*" (grail-concept-name obj)) (symbol-package (grail-concept-name obj)))
				       d)))
		     (car (multiple-value-list
			   (intern (format nil "~A*" (grail-concept-name obj)) (symbol-package (grail-concept-name obj))))))
		 (kris-concept d))))
	    (role 
	     (let (d)
	       (when (role-transfered-by obj) (push t d) (push :transitive d))
	       (when (role-parents obj) (push (role-parents obj) d) (push :supers d))
	       (push (role-grail-name obj) d)))))


;;; ************** KRSS INTERFACE **************

(defun define-concept-f (n d)
  (defconcept-f n d))

(defmacro define-concept (n d)
  `(defconcept-f ',n ',d))

;;; Strictly speaking, KRSS specifies that there MUST be a definition
;;; but in practice this seems to be ignored so we will make it optional
(defun define-primitive-concept-f (n &optional d)
  (defprimconcept-f n d))

(defmacro define-primitive-concept (n &optional d)
  `(defprimconcept-f ',n ',d))

(defun define-primitive-role-f (n &optional d)
  (if d
      (if (and (listp d) (member (first d) '(:and and)))
	  (defprimrole-f n :parents (cdr d))
	(if (member d '(:top top))
	    (defprimrole-f n)
	  (defprimrole-f n :parents d)))
    (defprimrole-f n)))

(defmacro define-primitive-role (n &optional d)
  `(define-primitive-role-f ',n ',d))

(defun define-primitive-attribute-f (n &optional d)
  (if d
      (if (and (listp d) (member (first d) '(:and and)))
	  (defprimattribute-f n :parents (cdr d))
	(if (member d '(:top top))
	    (defprimattribute-f n)
	  (defprimattribute-f n :parents d)))
    (defprimattribute-f n)))

(defmacro define-primitive-attribute (n &optional d)
  `(define-primitive-attribute-f ',n ',d))
