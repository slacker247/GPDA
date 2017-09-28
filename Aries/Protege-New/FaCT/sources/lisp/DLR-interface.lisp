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
 '(defprimrelationship defprimconcept implies impliesrel defprimattribute
   satcon? satrel? subscon? subsrel? load-tkb classify-tkb init-tkb))

;;; ************** DLR INTERFACE **************

(defparameter *dlr* T)
(defparameter *dlr-max-arity* 0)
(defparameter *dlr-rel-names* nil)
(defparameter *dlr-con-names* nil)
(defparameter *dlr-rel-defs* nil)
(defparameter *dlr-con-defs* nil)
(defparameter *dlr-implications* nil)
(defparameter *dlr-satisfiable* nil)

(defun intern-user (s)
  (intern s (find-package "USER")))

(defun dlr-set-max-arity (arity)
  (setf *dlr-max-arity* (max *dlr-max-arity* arity))
  arity)

(defun dlr-toprel (n)
  (if (and (integerp n) (> n 0))
      (intern-user (format nil "TOP~A" (dlr-set-max-arity n)))))

(defun dlr-chnum (ch)
  (- (char-code ch) (char-code #\0)))

(defun dlr-rel-role (role &optional (arity 9))
  (let ((s (symbol-name role)))
    (if (and (= (length s) 2) (eq (elt s 0) #\$))
	(let ((sel (dlr-chnum (elt s 1))))
	  (if (<= sel arity)
	      (if *dlr* (intern-user (format nil "U~A" sel))
		(intern-user (format nil "TOPROLE~A~A" (dlr-set-max-arity arity) sel))))))))

(defun dlr-translate-rel (r)
  (if (atom r)
      (let ((rn (symbol-name r)))
	(if (and (= (length rn) 4) (string-equal rn "Top" :end1 3) (>= (dlr-chnum (elt rn 3)) 1)
		 (<= (dlr-chnum (elt rn 3)) 9))
	    (dlr-toprel (dlr-chnum (elt rn 3)))
	  (progn
	    (pushnew r *dlr-rel-names*)
	    r)))
    (if (endp r)
	(error "bad relation syntax ~S" r)
      (case (car r)
	((and or) (cons (first r) (mapcar #'dlr-translate-rel (rest r))))
	(not (list 'not (dlr-translate-rel (second r))))
	(T
	 (if (and (= (length r) 3) (dlr-rel-role (first r) (second r)) (dlr-toprel (second r)))
	     `(and ,(dlr-toprel (second r)) (all ,(dlr-rel-role (first r) (second r))
					     ,(dlr-translate-con (third r))))
	   (error "bad relation syntax ~S" r)))))))
	
(defun dlr-translate-con (c)
  (if (atom c)
      (case (if (symbolp c) (intern (symbol-name c) "KEYWORD") c)
	(:top :top)
	(:bottom :bottom)
	(T
	 (pushnew c *dlr-con-names*)
	 c))
    (if (endp c)
	(error "bad concept syntax ~S" c)
      (case (if (symbolp (car c)) (intern (symbol-name (car c)) "KEYWORD") 
		    (car c))
	(:and (cons :and (mapcar #'dlr-translate-con (rest c))))
	(:or (cons :or (mapcar #'dlr-translate-con (rest c))))
	(:not (list 'not (dlr-translate-con (second c))))
	(:some (list :some
		     `(:inv ,(dlr-rel-role (second c)))
		     (dlr-translate-rel (third c))))
	(:all (list :all
	       	  `(:inv ,(dlr-rel-role (second c)))
	       	  (dlr-translate-rel (third c))))
	(:atmost
	 (if (and (= (length c) 4) (integerp (second c))
		  (>= (second c) 0) (dlr-rel-role (third c)))
	     (if (equal *reasoner* "SHIQ")
		 `(:at-most ,(second c) (:inv ,(dlr-rel-role (third c)))
			    ,(dlr-translate-rel (fourth c)))
	       (if (zerop (second c))
		   `(:all (:inv ,(dlr-rel-role (third c)))
			  (:not ,(dlr-translate-rel (fourth c))))
		 (let ((pc (intern-user (format nil "ATMOST-~A-INV-~A-~A"
					   (second c) (dlr-rel-role (third c))
					   (if (atom (fourth c)) (fourth c) "C")))))
		   (format T "~&;;;!!WARNING!!: can't handle atmost ~A~%"
			   (second c))
		   (pushnew pc *dlr-con-defs*)
		   pc)))
	   (error "bad concept syntax ~S" c)))
	(:atleast
	 (if (and (= (length c) 4) (integerp (second c))
		  (>= (second c) 0) (dlr-rel-role (third c)))
	     (if (equal *reasoner* "SHIQ")
		 `(:at-least ,(second c) (:inv ,(dlr-rel-role (third c)))
			     ,(dlr-translate-rel (fourth c)))
	       (if (> (second c) 1)
		   (progn
		     (format T "~&;;;!!WARNING!!: can't handle atleast ~A~%" 
			     (second c))
		     `(some (:inv ,(dlr-rel-role (third c))) 
			    ,(dlr-translate-rel (fourth c))))
		 (if (= 1 (second c))
		     `(some (:inv ,(dlr-rel-role (third c))) 
			    ,(dlr-translate-rel (fourth c)))
		   (top-symbol))))
	   (error "bad concept syntax ~S" c)))))))

(defun dlr-defprimrelationship-f (rel arity)
  (if (and (atom rel) (integerp arity) (> arity 0))
      (if (member (list rel arity) *dlr-rel-defs* :key #'first)
	  (error "relationship multiply defined ~S" rel)
	(push (list rel (dlr-set-max-arity arity)) *dlr-rel-defs*))
    (error "syntax error (defprimrelationship ~A ~A)" rel arity)))

(defun dlr-defprimconcept-f (con)
  (if (atom con)
      (if (member con *dlr-con-defs*)
	  (error "concept multiply defined ~S" con)
	(push con *dlr-con-defs*))
    (error "syntax error (defprimconcept ~A)" con)))

(defun dlr-implies-f (c1 c2)
  (push (list (dlr-translate-con c1) (dlr-translate-con c2)) *dlr-implications*))

(defun dlr-impliesrel-f (r1 r2)
  (push (list (dlr-translate-rel r1) (dlr-translate-rel r2)) *dlr-implications*))

(defun satcon?-f (con) (push (dlr-translate-con con) *dlr-satisfiable*))

(defun satrel?-f (rel) (push (dlr-translate-rel rel) *dlr-satisfiable*))
		     
(defun dlr-rn (n)
  (intern-user (format nil "U~A" n)))

(defun dlr-write-kb (&optional (f T))
  (progv '(*print-circle*) '(nil)
  (dotimes (i *dlr-max-arity*)
;;;    (format f "(defprimrole U~A)~%" (1+ i))
    (format f "(defprimattribute U~A)~%" (1+ i))
;;;    (format f "(implies *TOP* (at-most 1 U~A))~%" (1+ i))
    )
  (format f "(defprimconcept TOPC ~A)~%"
	  (do ((j 0 (1+ j))
	       (c nil (cons `(all ,(dlr-rn (1+ j)) :BOTTOM) c)))
	      ((= j *dlr-max-arity*) (cons 'and c))))
  (dotimes (i *dlr-max-arity*)
    (format f "(defprimconcept ~A ~A)~%" (dlr-toprel (1+ i))
	    (do ((j 0 (1+ j))
		 (c nil (cons (if (<= j i)
				  `(some ,(dlr-rn (1+ j)) TOPC)
				`(all ,(dlr-rn (1+ j)) :BOTTOM)) c)))
		((= j *dlr-max-arity*) (cons 'and c)))))
  (dolist (r *dlr-rel-defs*)
    (setf *dlr-rel-names* (delete (first r) *dlr-rel-names*))
    (format f "(defprimconcept ~A ~A)~%" (first r) (dlr-toprel (second r))))
  (dolist (r *dlr-rel-names*)
    (format T "~&;;;!!WARNING!!: relationship ~A used but not defined~%" r)
    (format f "(defprimconcept ~A ~A)~%" r (dlr-toprel *dlr-max-arity*)))
  (dolist (c *dlr-con-defs*)
    (setf *dlr-con-names* (delete c *dlr-con-names*))
    (format f "(defprimconcept ~A TOPC)~%" c))
  (dolist (c *dlr-con-names*)
    (format T "~&;;;!!WARNING!!: concept ~A used but not defined~%" c)
    (format f "(defprimconcept ~A TOPC)~%" c))
  (dolist (i *dlr-implications*)
    (format f "(implies ~A ~A)~%" (first i) (second i)))
  (if *dlr-satisfiable* (format f "~%"))
  (dolist (s *dlr-satisfiable*)
    (format f ";(satisfiable '~A)~%" s))
  (values)))

(defmacro defprimrelationship (rel arity)
  `(dlr-defprimrelationship-f ',rel ,arity))

(defmacro defprimconcept (con)
  `(dlr-defprimconcept-f ',con))

(defmacro implies (c1 c2)
  `(dlr-implies-f ',c1 ',c2))

(defmacro impliesrel (r1 r2)
  `(dlr-impliesrel-f ',r1 ',r2))


(defun satisfiable (concept)
;;;  (if *auto-classify* (classify-tkb))
  (progv '(*auto-install-primitives*) '(T)
;;;    (when (auto-pre-process 
;;;	   `(test-sat (encode-concept-term (translate-concept ',concept))))
    (when (test-sat (encode-concept-term (translate-concept concept)))
      T)))

(defmacro satcon? (c)
;;;  `(satcon?-f ',c))
  `(satisfiable (dlr-translate-con ',c)))

(defmacro satrel? (r)
;;;  `(satrel?-f ',r))
  `(satisfiable (dlr-translate-rel ',r)))

(defmacro subscon? (c1 c2)
  `(not (satisfiable (dlr-translate-con '(and ,c2 (not ,c1))))))

(defmacro subsrel? (r1 r2)
  `(not (satisfiable (dlr-translate-rel '(and ,r2 (not ,r1))))))
    
(defun dlr-translate-kb (infile outfile)
  (setf *dlr-max-arity* 0)
  (setf *dlr-rel-names* nil)
  (setf *dlr-con-names* nil)
  (setf *dlr-rel-defs* nil)
  (setf *dlr-con-defs* nil)
  (setf *dlr-implications* nil)
  (setf *dlr-satisfiable* nil)
  (let ((i-f (open (probe-file infile)))
	(o-f (open (or (probe-file outfile) outfile) :direction :output :if-exists :supersede)))
    (loop
      (let ((s (read i-f nil)))
	(if s (eval s) (return))))
    (dlr-write-kb o-f)
    (close i-f)
    (close o-f)))

(defun dlr2lisp (infile outfile) (dlr-translate-kb infile outfile))

(defun load-tkb (filename &key (verbose t) (overwrite nil))
  (progv
      '(*verbosity*)
      '((:warnings))
    (when verbose (set-verbosity :classify-1))
    (when overwrite (dlr-clear-kb))
    (let ((f (probe-file filename)))
      (if f
	  (let ((i-f (open f)))
	    (push (namestring f) *kb-file-names*)
	    (loop
	      (let ((s (read i-f nil)))
		(if s 
		    (eval s)
		  (return))))
	    (close i-f))
	(progn
	  (verbosity :warnings "~&!!WARNING!! file not found - ~S~%" filename)
	  nil)))))

(defun dlr-clear-kb ()
  "Initialise KB to contain only *TOP* and *BOTTOM*"
;;; DLR stuff
  (setf *dlr-max-arity* 0)
  (setf *dlr-rel-names* nil)
  (setf *dlr-con-names* nil)
  (setf *dlr-rel-defs* nil)
  (setf *dlr-con-defs* nil)
  (setf *dlr-implications* nil)
  (setf *dlr-satisfiable* nil)
;;; FaCT stuff
  (setf *kb-file-names* nil)
  (clrhash *grail-concept-table*)
  (setf *c-definitions* nil)
  (setf *next-available-concept* 2)
  (setf (s-concept *TOP*) (make-system-concept :children `(,*BOTTOM*) :classified 2
					       :synonym *TOP* :grail-name :top))
  (setf (s-concept *BOTTOM*) (make-system-concept :parents `(,*TOP*) :classified 2
						  :synonym *BOTTOM* :grail-name :bottom))
  (setf (g-concept :TOP) (make-grail-concept :name (top-symbol) :definition (top-symbol)))
  (setf (g-concept :BOTTOM) (make-grail-concept :name (bot-symbol) :definition (bot-symbol)))
  (clrhash *concept-definition-table*)
;;; Map GRAIL built in names for top and bottom concepts
  (setf (system-name :top) *TOP*)
  (setf (system-name :bottom) *BOTTOM*)
  (clrhash *relations*)
  (setf *r-definitions* nil)
  (setf *transitive-roles* nil)
  (setf *universal-constraint* nil)
  (setf *grail-universal-constraints* nil)
  (setf *implications* nil)
  (values))

(defun classify-tkb (&key (mode :nothing))
  (progv '(*verbosity*) '((:warnings :synonyms))
	 (case mode
	       (:stars (set-verbosity :classify-1))
	       (:names (set-verbosity :classify-2))
	       (:count (set-verbosity :test-counts))
	       (T))
	 (let ((o-f (open "dlr2fact.tmp" :direction :output :if-exists :supersede)))
	   (dlr-write-kb o-f)
	   (close o-f))
	 (let ((i-f (open "dlr2fact.tmp")))
	   (loop
	     (let ((s (read i-f nil)))
	       (if s 
		   (case (car s)
		     (defprimconcept (defprimconcept-f (second s) (third s)))
		     (implies (implies-f (second s) (third s)))
		     (T (eval s)))
		 (return))))
	   (close i-f))
	 (delete-file "dlr2fact.tmp")
	 (classify-all-concepts)))

(defun defprimrole-f (r &key (parents nil) (supers nil) (transitive nil)
			     (i-functional nil))
  (let ((i-r (inv-r r)))
    (setf parents (union parents supers))
    (when (and parents (not (listp parents)))
      (error "~&!!ERROR!! BAD :PARENTS LIST - ~S~%" parents))
    (grail-define-relation r i-r parents nil i-functional)
    (when (eq transitive T)
      (grail-redefine-relation r :transitive-across `(,r))
      (grail-redefine-relation i-r :transitive-across `(,i-r)))
    (car (multiple-value-list (r-defined r)))))

(defmacro defprimrole (r &key (parents nil) (supers nil) (transitive nil)
			      (i-functional nil))
  `(defprimrole-f ',r :parents ',parents :supers ',supers :transitive ,transitive
		  :i-functional ,i-functional))

(defun defprimattribute-f (r &key (parents nil) (supers nil) (i-functional nil))
  (setf parents (union parents supers))
  (when (and parents (not (listp parents)))
	(error "~&!!ERROR!! BAD :PARENTS LIST - ~S~%" parents))
  (grail-define-relation r (inv-r r)
;			 (intern-user (format nil "I*~A" r))
			 parents T i-functional))

(defmacro defprimattribute (r &key (parents nil) (supers nil) (i-functional nil))
  `(defprimattribute-f ',r :parents ',parents :supers ',supers :i-functional ,i-functional))

(defun defprimconcept-f (n &optional d)
  (grail-define-primconcept n (when d (translate-concept d))))

(defun implies-f (a c)
  (grail-define-implication (translate-concept a) (translate-concept c))
  (kris-concept a))

(defun init-tkb ()
  (dlr-clear-kb))
