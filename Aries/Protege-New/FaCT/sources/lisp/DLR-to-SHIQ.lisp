;;; -*- Mode: Lisp; Package: DLR -*-
;;; 
;;; COPYRIGHT (C) 2001 Sergio Tessaris
;;; and THE UNIVERSITY OF MANCHESTER, tessaris@cs.man.ac.uk
;;; 
;;; DLR interface for a DL reasoner
;;; 
;;; DLR-to-SHIQ.lisp
;;;
;;; $Id: DLR-to-SHIQ.lisp,v 1.4 2001/06/01 12:21:19 sergio Exp $
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                      ;;;
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; package declarations

(defpackage "DLR"
  ;;(:use "FACT")
  (:export "DEFPRIMRELATIONSHIP" "DEFPRIMCONCEPT" "IMPLIESREL" "IMPLIES"
	   "DEFPRIMRELATIONSHIP-F" "DEFPRIMCONCEPT-F" "IMPLIESREL-F" "IMPLIES-F"
	   "SATCON?" "SATREL?" "SATCON?-F" "SATREL?-F"
	   "LOAD-TKB" "CLASSIFY-TKB" "INIT-TKB" "WRITE-TKB"
	   ;; new interface
	   "DEFPRIMITIVE-F" "SATISFIABLE?-F" "EQUIVALENT?-F" "SUBSUME?-F"
	   "DEFPRIMITIVE" "SATISFIABLE?" "EQUIVALENT?" "SUBSUME?"
	   "INSTANCE" "INSTANCE-F"))

(in-package "DLR")

(unless (find-package "FACT") (make-package "FACT"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *dlr-max-arity* 0
  "Denotes the maximun arity of the relations currently defined.")

(defvar *dlr-names* (make-hash-table :test 'equal)
  "Contains the primitive names (concepts and relations) currently
defined, together with their arity.")

(defvar *dlr-tuples* (make-hash-table :test 'equalp)
  "Contains the defined tuples of individuals, the value is 
the individual representing the reification of the tuple.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Atomic names

(defun normalise-name (item)
  (typecase item
    (null (error "Symbol NIL is not a valid name"))
    ((or symbol string) (string-upcase (string item)))
    (t (string-upcase (format nil "~A" item)))))

(defun new-dlr-name (name arity)
  (let* ((key (normalise-name name))
	 (old-arity (gethash key *dlr-names*)))
    (cond
     (old-arity
      (when (/= arity old-arity)
	(error "Redefinition of ~A/~D as  ~A/~D" name (old-arity name arity))))
     (t
      (setf (gethash key *dlr-names*) arity)
      (if (> arity *dlr-max-arity*) (setq *dlr-max-arity* arity))))
    
    (values key arity)))

(defun dlr-name-arity (name)
  (let ((key (normalise-name name)))
    (or (gethash key *dlr-names*)
	(cond				; interpret standard names
	 ((string= key "TOP") 1)
	 ((string= key "BOTTOM") 1)
	 ((string= key "TOPC") 1)
	 ((string= key "TOP" :end1 3)
	  (let ((pos (extract-integer key :start 3)))
	    (if (integerp pos) pos)))))))

;;; Individuals

(defvar *tuple-counter* 0)

(defun tuple-reification-individual (tuple)
  (let ((tuple-array (make-array (list (length tuple))
				 :initial-contents (mapcar #'normalise-name
							   tuple))))
    (or (gethash tuple-array *dlr-tuples*)
	(setf (gethash tuple-array *dlr-tuples*)
	  (format nil "*TUPLE~D*" (incf *tuple-counter*))))))

(defun representative-concept (ind)
  (format nil "*CONC*-~A" (normalise-name ind)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Operators

(defun normalise-op (item)
  (etypecase item
    (string (intern (string-upcase item) (find-package "KEYWORD")))
    (symbol (intern (string-upcase (symbol-name item)) (find-package "KEYWORD")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Conversion related symbols

(defun dlr-top (arity)
  (if (= arity 1) "TOPC"
    (format nil "TOP~D" arity)))

(defun dlr-pos-attribute (position)
  (format nil "U~D" position))

(defun extract-integer (string &key (start 0) end)
  (let ((last-index (1- (length string))))
    (when (<= start last-index)
      (loop with result = nil
	  for i from start to (if end (min end last-index) last-index)
	  for value = (digit-char-p (aref string i))
	  unless value do (return nil)
	  do (setq result (if result (+ (* result 10) value) value))
	  finally (return result)))))

(defun dlr-proj-operator (op)
  (let ((string-op (string op)))
    (when (char= (aref string-op 0) #\$)
      (let ((pos (extract-integer string-op :start 1)))
	(if (integerp pos) pos)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Translate expressions

(defun dlr-expr->shiq (dlr-exp &key expected-arity)
  "Translates a DLR expression (concept or relation) to Shiq. If specified,
the value `expected-arity' is compared to the arity of the parsed expression."
  
  (if (atom dlr-exp)
      (let* ((name (normalise-name dlr-exp))
	     (arity (dlr-name-arity name)))
	(cond
	 
	 ((null arity)			; new name
	  (if expected-arity
	      (new-dlr-name name expected-arity)
	    (error "Undefined DLR name ~A" name))
	  (values (shiq-name name) expected-arity))
	 
	 ((integerp arity)
	  (if (and (integerp expected-arity) (/= arity expected-arity))
	      (error "DLR name ~A has arity ~D instead of the expected ~D"
		     dlr-exp arity expected-arity)
	    (values (shiq-name name) arity)))
	 
	 (t
	  (error "Bad DLR expression ~A." dlr-exp))))
    
    (let ((op (normalise-op (car dlr-exp)))
	  (args (cdr dlr-exp)))
      (case op
	((:AND :OR)			; (:AND R ... R) -> (:AND c-R ... c-R)
					; (:AND C ... C) -> (:AND c-C ... c-C)
	 (if (> 1 (length args))
	     (error "Bad DLR expression ~A" dlr-exp))
	 (loop with def-arity = expected-arity
	     for d-e in args
	     for (concept arity) = (multiple-value-list
				    (dlr-expr->shiq d-e :expected-arity def-arity))
	     when (or (null def-arity)
		      (= def-arity arity)) collect concept into result
	     when (and def-arity
		       (/= def-arity arity)) do (error "Uncompatible arity of ~A in ~A"
						       d-e
						       dlr-exp)
	     do (setq def-arity arity)
		
	     finally (return (values (if (eq op :AND)
					 (shiq-and result)
				       (shiq-or result))
				     def-arity))))
	
	(:NOT				; (:NOT R) -> (:AND TOPn (:NOT c-R))
					; (:NOT C) -> (:NOT c-C)
	 (if (/= 1 (length args))
	     (error "Bad DLR expression ~A" dlr-exp))
	 (multiple-value-bind (concept arity)
	     (dlr-expr->shiq (first args) :expected-arity expected-arity)
	   
	   (values (if (= arity 1)	; it's a concept
		       (shiq-not concept)
		     (shiq-and (shiq-name (dlr-top arity))
			       (shiq-not concept)))
		   arity)))
	
	((:SOME	:ALL)			; (:SOME $n R) -> (:SOME (:INV Un) c-R)
					; (:ALL $n R) = (:NOT (:SOME $n (:NOT R)))
	 (if (/= 2 (length args))
	     (error "Bad DLR concept expression ~A" dlr-exp))
	 (let ((pos (dlr-proj-operator (first args))))
	   (unless (typep pos '(integer 1))
	     (error "Bad DLR concept expression ~A" dlr-exp))
	   (multiple-value-bind (relation arity) (dlr-expr->shiq (second args))
	     (unless (<= pos arity)
	       (error "Selecting the ~:R component of a relation of arity ~D in ~A"
		      pos arity dlr-exp))
	     (values (if (eq op :SOME)
			 (shiq-some (shiq-inverse-role
				     (shiq-name (dlr-pos-attribute pos)))
				    relation)
		       (shiq-not
			(shiq-some (shiq-inverse-role (shiq-name (dlr-pos-attribute pos)))
				   (shiq-and (shiq-name (dlr-top arity))
					     (shiq-not relation)))))
		     1))))
	
	((:ATLEAST :ATMOST)		; (:ATLEAST k $i R) -> (:ATLEAST k (:INV Ui) R)
	 (if (/= 3 (length args))
	     (error "Bad DLR concept expression ~A" dlr-exp))
	 (let ((nr (first args))
	       (pos (dlr-proj-operator (second args))))
	   (unless (and (typep nr '(integer 0))
			(typep pos '(integer 1)))
	     (error "Bad DLR concept expression ~A" dlr-exp))
	   (multiple-value-bind (relation arity) (dlr-expr->shiq (third args))
	     (unless (<= pos arity)
	       (error "Selecting the ~:R component of a relation of arity ~D in ~A"
		      pos arity dlr-exp))
	     (values (if (eq op :ATMOST)
			 (shiq-atmost nr (shiq-inverse-role
					  (shiq-name (dlr-pos-attribute pos)))
				      relation)
		       (shiq-atleast nr (shiq-inverse-role
					 (shiq-name (dlr-pos-attribute pos)))
				     relation))
		     1))))
	
	(t				; ($i n C) -> (:AND TOPn (:ALL Ui c-C))
	 (if (= 2 (length args))
	     (let ((pos (dlr-proj-operator op))
		   (arity (first args))
		   (concept (dlr-expr->shiq (second args) :expected-arity 1)))
	       (if pos
		   (if (or (< pos 0) (> pos arity))
		       (error "Selecting the ~:R position in a relation of arity ~D in ~A"
			      pos arity dlr-exp)
		     (values (shiq-and (shiq-name (dlr-top arity))
				       (shiq-all (shiq-name (dlr-pos-attribute pos))
						 concept))
			     arity))
		 (error "Bad DLR relation ~A" dlr-exp)))
	   (error "Bad DLR operator ~S in ~A" op dlr-exp)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Implicit axioms/assertions

(defvar *asserted-arity* 0)
(defvar *asserted-primitive* nil)

(defvar *asserted-individuals* nil)

(defun implicit-axioms (&key (incremental t))
  (unless incremental
    (setq *asserted-arity* 0)
    (setq *asserted-primitive* nil))
  ;; Assertions for the top relations
  (when (> *dlr-max-arity* *asserted-arity*) ; the arity has changed
    (loop for i from 1 to *dlr-max-arity*
	for concept = (shiq-and
		       (loop for j from (if (> i *asserted-arity*) 1
					  (1+ *asserted-arity*)) to *dlr-max-arity*
			   for attribute = (shiq-name (dlr-pos-attribute j))
			   if (and (> i 1) (<= j i))
			   nconc (list
				  (shiq-some attribute (shiq-name (dlr-top 1)))
				  (shiq-atmost 1 attribute (shiq-name "TOP")))
			   else collect (shiq-all attribute (shiq-name "BOTTOM"))))
	do (shiq-implies (shiq-name (dlr-top i)) concept))
    (setq *asserted-arity* *dlr-max-arity*))
  
  ;; Primitive assertions
  (loop with added-prim = *asserted-primitive*
      for prim being each hash-key of *dlr-names* using (hash-value arity)
      unless (member prim *asserted-primitive* :test #'string=)
      do (shiq-implies (shiq-name prim) (shiq-name (dlr-top arity)))
	 (setq added-prim (cons prim added-prim))
      finally (setq *asserted-primitive* added-prim))
  
  (values))

(defun implicit-assertions (&key (incremental t))
  (unless incremental (setq *asserted-individuals* nil))
  
  (loop for tuple being each hash-key of *dlr-tuples* using (hash-value tuple-ind)
      for arity = (length tuple)
      unless (member tuple-ind *asserted-individuals*) do
	 
	(loop with concept-list = (list (shiq-name (dlr-top arity)))
	    for position from 0 to (1- arity)
	    for ind = (aref tuple position)
		      
	    finally (shiq-instance (shiq-name (aref tuple 0))
				   (shiq-atmost 1 (shiq-inverse-role
						   (dlr-pos-attribute 1))
						(shiq-and concept-list)))
		    (push tuple-ind *asserted-individuals*)
		      
	    if (> position 0) collect
	      (shiq-some (shiq-name (dlr-pos-attribute position))
			 (shiq-name (representative-concept ind))) into concept-list
									
	    unless (member ind *asserted-individuals*) do
	      (shiq-instance (shiq-name ind) (shiq-name (representative-concept ind)))
	      (push ind *asserted-individuals*)))
  
  (values))
		   


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; User interface

(defun init-tkb ()
  (clrhash *dlr-names*)
  (clrhash *dlr-tuples*)
  (setq *dlr-max-arity* 0)
  (setq *asserted-arity* 0)
  (setq *asserted-primitive* nil)
  (setq *asserted-individuals* nil)
  (setq *tuple-counter* 0)
  (shiq-reset)
  (values))

(defun load-tkb (filename)
  (init-tkb)
  (load filename))

(defun classify-tkb ()
  (shiq-classify))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Functional interface

(defun defprimrelationship-f (relation arity)
  (new-dlr-name relation arity)
  (values))

(defun defprimconcept-f (concept)
  (new-dlr-name concept 1)
  (values))

(defun impliesrel-f (rel1 rel2)
  (implies-f rel1 rel2))

(defun satcon?-f (dlr-conc-exp)
  (shiq-satisfiablep (dlr-expr->shiq dlr-conc-exp)))

(defun satrel?-f (dlr-rel-exp)
  (shiq-satisfiablep (dlr-expr->shiq dlr-rel-exp)))

;;; new uniform interface

;;; assertions

(defun defprimitive-f (name &optional arity)
  "Introduce a new primitive name having given arity. If arity is
not specified, then it is assumed 1 (a concept name)."
  (if (integerp arity)
      (new-dlr-name name arity)
    (new-dlr-name name 1))
  (values))

(defun implies-f (expr1 expr2)
  "Assert that `expr1' logically implies `expr2' (i.e. expr1 is 
a subclass of expr2). They must have the very same arity, otherwise
an error is raised."
  (multiple-value-bind (shiq-c1 arity-c1) (dlr-expr->shiq expr1)
    (multiple-value-bind (shiq-c2 arity-c2) (dlr-expr->shiq expr2)
      (unless (= arity-c1 arity-c2)
	(error "Expressions ~A/~D and ~A/~D must have the same arity."
	       expr1 arity-c1 expr2 arity-c2))
      (shiq-implies shiq-c1 shiq-c2)
      (values))))

(defun instance-f (ind expr)
  (etypecase ind
    (cons				; it's a tuple
     (multiple-value-bind (relation arity) (dlr-expr->shiq expr)
       (if (= arity (length ind))
	   (loop with ind-tuple = (tuple-reification-individual ind)
	       for individual in ind
	       for position from 1
	       finally (shiq-instance (shiq-name ind-tuple) relation)
	       do (shiq-relate ind-tuple (shiq-name (normalise-name individual))
			       (shiq-name (dlr-pos-attribute position))))
	 (error "Tuple ~S and expression ~S/~D have incompatible arities"
		ind expr arity))))
    
    ((or symbol string)			; individual name
     (multiple-value-bind (concept arity) (dlr-expr->shiq expr)
       (if (= arity 1)
	   (shiq-instance (shiq-name (normalise-name ind)) concept)
	 (error "Expecting a concept expression instead of ~S/~D" expr arity)))))
  
  (values))

;;; queries

(defun satisfiable?-f (dlr-expr)
  (shiq-satisfiablep (dlr-expr->shiq dlr-expr)))

(defun equivalent?-f (expr1 expr2)
  (multiple-value-bind (shiq-c1 arity-c1) (dlr-expr->shiq expr1)
    (multiple-value-bind (shiq-c2 arity-c2) (dlr-expr->shiq expr2)
      (and (= arity-c1 arity-c2) (shiq-equivalentp shiq-c1 shiq-c2)))))

(defun subsume?-f (expr1 expr2)
  (multiple-value-bind (shiq-c1 arity-c1) (dlr-expr->shiq expr1)
    (multiple-value-bind (shiq-c2 arity-c2) (dlr-expr->shiq expr2)
      (and (= arity-c1 arity-c2) (shiq-subsumep shiq-c1 shiq-c2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Macros

(defmacro defprimrelationship (relation arity)
  `(defprimrelationship-f ',relation ,arity))

(defmacro defprimconcept (concept)
  `(defprimconcept-f ',concept))

(defmacro impliesrel (rel1 rel2)
  `(impliesrel-f ',rel1 ',rel2))

(defmacro satcon? (concept)
  `(satcon?-f ',concept))

(defmacro satrel? (relation)
  `(satrel?-f ',relation))

;;; new uniform interface

(defmacro defprimitive (name &optional arity)
  `(defprimitive-f ',name ,arity))

(defmacro implies (conc1 conc2)
  `(implies-f ',conc1 ',conc2))

(defmacro instance (ind expr)
  `(instance-f ',ind ',expr))

(defmacro satisfiable? (expr)
  `(satisfiable?-f ',expr))

(defmacro equivalent? (expr1 expr2)
  `(equivalent?-f ',expr1 ',expr2))

(defmacro subsume? (expr1 expr2)
  `(subsume?-f ',expr1 ',expr2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Shiq bridge


(defparameter *shiq-resend-axioms* nil
  "If non-nil all the axioms are sent to SHIQ reasoner
each time.")

(defvar *shiq-axioms* nil)
(defvar *shiq-assertions* nil)
(defvar *shiq-classified* nil)
(defvar *shiq-satisfiable* t)

(defun shiq-reset ()
  "Initialise the reasoner."
  (setq *shiq-axioms* nil)
  (setq *shiq-assertions* nil)
  (setq *shiq-classified* nil)
  (setq *shiq-satisfiable* t)
  
  (classifier-reset)
  (values))

(defun shiq-implies (c1 c2)
  (push (cons c1 c2) *shiq-axioms*)
  (setq *shiq-classified* nil)
  (unless *shiq-resend-axioms* (classifier-implies c1 c2))
  (values))

(defun shiq-instance (ind concept)
  (push (cons ind concept) *shiq-assertions*)
  (setq *shiq-classified* nil)
  (unless *shiq-resend-axioms* (classifier-instance ind concept))
  (values))

(defun shiq-relate (ind1 ind2 role)
  (push (cons (cons ind1 ind2) role) *shiq-assertions*)
  (setq *shiq-classified* nil)
  (unless *shiq-resend-axioms* (classifier-relate ind1 ind2 role))
  (values))

(defun write-tkb (&optional (stream t))
  "Write all the axioms to the stream (default to standard output).
Variable `stream' can be either t for standar output, a valid output
stream, or a string denoting a filename."
  
  ;; first verify that all the necessary DLR axioms have been committed
  (implicit-axioms)
  (implicit-assertions)

  (let ((*package* (find-package "CL-USER")) ; for decent printing
	(os (cond
	     ((streamp stream) stream)
	     ((stringp stream) (open stream :direction :output
				     :if-exists :overwrite
				     :if-does-not-exist :create))
	     (t t))))
				       
    (loop for (c1 . c2) in *shiq-axioms*
	do (format os "~&~S" (list 'IMPLIES c1 c2)))
    
    (loop for (item . expr) in *shiq-assertions*
	do (format os "~&~S" (if (consp item)
				 (list 'RELATED (car item) (cdr item) expr)
			       (list 'INSTANCE item expr))))
    
    (when (and (streamp os) (stringp stream)) (close os)))
  
  (values))

(defun shiq-classify ()
  (unless *shiq-classified*
    ;; first verify that all the necessary DLR axioms have been committed
    (implicit-axioms)
    (implicit-assertions)
    (when *shiq-resend-axioms* ;; send all the axioms every time
      ;; here we need something to initialise the KB
      (classifier-reset)
      (loop for (c1 . c2) in *shiq-axioms*
	  do (classifier-implies c1 c2))
      (loop for (item . expr) in *shiq-assertions*
	  do (if (consp item)
		 (classifier-relate (car item) (cdr item) expr)
	       (classifier-instance item expr))))
    ;; call the reasoner
    (setq *shiq-satisfiable* (classifier-kb-sat))
    (setq *shiq-classified* t))
  
  *shiq-satisfiable*)
  

(defun shiq-satisfiablep (concept)
  "Returns not nil if the shiq concept is satisfiable
w.r.t. the knowledge base."
  ;; first make sure that the KB is classified
  (shiq-classify)
  ;; then go to the classifier
  (classifier-concept-sat concept))

(defun shiq-subsumep (c1 c2)
  "Returns not nil if concept `c1' subsumes `c2'."
  ;; first make sure that the KB is classified
  (shiq-classify)
  ;; then go to the classifier
  (classifier-concept-subsume c1 c2))

(defun shiq-equivalentp (c1 c2)
  "Returns not nil if concept `c1' is equivalent to `c2'."
  (and (shiq-subsumep c1 c2) (shiq-subsumep c2 c2)))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Classifier specific functions/macros

(defmacro call-classifier (&body body)
  "Isolate calls to the classifier by trapping conditions."
  `(handler-case (progn ,@body)
     (serious-condition (condition)
       (format t "~&Error in Classifier: ~A~&" condition)
       (if (y-or-n-p "Do you want the debugger? ")
	   (invoke-debugger condition)
	 (format t "~&You better reset the system.~&")))))

;;; syntax

(defun shiq-name (name)
  (cond
   ((string= name "TOP") :TOP)
   ((string= name "BOTTOM") :BOTTOM)
   (t (intern name (find-package "CL-USER")))))

(defun shiq-and (concept &rest args)
  (if (consp concept)
      `(:AND ,@concept)
    `(:AND ,concept ,@args)))

(defun shiq-or (concept &rest args)
  (if (consp concept)
      `(:OR ,@concept)
    `(:OR ,concept ,@args)))

(defun shiq-not (concept)
  `(:NOT ,concept))

(defun shiq-some (role concept)
  `(:SOME ,role ,concept))

(defun shiq-all (role concept)
  `(:ALL ,role ,concept))

(defun shiq-atmost (number role concept)
  `(:ATMOST ,number ,role ,concept))

(defun shiq-atleast (number role concept)
  `(:ATLEAST ,number ,role ,concept))

(defun shiq-inverse-role (role)
  `(:INV ,role))

;;; control

(defun classifier-reset ()
  "Resets the status of the classifier."
  (call-classifier (fact::clear-kb)))

(defun classifier-implies (c1 c2)
  "Asserts the concept implication."
  (call-classifier
     ;;(fact::implies-f c1 c2)
     (fact::grail-define-implication (fact::translate-concept c1)
				     (fact::translate-concept c2))))

(defun classifier-instance (individual concept)
  "Assert individual membership. Unimplemented."
  (declare (ignore individual concept))
  (values))

(defun classifier-relate (ind1 ind2 role)
  "Assert individuals relation. Unimplemented."
  (declare (ignore ind1 ind2 role))
  (values))


(defun classifier-kb-sat ()
  "Classifies the KB returning non NIL if the KB is satisfiable."
  (call-classifier
       (let ((fact::*verbosity* '(:warnings :synonyms)))
	 (and (fact::classify-all-concepts) t))))

(defun classifier-concept-sat (concept)
  "Returns non NIL if the concept is satisfiable w.r.t. the current KB."
  (call-classifier
   (let ((fact::*auto-install-primitives* t))
    (and (fact::test-sat (fact::encode-concept-term
			  (fact::translate-concept concept)))
	 t))))

(defun classifier-concept-subsume (c1 c2)
  "Returns not nil if concept `c1' subsumes `c2'."
  (call-classifier
   (let ((fact::*auto-install-primitives* t))
    (and (fact::c-subsumes c1 c2) t))))