;;; -*- Mode: Lisp; package: FACT; Syntax: COMMON-LISP; Base: 10 -*-

;;; FaCT and iFaCT COPYRIGHT (C) 1997, 1998, 1999 IAN R. HORROCKS
;;; and THE UNIVERSITY OF MANCHESTER, horrocks@cs.man.ac.uk
;;; Time-stamp: Tue Jun 15 17:41:50 BST 1999

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This is the FaCT and iFaCT system definition.                     ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Personalize the pathnames preceded by the comment:                ;;;
;;;               ";;; ### Personalize the following * pathname"      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

#-(OR ALLEGRO-V4.2 MCL LCL4.2 X3J13 CLTL2)
(IN-PACKAGE "USER")

#+(OR ALLEGRO-V4.2 MCL LCL4.2 X3J13 CLTL2)
(IN-PACKAGE "COMMON-LISP-USER")

#+ALLEGRO (EXCL:SET-CASE-MODE :CASE-INSENSITIVE-UPPER)

#-(OR ALLEGRO-V4.2 MCL LCL4.2 X3J13 CLTL2)
(error "FaCT and iFaCT are not meant to work on non-CLTL2 Common-Lisps.
Please consult the author Ian Horrocks at horrocks@cs.man.ac.uk~%
We treat the following as CLTL2: Allegro 4.2/4.3, Macintosh MCL 2.0/3.0, 
                               Lucid 4.2/Liquid 5.0, CMU Common Lisp 17f.
We consider non-CLTL2: GCL 2.2, Lucid 4.0, Xerox Medley,  LispWorks 4.1.
We do not have direct experiences with other lisps.
")

#+(OR ALLEGRO-V4.2 MCL LCL4.2 X3J13)
(pushnew :CLTL2 *features*)

#+ALLEGRO (setq excl:*ignore-package-name-case* t)
#+ALLEGRO (setq excl:*cltl1-in-package-compatibility-p* t)
#+ALLEGRO (setq comp:*cltl1-compile-file-toplevel-compatibility-p* t)
#+MCL (setq ccl::*autoload-lisp-package* t)
#+LUCID (setq lucid-common-lisp::*warn-if-no-in-package* nil)

(setq *LOAD-VERBOSE* nil)

#-XEROX
(proclaim '(optimize (speed 3)
		     (space 0)
		     (safety 0)
		     (compilation-speed 0)
	             #+ALLEGRO (debug 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The package definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (load compile eval)

  (unless (find-package "FACT")
;;;    (defpackage "FACT"
;;;      (:export "DEFPRIMROLE"
;;;	       "IMPLIES_R"
;;;	       "EQUAL_R"
;;;	       "TRANSITIVE"
;;;	       "FUNCTIONAL"
;;;
;;;	       "ALL_SUBS_R"
;;;	       "DIRECT_SUBS_R"
;;;	       "ALL_SUPERS_R"
;;;	       "DIRECT_SUPERS_R"
;;;
;;;	       "DEFPRIMCONCEPT"
;;;	       "IMPLIES_C"
;;;	       "EQUAL_C"
;;;	       
;;;	       "SATISFIABLE_C"
;;;	       "SUBSUMES_C"
;;;	       "EQUIVALENT_C"
;;;	       
;;;	       "ALL_SUBS_C"
;;;	       "DIRECT_SUBS_C"
;;;	       "ALL_SUPERS_C"
;;;	       "DIRECT_SUPERS_C"
;;;	       "TAXONOMY_POSITION"
;;;
;;;	       "DEFCONCEPT"
;;;	       "CLASSIFY-TKB"
;;;	       )))
;;;    (Make-package "FACT"))
    (rename-package "USER" (package-name "USER")
		    (cons "FACT" (package-nicknames "USER"))))
  )

(in-package "FACT")
;;;(in-package "USER")

;;;(proclaim '(special *cl-pathname-default*
;;;	    *fact-pathname-default*
;;;	    *fact-version*))

;;; According to CLTL2 the following is the aproved method for declaring special variables:
(defvar *cl-pathname-default* "Path where lisp utilities are found")
(defvar *fact-pathname-default* "Path where FaCT sources are found")
(defvar *fact-version* "FaCT version string")

(setf *fact-version*
      (format
       nil
       "~&FaCT version: ~A (~A).~%Lisp is ~A ~A,~%running on ~A, a ~A machine, equipped with ~A."
       "1.0 beta 18"
       "June, 1 1998"
       (lisp-implementation-type)(lisp-implementation-version)
       (long-site-name)(machine-type)(software-type)
       ))

#+GCL
(when (equal "T" (si::getenv "SOLARIS"))
  (pushnew :solaris *features*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Special tricky functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro format-return (text val)
  `(let ((temp ,val))
     (format t "~&~A: ~S~%" ,text temp)
     temp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Load utility packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ### Personalize the following *cl-pathname-default* pathname
;;;     to the directory where you have installed the lisp utilities
;;;     (usually "cl-utilities")

(setf *cl-pathname-default*
  #+LUCID
  (make-pathname
   :directory '(:absolute "/home/ian/lib/lisp/cl-utilities"))
  #+ALLEGRO
  (make-pathname
   :directory (pathname-directory (truename "~ian/lib/lisp/cl-utilities/")))
  #+LISPWORKS
  (make-pathname
   :directory (pathname-directory (truename "~ian/lib/lisp/cl-utilities/")))
  #+CMU
  (make-pathname
   :directory '(:absolute "home/ian/lib/lisp/cl-utilities"))
  #+(AND GCL (NOT SOLARIS)) 
  (make-pathname
   :directory '("home/ian/lib/lisp/cl-utilities"))
  #+(AND GCL SOLARIS)
  (make-pathname
   :directory '("home/ian/lib/lisp/cl-utilities"))
  #+MCL
  (make-pathname
   :directory '(:absolute "home" "ian" "lib" "lisp" "cl-utilities"))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#-:DEFSYSTEM
(load (merge-pathnames *cl-pathname-default*
		       (make-pathname :name "defsystem"
				      :type "lisp"))
      :verbose nil)

(use-package "MAKEFILE")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The cl-utilities system
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(unless makefile::*all-systems*
  (defsystem :cl-utilities
    (:default-pathname *cl-pathname-default*
		       :source-suffix "lisp")  

    ;; Do not load 'ansi-loop' if your lisp is CLtL2:
    #+(AND (NOT ANSI-LOOP) (NOT :CLTL2))
    ("ansi-loop")

    ;; Do not load 'logical-pathnames' if your lisp is CLtL2
    ;;   (with the exception of Lucid 4.2):
    #+(AND (NOT LOGICAL-PATHNAMES) (OR LCL4.2 (NOT CLTL2)))
    ("logical-pathnames")
   
    #+(AND ALLEGRO-V4.2 (NOT MCL)) ("psgraph-all42")
    #-(OR ALLEGRO-V4.2 MCL) ("psgraph")

    )
  
  (compile-system :cl-utilities :optimize t)
  (load-system :cl-utilities)

    ;; load LOOP expander
  #+ALLEGRO
  (loop for x in nil do nil)

  )

#+(OR LCL4.2 (NOT CLTL2))
(use-package "LOGICAL-PATHNAME")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Main pathnames
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ### Personalize the following *fact-pathname-default* pathname
;;;     to the directory where you have installed the code.

(setf *fact-pathname-default*
      #+LUCID
      (make-pathname
       :directory '(:absolute "/home/ian/systems/CORBA-FaCT"))
  #+ALLEGRO
  (make-pathname
   :directory (pathname-directory (truename "~ian/systems/CORBA-FaCT/")))
  #+LISPWORKS
  (make-pathname
   :directory (pathname-directory (truename "~ian/systems/CORBA-FaCT/")))
      #+CMU
      (make-pathname
       :directory '(:absolute "home/ian/systems/CORBA-FaCT"))
      #+GCL
      (make-pathname
       :directory '("/home/ian/systems/CORBA-FaCT"))
      #+MCL
      (make-pathname
       :directory '(:absolute "home" "ian" "systems" "CORBA-FaCT"))
      )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Derived pathnames
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+(OR CLTL2 (NOT CLTL2))
(setf (logical-pathname-translations "fact")
      `(("**;*.*.*"  ,(concatenate 'string 
                                   (namestring *fact-pathname-default*)
                                   #+XEROX "**>"
                                   #+UNIX  "**/"
                                   #+MCL "**:"))))

(setf *sources-pathname-default*
  #+(OR CLTL2 (NOT CLTL2))
  (pathname (translate-logical-pathname "fact:sources;"))
  #-(OR CLTL2 (NOT CLTL2))
  (make-pathname
   :directory "LENINGRAD:Lisp:fact:sources"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; THE SYSTEM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defsystem :FaCT
  (:default-pathname *sources-pathname-default*
		     :source-suffix "lisp"
		     :default-package "FACT")

  ;;; The FaCT system
  ("GNU-stuff")
  ("FaCT-reasoner")
  ("classifier")
  ("kris-interface"))

(defsystem :iFaCT
  (:default-pathname *sources-pathname-default*
		     :source-suffix "lisp"
		     :default-package "FACT")
  
  ;;; The iFaCT system
  ("GNU-stuff")
  ("iFaCT-reasoner")
  ("classifier")
  ("kris-interface"))

(defsystem :CORBA-FaCT
  (:default-pathname *sources-pathname-default*
		     :source-suffix "lisp"
		     :default-package "FACT")
  
  ;;; The CORBA-FaCT system
  ("GNU-stuff")
  ("FaCT-reasoner")
  ("classifier")
  ("CORBA-interface"))

(defsystem :CORBA-iFaCT
  (:default-pathname *sources-pathname-default*
		     :source-suffix "lisp"
		     :default-package "FACT")
  
  ;;; The CORBA-i FaCT system
  ("GNU-stuff")
  ("iFaCT-reasoner")
  ("classifier")
  ("CORBA-interface"))

(defsystem :DLR
  (:default-pathname *sources-pathname-default*
		     :source-suffix "lisp"
		     :default-package "FACT")
  
  ;;; The DLR system
  ("GNU-stuff")
  ("iFaCT-reasoner")
  ("classifier")
  ("DLR-interface"))
