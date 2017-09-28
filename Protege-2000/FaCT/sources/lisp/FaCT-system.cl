;;; -*- Mode: Lisp; package: FACT; Syntax: COMMON-LISP; Base: 10 -*-

;;; FaCT and iFaCT COPYRIGHT (C) 1997, 1998, 1999 IAN R. HORROCKS
;;; and THE UNIVERSITY OF MANCHESTER, horrocks@cs.man.ac.uk
;;; Time-stamp: Sat Sep 18 10:54:57 BST 1999

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

#+ALLEGRO (EXCL:SET-CASE-MODE :CASE-INSENSITIVE-UPPER)

#-ALLEGRO
(error "This version of the FaCT and iFaCT system definition is only
meant to work with Allegro Common Lisp.
")

(IN-PACKAGE "COMMON-LISP-USER")

;;;(pushnew :CLTL2 *features*)

(setq excl:*ignore-package-name-case* t)
(setq excl:*cltl1-in-package-compatibility-p* t)
(setq comp:*cltl1-compile-file-toplevel-compatibility-p* t)

(setq *LOAD-VERBOSE* nil)

(proclaim '(optimize (speed 3)
		     (space 0)
		     (safety 0)
		     (compilation-speed 0)
	             (debug 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The package definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (load compile eval)
  (unless (find-package "FACT")
    (make-package "FACT"))
;;;    (rename-package "USER" (package-name "USER")
;;;		    (cons "FACT" (package-nicknames "USER"))))
  )

;;;(in-package "FACT")

;;; According to CLTL2 the following is the aproved method for declaring special variables:
;;;(defvar fact::*fact-sources-pathname-default* "Path where FaCT sources are found")
;;;(defvar fact::*fact-pathname-default* "Path where FaCT system is found")
;(defvar *fact-version* "FaCT version string")

;(setf *fact-version*
;      (format
;       nil
;       "~&FaCT version: ~A (~A).~%Lisp is ~A ~A,~%running on ~A, a ~A machine, equipped with ~A."
;       "1.0 beta 18"
;       "June, 1 1998"
;       (lisp-implementation-type)(lisp-implementation-version)
;       (long-site-name)(machine-type)(software-type)
;       ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Special tricky functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(defmacro format-return (text val)
;  `(let ((temp ,val))
;     (format t "~&~A: ~S~%" ,text temp)
;     temp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Main pathnames
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ### Personalize the following *fact-pathname-default* pathname
;;;     to the directory where you have installed the code.

(setf *fact-pathname-default*
  (make-pathname
   :device (pathname-device *load-truename*)
   :directory (pathname-directory *load-truename*))
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

(setf *fact-sources-pathname-default* *fact-pathname-default*)
;;;  (pathname (translate-logical-pathname "fact:sources;")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; THE SYSTEM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(excl:defsystem :FaCT
    (:default-pathname #.*fact-sources-pathname-default*
	:default-file-type "lisp" :default-package "FACT")
  (:serial
   "FaCT-definitions"
   "GNU-stuff" 
   "FaCT-reasoner" 
   "classifier" 
   "kris-interface"
   "initialise"))

(excl:defsystem :SHIQ
    (:default-pathname #.*fact-sources-pathname-default*
	:default-file-type "lisp" :default-package "FACT")
    (:serial
   "FaCT-definitions"
   "GNU-stuff" 
   "SHIQ-reasoner" 
   "classifier" 
   "kris-interface"
   "initialise"))

(excl:defsystem :CORBA-FaCT
    (:default-pathname #.*fact-sources-pathname-default*
	:default-file-type "lisp" :default-package "FACT")
    (:serial
   "FaCT-definitions"
   "GNU-stuff" 
   "FaCT-reasoner" 
   "classifier" 
   "CORBA-interface"
   "initialise"))

(excl:defsystem :CORBA-SHIQ
    (:default-pathname #.*fact-sources-pathname-default*
	:default-file-type "lisp" :default-package "FACT")
    (:serial
   "FaCT-definitions"
   "GNU-stuff" 
   "SHIQ-reasoner" 
   "classifier" 
   "CORBA-interface"
   "initialise"))

(excl:defsystem :DLR
    (:default-pathname #.*fact-sources-pathname-default*
	:default-file-type "lisp" :default-package "FACT")
    (:serial
   "FaCT-definitions"
   "GNU-stuff" 
   "SHIQ-reasoner" 
   "classifier" 
   "DLR-interface"
   "initialise"))

(defun make-system (system &key (clean nil))
  (if clean (excl:clean-system system))
  (excl:load-system system :interpreted T)
  (excl:compile-system system)
  (excl:load-system system :reload T))
