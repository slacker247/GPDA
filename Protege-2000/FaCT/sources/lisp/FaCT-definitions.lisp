;;; -*- Mode: Lisp; package: FACT; Syntax: COMMON-LISP; Base: 10 -*-

;;; FaCT COPYRIGHT (C) 1997, 1998, 1999 IAN R. HORROCKS
;;; and THE UNIVERSITY OF MANCHESTER, horrocks@cs.man.ac.uk
;;; Time-stamp: Tue Jun 15 17:41:50 BST 1999

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                      ;;;
;;; FaCT description logic classifiers                                   ;;;
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
;;; GNU General Public License for more details.             *version-number*            ;;;
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

(IN-PACKAGE "COMMON-LISP-USER")

#+ALLEGRO (EXCL:SET-CASE-MODE :CASE-INSENSITIVE-UPPER)

#+ALLEGRO (setq excl:*ignore-package-name-case* t
		excl:*cltl1-in-package-compatibility-p* t
		comp:*cltl1-compile-file-toplevel-compatibility-p* t
		*LOAD-VERBOSE* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The package definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (load compile eval)
  (unless (find-package "FACT")
    #+X3J13 (defpackage "FACT")
    #-X3J13 (make-package "FACT")
    ))

(in-package "FACT")

(defconstant *version-number* "2.32")
(defvar *verbose-description* "Verbose system description (version etc.)")
