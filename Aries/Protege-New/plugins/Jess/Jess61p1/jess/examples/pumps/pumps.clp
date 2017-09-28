;; -*- clips -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Free-standing Pumps and Tanks control example. The damped algrithm
;; coded in this file works reasonably well and can protect both Tanks
;; indefinitely.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Register Java classes for matching like deftemplates.
;; Bean-like properties become slots. Classes must support
;; addPropertyChangeListener. First argument is the 'deftemplate name'.

(import jess.examples.pumps.*)
(import javax.swing.*)
(import java.awt.*)

;;(defclass machine jess.examples.pumps.Machine )
(deftemplate machine (slot name) (slot class) (slot OBJECT))
(defclass tank Tank extends machine)
(defclass pump Pump extends machine)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This fact will be used to sleep when idle

(deffacts idle-fact
  (idle 0)
  (adjust-time 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; If tank is low, raise appropriate pump's pumping rate
;; Don't raise it too high!
;; Notice how we can call methods of the objects we match, like Pump.setFlow()

(defrule warn-if-low
  (tank (name ?name) (low TRUE) (intact TRUE))
  (not (warning low ?name))
  =>
  (assert (warning low ?name))
  (printout t "WARNING: TANK " ?name " IS LOW!" crlf)
  )

(defrule raise-rate-if-low
  ?warning <- (warning low ?name)
  (pump (name ?name) (flow ?flow-rate) (OBJECT ?pump))
  (test (< ?flow-rate 25))
  (idle ?n)
  ?a <- (adjust-time ?t&:(< ?t (- ?n 20)))
  =>
  (retract ?warning)
  (set ?pump flow (+ ?flow-rate 1))
  (assert (adjust-time ?n))
  (retract ?a)
  (printout t "Raised pumping rate of pump " ?name " to "
            (get ?pump flow) crlf))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; If tank is high, lower appropriate pump's pumping rate
;; Don't lower it too much.

(defrule warn-if-high
  (tank (name ?name) (high TRUE) (intact TRUE))
  (not (warning high ?name))
  =>
  (assert (warning high ?name))
  (printout t "WARNING: TANK " ?name " IS HIGH!" crlf)
  )

(defrule lower-rate-if-high
  ?warning <- (warning high ?name)
  (pump (name ?name) (flow ?flow-rate) (OBJECT ?pump))
  (test (> ?flow-rate 0))
  (idle ?n)
  ?a <- (adjust-time ?t&:(< ?t (- ?n 20)))
  =>
  (retract ?warning)
  (retract ?a)
  (set ?pump flow (- ?flow-rate 1))
  (assert (adjust-time ?n))
  (printout t "Lowered pumping rate of pump " ?name " to "
            (get ?pump flow) crlf))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; If tank is ok, sleep for 100 milliseconds
;; Notice outcall to Java static method!

(defrule notify-if-ok
  ?warning <- (warning ? ?name)
  (tank (name ?name) (high FALSE) (low FALSE))
  =>
  (retract ?warning)
  (printout t "Tank " ?name " is now OK." crlf))

(defrule sleep-if-bored
  (declare (salience -100))
  ?idle <- (idle ?n)
  =>
  (retract ?idle)
  (call Thread sleep 25)
  (assert (idle (+ ?n 1))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; If tank is damaged, report it

(defrule report-fire
  ?t <- (tank (name ?name) (low TRUE) (intact FALSE))
  =>
  (printout t "*********************************************" crlf)
  (printout t "* Tank " ?name " has run dry and caught fire.    " crlf)
  (printout t "*********************************************" crlf)
  (retract ?t)
  (halt))

(defrule report-explosion
  ?t <- (tank (name ?name) (high TRUE) (intact FALSE))
  =>
  (printout t "*********************************************" crlf)
  (printout t "* Tank " ?name " has overfilled and exploded " crlf)
  (printout t "*********************************************" crlf)
  (retract ?t)
  (halt))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Announce new machine. This is just to demonstrate
;; how inheritance works.

(defrule announce-new-machinery
  ?m <- (machine (name ?n) (class ?c))
  (not (saw-machine ?n ?c))
  =>
  (assert (saw-machine ?n ?c))
  (printout t "*** New Machine *** named " ?n " of type " (call ?c toString) crlf))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Create hardware, register for matching
;; Create two pumps and two tanks
;; Notes:
;; 1) See how we use the 'deftemplate name' from the defclass calls.
;; 2) Objects in definstance must be assignable to the type named in defclass
;; 3) The semantics here are different that definstances in CLIPS - this allows
;;    us to use pre-existing objects.

(defrule startup
  =>
  (bind ?frame (new JFrame "Pumps Demo"))
  (call (?frame getContentPane) setLayout (new GridLayout 2 3))
  (definstance tank (bind ?tank (new Tank "MAIN")))
  (call (?frame getContentPane) add (?tank getComponent))
  (definstance pump (bind ?pump (new Pump "MAIN" ?tank)))
  (call (?frame getContentPane) add (?pump getComponent))
  (call (?frame getContentPane) add (new JLabel "MAIN"))
  (definstance tank (bind ?tank (new Tank "AUX")))
  (call (?frame getContentPane) add (?tank getComponent))
  (definstance pump (bind ?pump (new Pump "AUX" ?tank)))
  (call (?frame getContentPane) add (?pump getComponent))
  (call (?frame getContentPane) add (new JLabel "AUX"))
  (?frame pack)
  (?frame setVisible TRUE))



(reset)
(run)

