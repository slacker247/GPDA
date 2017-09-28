;; -*- clips -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pumps and Tanks control example, used by the MainInJava class. The
;; simple algrithm coded in this file isn't good enough to control the
;; Pump and the Tank will explode on the first cycle.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Register Java classes for matching like deftemplates.
;; Bean-like properties become slots. Classes must support
;; addPropertyChangeListener. First argument is the 'deftemplate name'.

(defclass tank jess.examples.pumps.Tank )
(defclass pump jess.examples.pumps.Pump )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This fact will be used to sleep when idle

(deffacts idle-fact
  (idle))

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
  =>
  (retract ?warning)
  (set ?pump flow (+ ?flow-rate 5))
  (printout t "Raised pumping rate of pump " ?name " to "
            (get ?pump flow) crlf))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; If tank is low, raise appropriate pump's pumping rate
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
  (test (> ?flow-rate 5))
  =>
  (retract ?warning)
  (set ?pump flow (- ?flow-rate 5))
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
  ?idle <- (idle)
  =>
  (retract ?idle)
  (call java.lang.Thread sleep 100)
  (assert (idle)))
  
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

;; Actual creation of objects, as well as (reset) and (run) calls,
;; must be made form Java code that runs this batch file.