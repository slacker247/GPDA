
   
(defrule change_states
      (declare (salience -1000))
      ?now <- (state ?current)
      ?change <- (change_state ?current ?to)
=>
      (retract ?now)
      (retract ?change)
      (assert (state ?to)))

(defrule mission_planning
      ?state <- (state mission_planning)
=>
      (retract ?state)
      (assert (state initial))
      (printout t "Mission planning state" crlf))

(defrule execution_planning
      ?state <- (state execution_planning)
=>
      (retract ?state)
      (assert (state initial))
      (printout t "Execution planning state" crlf))

(defrule execute
      ?state <- (state execute)
=>
      (retract ?state)
      (assert (state initial))
      (printout t "Execute state" crlf))

(defrule terminate
      ?state <- (state terminate)
=>
      (retract ?state)
      (assert (state initial))
      (printout t "Terminate state" crlf))