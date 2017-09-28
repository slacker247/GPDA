(defrule implement_DEFCON_change "describes the actions to take when a DEFCON change is indicated"
      (change defcon)
      (defcon_authorization granted)
      ?level <- (defcon level)

      
=>
;Create part of a return message that indicates the change in DEFCON and the state of 
;acknowledgements. 
;Want to finish all the appropriate updates before sending the message, but need to work
;within time constraint.





            