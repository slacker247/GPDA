(deffacts Authority
      (authority DEFCON)
      (authority MSN_OBJ)
      (authority BP)
      (authority DEA)
      (authority RP)
      (authority PP)
      (authority BMC2)
      (authority BP_Creation)
      (authority Oper_Auth))

(defrule DEFCON_1
      (authority DEFCON)
      ?level <- (send [current_state] get-defcon)

