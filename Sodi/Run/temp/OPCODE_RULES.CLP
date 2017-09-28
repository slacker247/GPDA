(defrule  OP_INIT_SC
	(object (is-a HEADER))
	(object (opcode 0))		;opcode 0 is Initialize SC
	(object (destination [SC]))
	?node <- (send [SC] get-node_type)
	?position <- (send [SC] get-position_type)
=>
	(assert (node ?node))
	(assert (position ?position))
	(printout t "SC node is "?node crlf)
	(printout t "SC position is "?position crlf))

(defrule DEFCON_authority
	(node BMDC)
	(position DR)
=>
	(assert (authority DEFCON))
	(printout t "Authority to change DEFCON granted." crlf))
	
(defrule  OP_DB_STATUS

=> )
(defrule OP_DEFCON 

=> )

(defrule  OP_RP

=> )
(defrule OP_DEA

=> )
(defrule OP_ROE

=> )
(defrule  OP_MSN_OBJ

=> )

(defrule  OP_BP

=> )

(defrule OP_WA

=> )

(defrule OP_TRK_ENGMT 

=> )
(defrule  OP_POT_EVENT

=> )
(defrule OP_INIT_MO

=> )

(defrule OP_INIT_BP

=> )
(defrule OP_INIT_WA

=> )











