(defrule Opcode_0_Init_SC          ;Indicates what to do to initialize 
      (object (is-a MESSAGE))
      (object (Opcode 0))
      ?ins <-  (object)
      ?SC_node <- (object (node))  ;need to prepare a single message, no matter what it is and send it on
                                   ;so can pick up any information that may be there in general
                                   ; cannot depend of what is happening in Translator case statement
                                   ;calls in Translator case statement should be used to construct that part
                                   ;of the common message.
=>
      

