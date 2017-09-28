(defclass MESSAGE "Defines the basic structure and methods used in communicating with Battle Planner"
      (is-a USER)                  ;USER is the top level user defined superclass
      (role concrete)              ;Allows the class to have direct instances
      (pattern-match reactive)     ;Any changes made to an instance will cause pattern matching to occur.
      
      (slot SourceID 
            (type SYMBOL))   
      (slot DestinationID
            (type INTEGER))
      (slot SCID
            (type INTEGER))
      (slot Opcode
            (type INTEGER))
      (slot SCActive
            (type INTEGER))
      (slot CurrentTime
            (type FLOAT))
      (slot Reserved7
            (type INTEGER))
      (slot Reserved8
            (type INTEGER))
      (slot Reserved9
            (type INTEGER))

;      (message-documentation
;            handler-type)
            )

