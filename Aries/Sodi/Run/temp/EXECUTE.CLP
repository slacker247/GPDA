;Execute Directive (CCSIL Message #103)
;The Execute Directive message is actually a family of 13 separate 
;directives that a commander can use to change the current behavior
;of a unit.  The changes that can be accomplished by the execute 
;Directive messages are simple and are intended to be short-lived.
;More complex or far reaching changes require a Fragmentary Order 
;or an Operation Order message.

;Field Names: header, directive, unit_name_list
;Corresponding Data Types: CFOR_CCSIL_MSG_HEADER_STRUCT,
;CFOR_DIRECTIVE_TYPE, CFOR_NAME_STRUCT

;header - Specifies the sender of this message as well as all units
;that are explicitly intended to be recipients.

;directive - An instruction to the subordinate or  supporting
;unit

;unit_name_list - The names of the units to which this directive 
;applies.

;directive = START
;The START Directive can be used to cause a unit to begin conducting 
;a phase and mission as specified in a previous Operations Order.  
;This message is used by the commander to control the sequence of
;phases and messions that his unit is conducting.  The phase and 
;mission numbers given must corespond to the phase and mission
;numbers given in the most recent Operations Order.

;Field Names: order, phase_number, mission_number, task_number
;Corresponding Data Types: CFOR_ORDER_TYPE, INTEGER, INTEGER, 
;INTEGER

;order - The type of order that the receiving unit should refer to
;when initiating the specified phase-mission-task combination.

;phase_number - A reference to one of the phases in the Concept of
;Operation or Subordinate Unit Plans subparagraph.

;mission_number - A reference to one of the missions in the Concept of
;Operation or Subordinate Unit Plans subparagraph.

;task_number - A reference to one of the tasks in the Concept of
;Operation or Subordinate Unit Plans subparagraph.


 
