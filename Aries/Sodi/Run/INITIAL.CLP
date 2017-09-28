
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;FILE 		INITIAL.CLP
;;Created by	Mary L.  Cheek
;;Created	Sept.  , 1998
;;Modified	June 16, 1999
;;Use		This file contains all the constructs used by FuzzyCLIPS to
;;drive the decision making of a prototyped NMD Simulated Commander.  The file
;;is loaded into the FuzzyCLIPS environment upon the first call to the
;;translator function in translator.c.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;This section defines the modules of the SCs created for the demo.
;;Modules are used so that an individual SC has a separate fact list.  The acronyms for
;;the names came from the NMD BMC3 CI3 DDD.
;;In order for information to be shared between modules, the constructs must be
;;exported and imported.  Modules are the basis of using FuzzyCLIPS as a
;;blackboard.

(defmodule MAIN (export ?ALL))

(defmodule CCC_DO (import MAIN deftemplate ?ALL)
		  (import MAIN defclass ?ALL)
		  (import MAIN defglobal ?ALL))

(defmodule BMDC_DR (import MAIN deftemplate ?ALL)
		   (import MAIN defclass ?ALL)
		   (import MAIN defglobal ?ALL))

(defmodule FU_DR (import MAIN deftemplate ?ALL)
		 (import MAIN defclass  ?ALL)
		 (import MAIN defglobal ?ALL))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;These constructs are used to define and manipulate phase changes during each
;;call from the translator.  The initialize phase is asserted in the
;;translator.  The salience of the change-phase rule is low so that it will be
;;the last rule to fire in that phase.  Phases are used to add procedural
;;control to the firing of rules.  this usage of phase control was taken from
;;the book "Expert Systems principles and Programming" by Giarratano page 488.

(deftemplate MAIN::phase_control
	(slot phase)
	(multislot phase-after))

(deffacts MAIN::control-information
	(phase_control (phase-after initialize monitor))
	(phase_control (phase-after monitor assess))
	(phase_control (phase-after assess replan))
	(phase_control (phase-after replan execute)))

(defrule MAIN::change-phase
	(declare (salience -100))
	?phase <- (phase_control (phase ?current-phase))
	(phase_control (phase-after ?current-phase ?next-phase))
=>
	(retract ?phase)
	(assert (phase_control (phase ?next-phase))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;This section contains class definitions in CLIPS Object Oriented Language
;;(COOL) for all the types of messages passed from the translator to the 
;;FuzzyCLIPS environment.  Work still needs to be done in order to make the 
;;design of SC Object Oriented. Information in instances is available for
;;pattern matching like facts on the fact list.  The system class USER provides
;;message-handlers for such things as initialization and deletion.  The best
;;source of information about COOL is the CLIPS Programmer's Guide chapter 9.

;;The messages were patterned after screens described in BMC3 CI3 DDD, and what
;;could be provided by the demo Battle Planner. Note some fields and many 
;;create-accessors are not used.

;;Instances of the STATE class are used to return decisions to the Battle
;;Planner and to provide state information to SC upon return to an active
;;status. The instance is deleted after the message is processed.  The
;;information remains in the updated global variables.
 
(defclass MAIN::STATE (is-a USER)
      (role concrete) (pattern-match reactive)
      (slot DEFCON (type INTEGER) (allowed-values 1 2 3 4 5)(create-accessor read-write))
      (slot RP (type INTEGER) (allowed-values 1 2)(create-accessor read-write))
      (slot DEA (type STRING) (create-accessor read-write))
      (slot ROE (type STRING) (create-accessor read-write))
      (slot Msn_Obj_Name (type STRING) (create-accessor read-write))
      (slot BP_Name (type STRING)(create-accessor read-write)))

;;Instances of the MSN_OBJ (mission objective) class are used to help determine
;;if enough weapons will be available to fight the battle.
;;There are assumed to be MSN_OBJ with choices of two values for name slots. 
;;Upon initialization of MSN_OBJ, a copy is made with a gen-sym 
;;(general symbol) name and the original instance is deleted.

(defclass   MAIN::MSN_OBJ (is-a USER)
      (role concrete)(pattern-match reactive)
      (slot msn_obj_name (type STRING)(create-accessor read-write))	
      (slot strategy (type STRING) (create-accessor read-write))
      (slot tactic (type STRING) (create-accessor read-write))
      (slot pms (type FLOAT) (range 0.0 1.0)(create-accessor read-write))
      (slot mode (type INTEGER) (allowed-values 1 2 3)(create-accessor read-write))
      (slot withhold (type INTEGER)(create-accessor read-write)) 
      (slot add_bstrs (type INTEGER)(create-accessor read-write)))


;;Instances of the BATTLE_PLAN class are used for initialization only.  Copies
;;of the instances are made and kept active thoughout the demo, but they are
;;not used.  There are no assumptions about how many battle plans are passed.

(defclass   MAIN::BATTLE_PLAN (is-a USER)
      (role concrete)(pattern-match reactive)
      (slot bp_name (type STRING)(create-accessor read-write)) 	
      (slot planner_mode (type STRING)(create-accessor read-write))
      (slot target_value_cutoff (type STRING) (create-accessor read-write))
      (slot accept_kill_criteria (type STRING)(create-accessor read-write))
      (slot override_salvo (type INTEGER) (create-accessor read-write))
      (slot launch_mode (type INTEGER) (allowed-values 1 2 3)(create-accessor read-write))
      (slot rv_threshold (type FLOAT) (range 0.0 1.0)(create-accessor read-write))
      (slot pk_cutoff (type FLOAT) (range 0.0 1.0)(create-accessor read-write))
      (slot weight_population (type INTEGER) (create-accessor read-write))
      (slot weight_military (type INTEGER)(create-accessor read-write))
      (slot weight_selfdefense (type INTEGER)(create-accessor read-write))
      (slot weight_ncauthority (type INTEGER)(create-accessor read-write))
      (slot weight_industrial (type INTEGER)(create-accessor read-write)))

;;Instances of the PLAYER class are used to identify NMD Simulated Commanders.
;;The possible SC roles come from BMC3 CI3 DDD. These do not coincide with
;;WG2K NMD players.

(defclass  MAIN::PLAYER (is-a USER)
	(role concrete)(pattern-match reactive)
	(slot node_type (type STRING)(create-accessor read-write))
	(slot node_number (type INTEGER)(create-accessor read-write))
	(slot position_type (type STRING)(create-accessor read-write))
	(slot position_number (type INTEGER)(create-accessor read-write))
	(slot active (type INTEGER)(create-accessor read-write)))

;;Instances of the ENGAGEMENT class are used to determine if changes in mission
;;objectives or battle plan (later) is needed.  When a new engagement is
;;detected it is copied with a gen-sym name, and updated on subsequent messages
;;that involve the engagement with the same id values.

(defclass  MAIN::ENGAGEMENT (is-a USER)
	(role concrete)(pattern-match reactive)
	(slot gbi_task_id (type INTEGER) (create-accessor read-write))
	(slot tgt_trk_id (type STRING) (default "11:22:33") (create-accessor read-write))
	(slot eng_status (type STRING)(create-accessor read-write))
	(slot cur_eng_num_weapons (type INTEGER)(create-accessor read-write))
	(slot cur_eng_tti (type FLOAT)(create-accessor read-write))
	(slot cur_eng_pk (type FLOAT)(create-accessor read-write))
	(slot eng_opp_rem (type INTEGER)(create-accessor read-write)))

;;Instances of the MISSILE_TRACK class are used to set appropriate ROE and 
;;DEFCON.When a new track is detected it is copied with a gen-sym name, and 
;;updated on subsequent messages that involve the track with the same id.  

(defclass  MAIN::MISSILE_TRACK (is-a USER)
	(role concrete)(pattern-match reactive)
	(slot id (type STRING)(create-accessor read-write))
	(slot obj_type (type STRING)(create-accessor read-write))
	(slot missile_type (type STRING)(create-accessor read-write))
	(slot missile_class (type STRING)(create-accessor read-write))
	(slot exp_targets (type INTEGER) (create-accessor read-write))
	(slot leth_val (type FLOAT)(allowed-values 0.0 1.0)(create-accessor read-write))
	(slot launch_time (type FLOAT)(create-accessor read-write))
	(slot launch_country (type STRING)(create-accessor read-write))
	(slot launch_site (type STRING)(create-accessor read-write))
	(slot imp_lat (type FLOAT)(create-accessor read-write))
	(slot imp_long (type FLOAT)(create-accessor read-write))
	(slot earliest_imp_time (type FLOAT)(create-accessor read-write))
	(slot predicted_imp_reg (type STRING)(create-accessor read-write))
	(slot num_bstrs (type INTEGER)(create-accessor read-write))
	(slot lethals_exp (type INTEGER)(create-accessor read-write))
	(slot track_status (type STRING)(create-accessor read-write)))


;;Instances of the POTENTIAL_EVENT class are used for evaluating the perceived
;;threat from quick alerts.  

(defclass MAIN::POTENTIAL_EVENT (is-a USER)
	(role concrete) (pattern-match reactive)
	(slot event_time (type FLOAT) (create-accessor read-write))
	(slot event_country (type STRING) (create-accessor read-write))
	(slot event_site (type STRING) (create-accessor read-write))
	(slot event_lat (type FLOAT) (create-accessor read-write))
	(slot event_long (type FLOAT) (create-accessor read-write))
	(slot event_status (type STRING) (create-accessor read-write))
	(slot value (type FLOAT) (create-accessor read-write))
	(slot persistence (type FLOAT) (default 0.5) (create-accessor read-write)))


;;Instances of the GBI_FARM class are used as slot values for the WEAPONS
;;class.  The number of weapons is changed by Battle Planner and updated in
;;translator.c.  

(defclass  MAIN::GBI_FARM (is-a USER)
	(role concrete) (pattern-match reactive)
	(slot id (type INTEGER))
	(slot weapons (type INTEGER))
	(slot weapons_held (type INTEGER)))

;;Weapons are structured in this way to match the double array structure used
;;in translator.c.

(defclass MAIN::WEAPONS (is-a USER)
	(role concrete) (pattern-match reactive)
	(multislot gbi_farms (type INSTANCE-NAME)(create-accessor read-write))
	(message-handler put-next))

;;WEAPONS uses the message handler put-next in the for loop of translator.c
;;during initialization.  The variable ?value is copied and given a gen-sym
;;name for the instance of GBI_FARM that is being updated.

(defmessage-handler MAIN::WEAPONS put-next primary (?value)
	(bind ?x (duplicate-instance ?value))
	(slot-direct-insert$ gbi_farms (+ (length$ gbi_farms) 1) ?x)
	(send ?value delete))

;;Instances of the INTEL_INFO class are used to represent the perceived threat
;;from intelligence messages.  The value slot has a number from 0 to 100 to
;;indicate the strength of the perceived threat. The persistence slot gives a
;;time value measured in hours.  The time slot has time of creation of INFO 
;;measured in hours relative to gvt = 0.  The degrade-value method supplies
;;a value to degraded slot that is the value slot exponentially degraded with
;;respect to persistence.

(defclass MAIN::INTEL_INFO (is-a USER)
	(role concrete) (pattern-match reactive)
	(slot source_type (type STRING) (create-accessor read-write))
	(slot source_num (type INTEGER) (create-accessor read-write))
	(slot value (type FLOAT) (create-accessor read-write))
	(slot persistence (type FLOAT) (create-accessor read-write))
	(slot time (type FLOAT) (create-accessor read-write))
	(slot degraded (type FLOAT) (create-accessor read-write))
	(message-handler degrade-value))

(defmessage-handler MAIN::INTEL_INFO degrade-value primary (?gvt)
	(bind ?x (/ ?gvt 3600.0))
	(bind ?x (- ?self:persistence (- ?x ?self:time)))
	(if (<= 0 ?x)
	  then 
	     (bind ?self:degraded 0.0)
	  else 
	     (bind ?self:degraded (* ?self:value (exp (/ ?x ?self:persistence))))))

;;Instances of the INTEL_MSG class represent individual instances of intel
;;messages.  Each message can have multiple sources.  The put-next method is
;;used to handle htese multiple sources.  The time-since method is used to
;;calculate the creation time of the message measured in hours relative to
;;gvt = 0.  This time is the value of the INTEL_INFO time slot.
 
(defclass MAIN::INTEL_MSG (is-a USER)
	(role concrete) (pattern-match reactive)
	(slot day (type INTEGER) (create-accessor read-write))
	(slot time (type FLOAT) (create-accessor read-write))
	(multislot info (type INSTANCE-NAME) (create-accessor read-write))
	(message-handler put-next)
	(message-handler time-since)) 

(defmessage-handler MAIN::INTEL_MSG put-next primary (?value)
	(bind ?x (duplicate-instance ?value))
	(slot-direct-insert$ info (+ (length$ info) 1) ?x)
	(send ?value delete))

(defmessage-handler MAIN::INTEL_MSG time-since primary ()
	(/ (+ (* ?self:day 86400) (* ?self:time 3600)) 3600))

;;Instances of STRING_PROMPT and INTEGER_PROMPT are used as message bodies for
;;passing a single piece of information.

(defclass  MAIN::STRING_PROMPT (is-a USER)
	(role concrete) (pattern-match reactive)
	(slot prompt (type STRING)(create-accessor read-write)))

(defclass  MAIN::INTEGER_PROMPT (is-a USER)
	(role concrete)(pattern-match reactive)
	(slot prompt (type INTEGER)(create-accessor read-write)))

;;Instances of MSG_HEADER class are used to identify source and destination
;;players, type of message, and the global virtual time (gvt) of the message.

(defclass  MAIN::MSG_HEADER (is-a USER)
	(role concrete) (pattern-match reactive)
	(slot destination_id (type INSTANCE-NAME)(create-accessor read-write))
	(slot source_id (type INSTANCE-NAME)(create-accessor read-write))
        (slot opcode (type INTEGER)(create-accessor read-write))
	(slot gvt (type FLOAT)(create-accessor read)))

;;INT_HEADER class represent internal messages.  These are required because
;;only one SC can be logged into the Battle Planner at a time.  Thus the 
;;internal messages are distinquished from those that interact with the BP.
;;the ack slot is added for convenience, so a separate message body does not 
;;need to be created.

(defclass MAIN::INT_HEADER (is-a MSG_HEADER)
	(slot ack (create-accessor read-write)))

(defclass  MAIN::MESSAGE (is-a USER)
	(role concrete) (pattern-match reactive)
	(slot message_header (type INSTANCE-NAME)(create-accessor read-write))
	(slot msg_body (type INSTANCE-NAME)(create-accessor read-write)))

(defclass MAIN::INT_MESSAGE (is-a MESSAGE))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;This section contains the fuzzy set templates and the templates of facts 
;;that have fuzzy slots.  The z, s, and pi indicate built in fuzzy set
;;membership functions, and are described on page 35 of "FuzzyCLIPS Version
;;6.04A User's Guide".

;;The threat indicator templates give fuzzy set representations of crisp values
;;that are associated with given threats.

(deftemplate MAIN::Intel_Threat_Indicator
	0 100
	( (low (z 10 20))
	  (moderate (pi 10 25))
	  (high (s 35 55))))

(deftemplate MAIN::Potential_Threat_Indicator
	0 100
	( (low (z 10 30))
	  (moderate (pi 15 40))
	  (high (s 65 80))))

(deftemplate MAIN::Missile_Threat_Indicator
	0 100
	( (low (pi 0 0))
	  (high (pi 0 100))))

;;The threat templates provide vehicles to represent the identity of the threat
;;with the fuzzy set representation.
 
(deftemplate MAIN::Intel_Threat
	(slot source) (slot number) (slot time) (slot threat (type FUZZY-VALUE Intel_Threat_Indicator)))

(deftemplate MAIN::Potential_Threat
	(slot country) (slot event_time) (slot threat (type FUZZY-VALUE Potential_Threat_Indicator)))

(deftemplate MAIN::Missile_Threat
	(slot trk_id) (slot threat (type FUZZY-VALUE Missile_Threat_Indicator)))

;;The total threat templates are used to fuse the individual threats by fuzzy
;;union.

(deftemplate MAIN::Total_Intel_Threat
	(slot threat (type FUZZY-VALUE Intel_Threat_Indicator)))

(deftemplate MAIN::Total_Potential_Threat
	(slot threat (type FUZZY-VALUE Potential_Threat_Indicator)))

(deftemplate MAIN::Total_Missile_Threat
	(slot threat (type FUZZY-VALUE Missile_Threat_Indicator)))

(deftemplate MAIN::Threat_Severity_Level
	0 1
	( (Threat_Level_5 (z .1 .2))
	  (Threat_Level_4 (pi .15 .3))
	  (Threat_Level_3 (pi .15 .5))
	  (Threat_Level_2 (pi .15 .7))
	  (Threat_Level_1 (s .82 .9))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;These templates are for facts that need to be shared among modules.

(deftemplate MAIN::ROE
	(slot roe))

(deftemplate MAIN::Change
	(slot propose))

(deftemplate MAIN::Return
	(slot ack))

(deftemplate MAIN::Weapons
	(slot id) (slot msn_obj) (slot wa))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;This section represents information that is known about the world at the time
;;the simulation starts.  Deffacts are put on the facts list when the 
;;FuzzyCLIPS environment is reset.  Hostility and Events are used to compute
;;the potential threat from a Potential Event message.  The value given is a 
;;membership value in the Potential_Threat_Indicator fuzzy template.

(deffacts MAIN::Hostility
	(country "N. Korea" 40)
	(country "China" 30)
	(country "Russia" 40)
	(country "Open Sea" 40)
	(country "Others" 5))

(deffacts MAIN::Events
	(event "Quick Alert" 30)
	(event "Missile Event" 80))


(defglobal MAIN
	?*defcon* = 5
	?*rp* = 2
	?*dea* = "Hold"
	?*roe* = "N. America"
	?*msn_obj* = "Current"
	?*bp* = "General Umbrella")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Constructs belonging to the BMDC_DR MODULE.  The function is used only for
;;a BMDC_DR rule.  The global variables are used to propose changes and are
;;accessed only within the module.  The module has its own authorization list,
;;and its own list of contacts with superiors and subordinates identified.

(deffunction BMDC_DR::total_slot_value (?class ?slot)
	(bind ?total 0)
	(do-for-all-instances ((?ins ?class)) TRUE
		(bind ?total (+ ?total ?ins:?slot)))
	?total)

(defglobal BMDC_DR
	?*bmdc_defcon* = 5
	?*bmdc_rp* = 2
	?*bmdc_roe* = "N. America"
	?*bmdc_dea* = "Hold"
	?*bmdc_msn_obj* = "Current"
	?*bmdc_bp* = "General Umbrella")

(deftemplate BMDC_DR::Authority
      (slot change))

(deffacts BMDC_DR::Authority
      (Authority (change DEFCON))
      (Authority (change MSN_OBJ))
      (Authority (change BP))
      (Authority (change DEA))
      (Authority (change RP)))

(deftemplate BMDC_DR::Contact
	(slot hierarchy)
	(multislot ID))
  
(deffacts BMDC_DR::Contact
	(Contact (hierarchy superior) (ID "CCC" "DO"))
	(Contact (hierarchy subordinate) (ID "FU" "DR")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Constructs for the CCC_DO module.  Comments are similar to those of BMDC_DR.

(defglobal CCC_DO
	?*ccc_defcon* = 5
	?*ccc_rp* = 2
	?*ccc_dea* = "Hold"
	?*ccc_msn_obj* = "Current"
	?*ccc_bp* = "General Umbrella"
	?*ccc_roe* = "N. America")

(deftemplate CCC_DO::Authority
      (slot change))

(deffacts CCC_DO::Authority
      	(Authority (change DEFCON))
      	(Authority (change DEA))
      	(Authority (change RP))
      	(Authority (change ROE)))

(deftemplate CCC_DO::Contact
	(slot hierarchy)
	(multislot ID))
  
(deffacts CCC_DO::Contact
	(Contact (hierarchy subordinate) (ID "BMDC" "DR"))
	(Contact (hierarchy subordinate) (ID "FU" "DR")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Constructs for the FU_DR module.  The demo does not implement decisions that
;;the FU_DR makes, hence the lack of authority facts.

(deftemplate FU_DR::Contact
	(slot hierarchy)
	(multislot ID))
  
(deffacts FU_DR::Contact
	(Contact (hierarchy superior) (ID "CCC" "DO"))
	(Contact (hierarchy superior) (ID "BMDC" "DR")))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;The following instances are created to use for test purposes.
;;Currently only one SC can be logged into the Battle planner at a time
;;so these SC's will be used to show how BMDC_DR can communicate with other
;;commanders.

(definstances Players
	(CCC_DO of PLAYER (node_type "CCC") (node_number 1) (position_type "DO") (position_number 1) (active 0))
	(FU_DR of PLAYER (node_type "FU") (node_number 1) (position_type "DR") (position_number 1) (active 0))) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;This rule sets the environment within FuzzyCLIPS.  The alpha-value is a 
;;number that is used as a minimum Confidence Factor CF for all the rules.  In
;;FuzzyCLIPS the CF of a rule is computed from the membership values of the
;;fuzzy facts being matched in the conditional elements.  The CF gives a sense
;;of how fully the conditional elements are satisfied.  In the demo, the fuzzy
;;facts contributing to setting the threat severity level had low CF, so in 
;;order for the threat severity level rules to fire the alpha-level needed to
;;be set low.  The tron command is a user defined function that sends trace 
;;information to both the file trace.txt and to the SC GUI HISTORY window. 

(defrule MAIN::Set_Env
	(phase_control (phase initialize))
	(not (env set))
=>
	(set-alpha-value  0.4)
	(tron)
	(assert (env set)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;During the Initial phase the MAIN module extracts information from
;;initialize messages.  Keeps instances of Mission Objectives, Battle Plans,
;;and GBI farms active throughout the simulation.  There are no assumptions on
;;the number of instances kept current for this, however in a different phase
;;the Mission Objectives are assumed to have name slots of "Current" and 
;;"Alternate".

(defrule MAIN::Get_OP
      (object (is-a MSG_HEADER))
      (phase_control (phase initialize))	
=>
     (assert (opcode (send [msg_header] get-opcode)) (time (send [msg_header] get-gvt))))	

(defrule MAIN::OP_INIT_SC 
	(phase_control (phase initialize))
	(opcode 0)      
=>
	(assert (node (send [SimCmdr] get-node_type )))
	(assert (position (send [SimCmdr] get-position_type)))
	(assert (active (send [SimCmdr] get-active)))	
	(assert (opcode 501)))	

(defrule MAIN::OP_DB_STATUS  
       	(phase_control (phase initialize))
        (opcode 100)
	(object (is-a STATE) (name [msg_body]))
=>
	(bind ?*defcon* (send [msg_body] get-DEFCON))
	(bind ?*rp* (send [msg_body] get-RP))
	(bind ?*dea* (send [msg_body] get-DEA))
	(bind ?*roe* (send [msg_body] get-ROE))
	(bind ?*msn_obj* (send [msg_body] get-Msn_Obj_Name))
	(bind ?*bp* (send [msg_body] get-BP_Name))
	(assert (opcode 502)))

(defrule MAIN::OP_INIT_MO
	(phase_control (phase initialize))
        (opcode 301) 
	
=>
	(duplicate-instance [msn_obj])
	(send [msn_obj] delete)
	(assert (opcode 502)))


(defrule MAIN::OP_INIT_BP
	(phase_control (phase initialize))
        (opcode 302)  
=>
	(duplicate-instance [bp])
	(send [bp] delete)
	(assert (opcode 502)))

(defrule MAIN::OP_INIT_WA
	(phase_control (phase initialize))
	(opcode 303) 
	(object (is-a GBI_FARM) (id ?gbi) (weapons ?x)) 
	(object (is-a MSN_OBJ) (msn_obj_name ?name) (withhold ?y))
=>
	(assert (Weapons (id ?gbi) (msn_obj ?name) (wa (- ?x ?y))))
	(assert (opcode 502)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;During the Monitor phase the MAIN module records track and engagement 
;;information, updates weapons information, and prompts appropriate next steps.
;;

(defrule MAIN::OP_DEFCON 
	(phase_control (phase monitor))
	?x <- (opcode 101)     
	(object (name [msg_header]) (source_id [BattlePlanner]))
=>	
	(retract ?x)
	(bind ?*defcon* (send [msg_body] get-prompt))
	(assert (opcode 502)))

(defrule MAIN::OP_RP 
	(phase_control (phase monitor))
	?x <- (opcode 102)     
	(object (name [msg_header]) (source_id [BattlePlanner]))
=>	
	(retract ?x)
	(bind ?*rp* (send [msg_body] get-prompt))
	(assert (opcode 502)))

(defrule MAIN::OP_DEA
	(phase_control (phase monitor))
	?x <- (opcode 103)	     
	(object (name [msg_header]) (source_id [BattlePlanner]))
=>
	(retract ?x)
	(bind ?*dea* (send [msg_body] get-prompt))
	(assert (opcode 502)))

(defrule MAIN::OP_ROE
	(phase_control (phase monitor))
	?x <- (opcode 104)     
	(object (name [msg_header]) (source_id [BattlePlanner]))
=>
	(retract ?x)
	(bind ?*roe* (send [msg_body] get-prompt))
	(assert (opcode 502)))

(defrule MAIN::OP_MSN_OBJ
	(phase_control (phase monitor))
	?x <- (opcode 105)
	(object (name [msg_header]) (source_id [BattlePlanner]))
=>
	(retract ?x)
	(bind ?*msn_obj* (send [msg_body] get-prompt))
	(assert (opcode 502)))

(defrule MAIN::OP_BP
	(phase_control (phase monitor))
	?x <- (opcode 106)     
	(object (name [msg_header]) (source_id [BattlePlanner]))
=>
	(retract ?x)
	(bind ?*bp* (send [msg_body] get-prompt))
	(assert (opcode 502)))

(defrule MAIN::OP_WA
	(phase_control (phase monitor))
	(object (is-a GBI_FARM) (name [msg_body]) (id ?gbi) (weapons ?wa))
	(object (is-a ENGAGEMENT) (gbi_task_id ?gbi))
	(object (is-a MSN_OBJ) (msn_obj_name ?name) (withhold ?hold))
        (opcode 107) 
=>
	(assert (Weapons (id ?gbi) (msn_obj ?name) (wa (- ?wa ?hold))))
	(assert  (opcode 502)))


(defrule MAIN::OP_MISSILE_TRACK_NEW
	(phase_control (phase monitor))
        ?x <- (opcode 130) 
	(object (is-a MISSILE_TRACK) (name [msg_body]) (id ?y))
	(not (track_id ?y))

=>
	(retract ?x)
	(assert (track_id ?y))
	(duplicate-instance [msg_body])
	(assert (opcode 501)))

(defrule MAIN::OP_MISSILE_TRACK_UPDATE
	(phase_control (phase monitor))
	?x <- (opcode 130)
	(object (is-a MISSILE_TRACK) (name [msg_body]) (id ?track_id))
	(object (is-a MISSILE_TRACK) (name ?y&~[msg_body]) (id ?track_id))
	(track_id ?track_id)
=>
	(retract ?x)
	(duplicate-instance msg_body to ?y)
	(assert (opcode 501)))

(defrule MAIN::Missile_Threat_Initial
	(phase_control (phase monitor))
	(object (is-a MISSILE_TRACK) (name ~[msg_body]) (id ?id) (exp_targets ?missiles&:(> ?missiles 0)))
	(not (Missile_Threat (trk_id ?id) (threat ?)))
=>
	(assert (Missile_Threat (trk_id ?id) (threat high))
		(initial_missile ?id)))

(defrule MAIN::Missile_Threat
	(phase_control (phase monitor))
	(object (is-a MISSILE_TRACK) (name ~[msg_body]) (id ?id) (exp_targets ?missiles&:(> ?missiles 0)))
	?x <- (Missile_Threat (trk_id ?id) (threat ?))
	(not (initial_missile ?track_id))
=>
	(retract ?x)
	(assert (Missile_Threat (trk_id ?id) (threat high))))

(defrule MAIN::Remove_Missile_Threat
	(phase_control (phase monitor))
	(object (is-a MISSILE_TRACK) (id ?id) (exp_targets ?missiles&:(= ?missiles 0)))
	?x <- (Missile_Threat (trk_id ?id) (threat ?)) 
	?y <- (initial_missile ?id)
=>
	(retract ?x ?y))

(defrule MAIN::Low_Missile_Threat
	(phase_control (phase monitor))
	(not (object (is-a MISSILE_TRACK)))
=>
	(assert (Total_Missile_Threat (threat low))))

(defrule MAIN::Total_Missile_Threat
	(phase_control (phase monitor))
	(exists (Missile_Threat (trk_id ?) (threat high)))
=>
	(assert (Total_Missile_Threat (threat high))))

(defrule MAIN::OP_NEW_ENGMT
	(phase_control (phase monitor))
	?z <- (opcode 108)
	(object (is-a ENGAGEMENT) (name [msg_body]) (gbi_task_id ?y) (tgt_trk_id ?track_id))
	(not (engagement_id ?track_id ?y))
	
=>
	(retract ?z)
	(assert (engagement_id ?track_id  ?y))
	(duplicate-instance [msg_body])
	(assert (opcode 501)))

(defrule MAIN::OP_UPDATE_ENGMT
	(phase_control (phase monitor))
	?x <- (opcode 108)
	(object (is-a ENGAGEMENT) (name [msg_body]) (gbi_task_id ?gbi_id) (tgt_trk_id ?track_id))
	(object (is-a ENGAGEMENT) (name ?y&~[msg_body]) (gbi_task_id ?gbi_id) (tgt_trk_id ?track_id))
	(engagement_id ?track_id ?gbi_id)
=>
	(retract ?x)
	(duplicate-instance msg_body to ?y)
	(assert (opcode 501)))

(defrule MAIN::Low_Potential_Threat
	(phase_control (phase monitor))
	(not (object (is-a POTENTIAL_EVENT)))
=>
	(assert (Total_Potential_Threat (threat low))))

(defrule MAIN::OP_POTENTIAL_EVENT
	(phase_control (phase monitor))
	(opcode 140)
	(time ?gvt)
	(object (is-a POTENTIAL_EVENT) (name [msg_body]) (event_country ?country) (event_status ?event) 
			(event_time ?e_time) (persistence ?per))
	(test (<= ?gvt (+ ?e_time (* ?per 3600))))
	(country ?country ?c_value)
	(event ?event ?e_value)
	(not (Potential_Threat (country ?country) (event_time ?e_time) (threat ?))) 
	?y <- (Total_Potential_Threat (threat ?))
=>
	(duplicate-instance [msg_body])
	(modify ?y (threat (fuzzy-union (create-fuzzy-value Potential_Threat_Indicator (pi 3 (/ (+ ?c_value ?e_value) 2.0)))	(get-fuzzy-slot ?y threat))))
	(assert (Potential_Threat (country ?country) (event_time ?e_time) (threat (pi 3 (/ (+ ?c_value ?e_value) 2.0))))))

(defrule MAIN::Remove_Pot_Event
	(phase_control (phase monitor))
	(time ?gvt)
	(object (is-a POTENTIAL_EVENT) (event_country ?country) (event_time ?e_time) (persistence ?per))
	(test (> ?gvt (+ ?e_time (* ?per 3600))))
	?x <- (Potential_Threat (country ?country) (event_time ?e_time) (threat ?))
=>
	(retract ?x))

(defrule MAIN::OP_INTEL_INFO
	(phase_control (phase monitor))
	(opcode 150)
	?x <- (object (is-a INTEL_MSG) (info $?info&:(> (length$ ?info) 0)))
=>
	(send (nth$ 1 $?info) put-degraded (send (nth$ 1 $?info) get-value))
	(duplicate-instance (nth$ 1 $?info) (time (send ?x time-since)))
	(slot-delete$ (instance-name ?x) info 1 1)
	(send (nth$ 1 $?info) delete))

(defrule MAIN::Degrade_Intel
	(phase_control (phase monitor))
	?x <- (object (is-a INTEL_INFO) (source_type ?source) (source_num ?num) (value ?value) (persistence ?persistence) (time ?time))
	(time ?gvt)
=>
	(send ?x degrade-value ?gvt)
	(assert (degraded ?source ?num ?time)))

(defrule MAIN::Remove_old_Intel
	(phase_control (phase monitor))
	?x <- (object (is-a INTEL_INFO) (degraded ?value&:(< ?value 1.0)))
=>
	(send ?x delete))

(defrule MAIN::Fuzzify_Intel
	(phase_control (phase monitor))
	(time ?gvt)
	(forall (object (is-a INTEL_INFO) (source_type ?source) (source_num ?num) (time ?time)) (degraded ?source ?num ?time))
	(object (is-a INTEL_INFO) (source_type ?source) (source_num ?num) (time ?time) (degraded ?value&:(>= ?value 1.0)))
	(not (Intel_Threat (source ?source) (number ?num) (time ?time) (threat ?)))
=>
	(assert (Intel_Threat (source ?source) (number ?num) (time ?time) (threat (pi 1 ?value))))
	(assert (modified ?source ?num ?time ?gvt)))

(defrule MAIN::Fuzzify_Intel_Update
	(phase_control (phase monitor))
	(time ?gvt)
	(forall (object (is-a INTEL_INFO) (source_type ?source) (source_num ?num) (time ?time)) (degraded ?source ?num ?time))
	(object (is-a INTEL_INFO) (source_type ?source) (source_num ?num) (time ?time) (degraded ?value&:(>= ?value 1.0)))
	?x <- (Intel_Threat (source ?source) (number ?num) (time ?time) (threat ?))
	(not (modified ?source ?num ?time ?gvt))
=>
	(modify ?x  (threat (create-fuzzy-value Intel_Threat_Indicator (pi 1 ?value))))
	(assert (modified ?source ?num ?time ?gvt)))

(defrule MAIN::Done_Fuzzify
	(phase_control (phase monitor))
	(time ?gvt)
	(forall (object (is-a INTEL_INFO) (source_type ?source) (source_num ?num) (time ?time)) 
		(modified ?source ?num ?time ?gvt))
;	?x <- (Total_Intel_Threat (threat ?))
=>
;	(retract ?x)
	(assert (intel fuzzified)))

(defrule MAIN::Initial_Intel
	(phase_control (phase monitor))
	(not (Total_Intel_Threat (threat ?)))
=>
	(assert (Total_Intel_Threat (threat (pi 1 5)))))

(defrule MAIN::Done_Degrade
	(phase_control (phase monitor))
	?y <- (degraded ?source ?num ?time)
	(intel fuzzified)
	?x <- (Total_Intel_Threat (threat ?))
	?z <- (Intel_Threat (threat ?))
	
=>
	(modify ?x (threat (fuzzy-union (get-fuzzy-slot ?x threat) (get-fuzzy-slot ?z threat))))
	(retract ?y))

(defrule MAIN::Final_Fuzzify
	(phase_control (phase monitor))
	?y <- (intel fuzzified)
	(not (degraded ? ? ?))
=>
	(retract ?y))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

;;The Assess phase is the first that focus is on the SC's module
;;Changes are suggested in response to information within messages

(defrule MAIN::OP_ENGMT_ENGAGED
	(phase_control (phase assess))	
	(object (is-a ENGAGEMENT) (name ?y&~[msg_body]) 
			(eng_status "Pre Plan"|"Planned"|"Launched"|"Post IFSR"|"Await KA"|"Engaged"|"In Flight"))
  
=>
	(assert (opcode 501)))

;(defrule MAIN::OP_ENGT_NEED_ENGAGED
;	(phase_control (phase assess))
;	(object (is-a ENGAGEMENT) (name ?y&~[msg_body]) (eng_status "Missed"))
;=>
;	(assert (assess MSN_OBJ)))

(defrule MAIN::OP_ENGT_REMOVE
	(phase_control (phase assess))
	(object (is-a ENGAGEMENT) (name ?y&~[msg_body])  (eng_status "Killed"))
=>
	(send ?y delete))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

;;The CCC (CINC Command Center) is the only node where ROE can be changed.
;;Other decisions can be made at the CCC, but the only SC that is linked to 
;;the Battle Planner is the BMDC_DR.  So the ROE rule is the only one that is 
;;demonstrated here for CCC.  
 
(defrule CCC_DO::Assess_ROE
	(declare (auto-focus TRUE))
	(phase_control (phase assess))
	(Authority (change ROE))
	(object (is-a MISSILE_TRACK) (name ?y&~[msg_body])  (imp_lat ?lat) (imp_long ?long))
=>
	(bind ?*ccc_roe* (roe_assess ?lat ?long))
	(assert (ROE (roe met)) (change ROE)))

(defrule CCC_DO::Propose_ROE
	(declare (auto-focus TRUE))
	(phase_control (phase replan))
	(Authority (change ROE))
	(change ?c&ROE)
	(object (is-a PLAYER) (name ?x) (node_type ?node) (position_type ?position))
	(Contact (hierarchy ?y) (ID ?node ?position))
	(test (<> (str-compare ?*ccc_roe* ?*roe*) 0))
=>
	(make-instance  of INT_HEADER (destination_id ?x) (source_id [CCC_DO]) (opcode 104) (ack ?c))
	(assert (Change (propose ?c))))

	

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

(defrule MAIN::TSL_5
	(phase_control (phase assess))
	(Total_Missile_Threat (threat low))
	(Total_Intel_Threat (threat low))
	(Total_Potential_Threat (threat low))
=>
	(assert (Threat_Severity_Level Threat_Level_5)))

(defrule MAIN::TSL_4
	(phase_control (phase assess))
	(Total_Missile_Threat (threat low))
	(or (and (Total_Intel_Threat (threat moderate))
	   (Potential_Threat (country ?) (event_time ?) (threat low)))
	  (and (Total_Intel_Threat (threat low))
	   (Total_Potential_Threat (threat moderate))))
=>
	(assert (Threat_Severity_Level Threat_Level_4)))

(defrule MAIN::TSL_3
	(phase_control (phase assess))
	(Total_Missile_Threat (threat low))
	(Total_Intel_Threat (threat moderate))
	(Total_Potential_Threat (threat moderate))
=>
	(assert (Threat_Severity_Level Threat_Level_3)))

(defrule MAIN::TSL_2
	(phase_control (phase assess))
	(Total_Missile_Threat (threat low))
	(Total_Intel_Threat (threat high))
	(Total_Potential_Threat (threat high))
=>
	(assert (Threat_Severity_Level Threat_Level_2)))

(defrule MAIN::TSL_1
	(phase_control (phase assess))
	(Total_Missile_Threat (threat high))
=>
	(assert (Threat_Severity_Level Threat_Level_1)))

(defrule BMDC_DR::Assess_RP
	(declare (auto-focus TRUE))
	(phase_control (phase assess))
	?x <- (Threat_Severity_Level ?)
	(test (> (moment-defuzzify (get-fuzzy-slot ?x)) 0.15))
	(test (<> ?*bmdc_rp* 1))
=>
	(assert (change RP))
	(bind ?*bmdc_rp* 1))

(defrule BMDC_DR::Assess_MSN_OBJ
	(declare (auto-focus TRUE))
	(phase_control (phase assess))
	?eng <- (object (is-a ENGAGEMENT) (name ?name&~[msg_body]) (eng_status ~"Pre Plan") (gbi_task_id ?gbi) 
		(tgt_trk_id ?trk)(cur_eng_pk ?pk))
	?x <-(Weapons (id ?gbi) (msn_obj ?y&:(= (str-compare ?y ?*msn_obj*) 0)) (wa ?wa))
	?z <-(Weapons (id ?gbi) (msn_obj ?other&:(<> (str-compare ?other ?*msn_obj*) 0)) (wa ?wa_other))
	(test (> (replan (total_slot_value MISSILE_TRACK exp_targets)
			 ?pk  ?wa  1 1) 0.4))
	(test (> (replan (total_slot_value MISSILE_TRACK exp_targets)
			 ?pk  ?wa  1 1) 
		 (replan (total_slot_value MISSILE_TRACK exp_targets)
			 ?pk ?wa_other 1 1) ))
=>
	(send ?eng print)
	(retract ?x ?z)
	(assert (change MSN_OBJ))
	(bind ?*bmdc_msn_obj* ?other))

(defrule BMDC_DR::Assess_DEA
	(declare (auto-focus TRUE))
	(phase_control (phase assess))
	?x <- (Threat_Severity_Level ?)
	(test (> (moment-defuzzify (get-fuzzy-slot ?x)) 0.35))
	(test (<> (str-compare ?*bmdc_dea* "Free") 0))
=>
	(assert (change DEA))
	(bind ?*bmdc_dea* "Free"))
	
(defrule BMDC_DR::Assess_DEFCON_1
	(declare (auto-focus TRUE))
	(phase_control (phase assess))
	?x <- (ROE (roe met))
	(test (<> ?*defcon* 1))
	(test (<> ?*bmdc_defcon* 1))
	?y <- (Threat_Severity_Level ?)
	(test (and (<= 0.75 (moment-defuzzify (get-fuzzy-slot ?y))) (<= (moment-defuzzify (get-fuzzy-slot ?y)) 1.0)))
=>
	(retract  ?x)
	(assert  (change DEFCON))
	(bind ?*bmdc_defcon* 1))

(defrule BMDC_DR::Assess_DEFCON_2
	(declare (auto-focus TRUE))
	(phase_control (phase assess))
	?x <- (ROE (roe met))
	(test (<> ?*defcon* 2))
	(test (<> ?*bmdc_defcon* 2))
	?y <- (Threat_Severity_Level ?)
	(test (and (<= 0.55 (moment-defuzzify (get-fuzzy-slot ?y))) (< (moment-defuzzify (get-fuzzy-slot ?y)) 0.75)))
=>
	(retract  ?x)
	(assert  (change DEFCON))
	(bind ?*bmdc_defcon* 2))

(defrule BMDC_DR::Assess_DEFCON_3
	(declare (auto-focus TRUE))
	(phase_control (phase assess))
	?x <- (ROE (roe met))
	(test (<> ?*defcon* 3))
	(test (<> ?*bmdc_defcon* 3))
	?y <- (Threat_Severity_Level ?)
	(test (and (<= 0.35 (moment-defuzzify (get-fuzzy-slot ?y))) (< (moment-defuzzify (get-fuzzy-slot ?y)) 0.55)))
=>
	(retract  ?x)
	(assert  (change DEFCON))
	(bind ?*bmdc_defcon* 3))

(defrule BMDC_DR::Assess_DEFCON_4
	(declare (auto-focus TRUE))
	(phase_control (phase assess))
	(test (<> ?*defcon* 4))
	(test (<> ?*bmdc_defcon* 4))
	?x <- (Threat_Severity_Level ?)
	(test (and (<= 0.15 (moment-defuzzify (get-fuzzy-slot ?x))) (< (moment-defuzzify (get-fuzzy-slot ?x)) 0.35)))
=>
	(assert  (change DEFCON))
	(bind ?*bmdc_defcon* 4))

(defrule BMDC_DR::Assess_DEFCON_5
	(declare (auto-focus TRUE))
	(phase_control (phase assess))
	(test (<> ?*defcon* 5))
	(test (<> ?*bmdc_defcon* 5))
	?x <- (Threat_Severity_Level ?)
	(test (and (<= 0 (moment-defuzzify (get-fuzzy-slot ?x))) (< (moment-defuzzify (get-fuzzy-slot ?x)) .15)))
=>
	(assert  (change DEFCON))
	(bind ?*bmdc_defcon* 5))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

(defrule BMDC_DR::Propose_DEFCON
	(declare (auto-focus TRUE))
	(phase_control (phase replan))
	(Authority (change DEFCON))
	(change ?c&DEFCON)
	(object (is-a PLAYER) (name ?x) (node_type ?node) (position_type ?position))
	(Contact (hierarchy ?y) (ID ?node ?position))
	(test (<> ?*bmdc_defcon* ?*defcon*))
=>
	(assert (Change (propose ?c)))
	(make-instance  of INT_HEADER (destination_id ?x) (source_id [SimCmdr]) (opcode 101) (ack ?c)))

(defrule BMDC_DR::Propose_DEA
	(declare (auto-focus TRUE))
	(phase_control (phase replan))
	(Authority (change DEA))
	(change ?c&DEA)
	(object (is-a PLAYER) (name ?x) (node_type ?node) (position_type ?position))
	(Contact (hierarchy ?y) (ID ?node ?position))
	(test (<> (str-compare ?*bmdc_dea* ?*dea*) 0))
=>
	(assert (Change (propose ?c)))
	(make-instance  of INT_HEADER (destination_id ?x) (source_id [SimCmdr]) (opcode 103) (ack ?c)))

(defrule BMDC_DR::Propose_RP
	(declare (auto-focus TRUE))
	(phase_control (phase replan))
	(Authority (change RP))
	(change ?c&RP)
	(object (is-a PLAYER) (name ?x) (node_type ?node) (position_type ?position))
	(Contact (hierarchy ?y) (ID ?node ?position))
	(test (<> ?*bmdc_rp* ?*rp*))
=>
	(assert (Change (propose ?c)))
	(make-instance of INT_HEADER (destination_id ?x) (source_id [SimCmdr]) (opcode 102) (ack ?c)))

(defrule BMDC_DR::Propose_MSN_OBJ
	(declare (auto-focus TRUE))
	(phase_control (phase replan))
	(Authority (change MSN_OBJ))
	(change ?c&MSN_OBJ)
	(object (is-a PLAYER) (name ?x) (node_type ?node) (position_type ?position))
	(Contact (hierarchy ?y) (ID ?node ?position))
	(test (<> (str-compare ?*bmdc_msn_obj* ?*msn_obj*) 0))
=>
	(assert (Change (propose ?c)))
	(make-instance of INT_HEADER (destination_id ?x) (source_id [SimCmdr]) (opcode 105) (ack ?c)))
	
(defrule MAIN::Ack_SC
     	(phase_control (phase replan))
	(Change (propose ?c))
=>	
     	(do-for-all-instances ((?Ack_header INT_HEADER)) (= (str-compare ?Ack_header:ack ?c) 0)
		(message-modify-instance ?Ack_header  (destination_id ?Ack_header:source_id) 
					(source_id ?Ack_header:destination_id) (opcode 502))))

(defrule BMDC_DR::BMDC_Ack
	(declare (auto-focus TRUE))
	(phase_control (phase replan))
	?Ack_header <- (object (source_id [SimCmdr]) (opcode 502) (ack ?c))
=>
	(make-instance of INT_MESSAGE (message_header (instance-name ?Ack_header)) 
			(msg_body (make-instance of STRING_PROMPT (prompt ?c)))))

(defrule CCC_DO::CCC_Ack
	(declare (auto-focus TRUE))
	(phase_control (phase replan))
	?Ack_header <- (object (source_id [CCC_DO]) (opcode 502) (ack ?c))
=>
	(make-instance of INT_MESSAGE (message_header (instance-name ?Ack_header))
			(msg_body (make-instance  of STRING_PROMPT (prompt ?c)))))


(defrule FU_DR::FU_Ack
	(declare (auto-focus TRUE))
	(phase_control (phase replan))
	?Ack_header <- (object (source_id [FU_DR]) (opcode 502) (ack ?c))
=>
	(make-instance of INT_MESSAGE (message_header (instance-name ?Ack_header))
			(msg_body (make-instance of STRING_PROMPT (prompt ?c)))))

(defrule MAIN::Accept_Ack
	(phase_control (phase replan))
	?y <- (Change (propose ?c))
=>
	(retract ?y)
	(do-for-all-instances ((?Ack_header INT_HEADER)) (and (= ?Ack_header:opcode 502) 
								(= (str-compare ?Ack_header:ack ?c) 0))
	(assert (Acked (send ?Ack_header:source_id get-node_type) (send ?Ack_header:source_id get-position_type) ?c)))

	(focus CCC_DO BMDC_DR))

(defrule BMDC_DR::Acks_Accept
	(phase_control (phase replan))
	(declare (auto-focus TRUE))
	(forall (Contact (hierarchy ?y) (ID ?node ?position))
		(Acked  ?node ?position ?c))
	(forall (change ?c) (Acked ?node ?position ?c))
;	(exists (Acked ? ? ?))
=>
	(assert (done acks))
	(bind ?*defcon* ?*bmdc_defcon*)
	(bind ?*rp* ?*bmdc_rp*)
	(bind ?*dea* ?*bmdc_dea*)
	(bind ?*msn_obj* ?*bmdc_msn_obj*))

(defrule BMDC_DR::Clear_Acks
	(phase_control (phase replan))
	(done acks)
	?ack <- (Acked ? ? ?c)
	?change <- (change ?c)
=>
	(retract ?ack ?change)) 
	
(defrule BMDC_DR::Final_Ack_Clear
	(phase_control (phase replan))
	 ?done <- (done acks)
	 (not (Acked ? ? ?))
=>
	(retract ?done))
 	
(defrule CCC_DO::Acks_Accept
	(phase_control (phase replan))
	(forall (Contact (hierarchy ?y) (ID ?node ?position))
		(Acked  ?node ?position ?c))
	(forall (change ?c) (Acked ?node ?position ?c))
=>
	(assert (done acks))
	(bind ?*roe* ?*ccc_roe*))

(defrule CCC_DO::Clear_Acks
	(phase_control (phase replan))
	(done acks)
	?ack <- (Acked ? ? ?c)
	?change <- (change ?c)
=>
	(retract ?ack ?change)) 
	
(defrule CCC_DO::Final_Ack_Clear
	(phase_control (phase replan))
	 ?done <- (done acks)
	 (not (Acked ? ? ?))
;	?change <- (change ?c)
=>
	(retract ?done))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 	

(defrule MAIN::OP_Exit_SC
	(phase_control (phase execute))
	?x <- (opcode 10)
	?y <- (opcode ?z&501|502|503)
	
=>
	(unmake-instance *)
	(troff)
	(retract ?x ?y))
	
(defrule MAIN::Clear_Int_messages
	(phase_control (phase execute))
	?msg <- (object (is-a INT_MESSAGE) (message_header ?Ack_header) (msg_body ?msg_body))
=>
	(send ?msg_body delete)
	(send ?Ack_header delete)
	(send (instance-name ?msg) delete))

(defrule MAIN::Remove_Opcode
	(phase_control (phase execute))
	?x <- (opcode ?y&~501&~502&~503)
=>
	(retract ?x)
	(assert (removed opcode)))	


(defrule MAIN::Rtn_Decision
	(phase_control (phase execute))
	(removed opcode)
	(not (opcode ?))
=>
	(assert (opcode 501)))

(defrule MAIN::Final_Opcode
	(phase_control (phase execute))
	(opcode ?y&501|502|503)
	?x <- (removed opcode)
=>
	(retract ?x))

(defrule MAIN::Return_Header
	(phase_control (phase execute))
	(opcode ?x&501|502|503)
=>
	(modify-instance [msg_header] 
		(destination_id [BattlePlanner]) (source_id [SimCmdr]) (opcode ?x)))

(defrule MAIN::OP_RTRN_ERR
	(phase_control (phase execute))
	?x <- (opcode 503)
	(object (name [msg_header]) (destination_id [BattlePlanner]))
=>
	(retract ?x)
	(send [msg_header] put-opcode 503)
	(send [msg_body] delete)
	(make-instance [msg_body] of STRING_PROMPT (prompt "An error has occurred."))
	(assert (return message)))

(defrule MAIN::OP_RTRN_ACK
	(phase_control (phase execute))
	?x <- (opcode 502)
	(object (name [msg_header]) (destination_id [BattlePlanner]))
=>
	(retract ?x)
	(modify-instance [msg_header] (opcode 502))
	(send [msg_body] delete)
	(make-instance [msg_body] of INTEGER_PROMPT (prompt 1))
	(assert (return message)))


(defrule MAIN::OP_RTRN_DCN
	(phase_control (phase execute))
	?x <- (opcode 501)
	(object (name [msg_header]) (destination_id [BattlePlanner]))
=>
	(retract ?x)
	(modify-instance [msg_header] (opcode 501))
	(send [msg_body] delete)
	(make-instance [msg_body] of STATE 
		(DEFCON ?*defcon*)
		(RP ?*rp*)
		(DEA ?*dea*)
		(ROE ?*roe*)
		(Msn_Obj_Name ?*msn_obj*)
		(BP_Name ?*bp*))
	(send [msg_body] print)
	(assert (return message)))


(defrule MAIN::Return_Message
	?phase <- (phase_control (phase execute))
	?x <- (return message)
	?y <- (time ?)
=>
	(modify-instance [msg] (message_header [msg_header]) (msg_body [msg_body]))
	(retract ?phase ?y ?x))













