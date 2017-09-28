PrologTab Plug-in 0.1.0
=======================

PrologTab is an integration of GNU Prolog for Java (http://gnuprologjava.sourceforge.net)
with Protege-2000 (http://protege.stanford.edu) version 1.7 and up.

Included Files:
---------------
build.xml			- Ant make file
gnuprolog-0.1.0-with-mods.jar	- Modified GNU Prolog jar
gnuprolog-mods.zip		- Source changes for GNU Prolog
prologtab-0.1.0.jar		- PrologTab jar filed
prologtab-pprj-0.1.0.zip	- Example PrologTab project
prologtab-src-0.1.0.zip		- PrologTab source files
readme.txt			- This file

Installation:
------------
* Copy the pre-built jar files (prologtab-0.1.0.jar and
gnuprolog-0.1.0-with-mods.jar) to your Protege plugins
directory. 
* Un-zip PrologTabProject.zip into your favorite
directory for holding Protege project files.

Running Simple Test:
--------------------
* Start Protege.
* Open PrologTabProject.pprj
* Select "PrologTab"
* Select "Modules" sub-tab
* Select "TestCases" module instance
* Select "Consult" from the pop-up menu

* Return to "Console" sub-tab.
  The text area should say: "?-Consulting module: TestCases...Done"
  
* At the console prompt type:
	?-testCase1. (then press enter).
	
* There should now be a new class defined (TestClass) with a single String
slot (testSlot). There should also be an instance of this class created.

* From the console you can execute a query such as:
  ?-instanceof(Instance,frame('TestClass')),testSlot(Instance,Value).

* To cleanup evaluate:
  ?-testCase2.

Knowledge base representation:
------------------------------
* PrologTab represents Protege frame-slot-value triples as fact clauses
within the Prolog database. I decided to use the slot name as the predicate
name to make the expression of the slot relations easier within Prolog.
* Frames (Instances,Slots and Classes) are refered to by wrapping the
:NAME atom within a frame term. E.g. the class :THING is referenced as
frame(':THING').
* Changes made to the Knowledge Base are kept in sync with the Prolog database.
* Collection slot values are represented as Lists in Prolog.
* Slots :NAME and :DIRECT-TYPE are renamed to :name and :direct-type 
to avoid collisions with template slots. PrologTab does not distinguish
template slots.

Protege-API and Java interface:
-------------------------------
* Built-in predicates invoke/4 and construct/3 allow invoking methods on and
constructing Java objects.
* The built-in get_tab/1 provides the initial reference to the PrologTab environment.
* The Protege-API module provides predicates for interacting with the Protege KB.

Contributors:
-------------
Many thanks to the authors of:

	JessTab - Henrik Eriksson
	FloraTab - Michael Sintek
	GNU Prolog for Java - Constantine Plotnikov

whose code samples made much of this possible.

Your feedback and input is greatly welcomed.

Troy Caldwell
troycald@pacbell.net
May 8, 2002
