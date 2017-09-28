README for the Ontoviz Tab


CHANGES

2001-12-18  directly overridden template slots are shown
2000-12-21  preferred arrow direction can be configured for each slot
2000-12-17  inverse slot extension added
2000-12-11  maximum numbers of values/symbols per slot and slots per node can be set
2000-12-04  slots can be individually configured; dot-1.7beta.exe included
2000-12-01  maximum depth for slot extension can be set
2000-11-29  improved icons (+C, +I, Op)
2000-11-17  slide mode (inverse colors) added
2000-11-16  single instances can be added to the "Config" table
2000-11-15  instances can be  displayed; column headings changed (ts -> slt etc.)
2000-11-14  initial version posted on some Protege mailing lists


INSTALLATION

1. a) copy ontoviz.jar into Protege's plugins directory
      IMPORTANT: if you have a previous version of OntoViz installed, 
                 you have to remove ontoviz-1.X.jar!)

   b) copy grappa1_2.jar into the plugins directory


2. install the Graphviz tool 
   - on Windows: execute gvXXX.exe (included in this distribution)
   - on Unix: download the appropriate version of Graphviz for
     your platform from:
     http://www.research.att.com/sw/tools/graphviz

3. adapt your protege.properties file:

   dot.command=<path-to-dot-exe>  (as installed in 2.)
     IMPORTANT: '\', ' ', and ':' must be preceded by a '\',
                e.g.: dot.command=C\:\\Program\ Files\\graphviz\\bin\\dot
   dot.font=<font-name>  (optional; usually Arial)
   dot.fontsize=<size-specification>  (optional; usually -2 or -3)
   dot.fontpath=<path-to-ttf-directory>  (needed on some Unixes;
     on Solaris: /usr/openwin/lib/X11/fonts/TrueType)

   See the example protege.properties file for the exact syntax
   (in most cases, it is ok to simply copy this file into your Protege
   directory).



USAGE

Configure your project with the Ontoviz tab (Project / Configure ...).

To create a graph, simply click on the "C" button. 

Clicking at a node displays the corresponding class in the classes tree (and
vice versa); middle/right button allows zooming.

To fine tune your graph (e.g., for showing only a part of your
ontology), the following options are available:


a. options per class/instance ("per row in the Config table")

You can add classes (or some of their instances) selected in the classes 
tree to the "Config" table by pressing the "add classes" (+C) or "add instances" (+I)
button and then check any of the following check boxes:

- sub   subclass closure
- sup   superclass closure
- slx   slot extension
- isx   inverse slot extension
- slt   slots
- sle   slot edges
- ins	instances
- sys   system frames

The (simplified) semantics is as follows:

For each row in the table:

  closure := {frame[row]}

  1. sub/sup: compute the subclass and superclass closures if frame is a class
     (independently; otherwise we would always end up with all classes):
     closure := closure U subClosure U supClosure

  2. slx/isx: for all classes/instances in closure, find the classes/instances that 
     are reachable by a slot of type INSTANCE or CLASS (the maximum depth can 
     be configured on the global options window; default is 1 for classes and 
     3 for instances):
     closure := closure U slotExtension

  3. ins: add all instances of classes in closure to closure:
     closure = closure U instances(closure)
     
  4. slt/sle/sys: (globally) mark all frames in closure accordingly

[If sys is not true, in all steps above ignore system classes and slots]

It is allowed to add a class more than once to the table (with
different options set, of course).

Some examples:

- inspect one class in the middle of the ontology (e.g., Employee in
  the newspaper example):
  Employee sup slt sys
  Employee sub
  -> shows all superclasses of Employee WITH template slots
     and all subclasses WITHOUT template slots

- show a class and its "template slot neighborhood" (e.g., Newspaper):
  Newspaper sup slx isx tse sys


b. options for slots

Slots can be configured individually by pressing the colored "S" button.

A slot can be configured as "default", "hidden", or "configured" (at the moment,
configured slot means user selected color and preferred arrow direction). 
Furthermore, all "default" slots can be hidden ("hide slots as default" check box). 
To configure a slot, click at the "default" cell in the table.


c. global options

The global options window is opened by clicking on the "Op" button.

- save as gif: also save the graph as a gif image in the directory
  where the project resides (or in the main Protege directory, if
  the project has not yet beed saved)

- show io :STANDARD-CLASS edges: only if this box is checked, instance of ("io")
  :STANDARD-CLASS links are shown (results in really huge graphs!)
  
- show system own slots: only if this button is checked, system own slots are
  shown (e.g., :NAME) which are not very useful except for debugging system
  stuff

- slot edges dashed: use dashed line for slot edges (useful for printing)

- show instances only: don't show the classes (useful if you
  want to inspect complex relationships between many instances)

- max. depth for slot extensions

- colors for nodes and edges



Michael -- sintek@smi.stanford.edu

