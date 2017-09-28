This version of The AssistRQ app completly works.

It's requirments are that it be installed in the $HOME dir along
with Gate_2.1-beta1.

9-23-02
Added to this version is the ability to change the destination of the Remote Host.

9-24-02
Added the move all button

9-25-02
Added the capability to drag and drop the values

9-27-02
Added a status dialog so when processing a text through GATE the
user can get feed back.
Also fixed a bug when running Gate on another machine the modules are unabled
to find their files.

10-1-02
Moved the save to file from the export dialog to it's own menu item.

10-4-02
Made the drag and drop within the trees work more effectivily.

10-7-02
Added the capability to use different rules or wordlists by just selecting
which one you want.

11-15-02
Made the hedge association more accurate.  The hedge capability is belief based on the hedge word and what the word relates to and taking into account the negative conotation.

11-19-02
The App runs off the mySQL database in two ways.  First it reads the availible domains and second it reads the field values.

11-20-02
Added the capability to edit the base configuration data and preserved it from
one session to another.

11-21-02
The app can save the evidence to the mySQL database.

1-6-03
Fixed a bug where if you open a file not located in AssistRQ/Sample Texts/ it would throw an error.  Also added the ability to run not connected to the Database but be up to date when connected.

1-28-03
Fixed the BackGround run to work with any connection defined in the Config.dat file.  Added the Source to the end of the Record.

2-11-03
Added a new interface to the Background run ( 1 after the AssitRQ.bin).  Also added the full functionality of the RQ form.  Partially implemented the new look into the old AssitRQ ( one without the 1 after it) only works if you process the text.  Will not work if you open a saved set.

2-12-03
Modified the submit string to the Sodi 1 by adding the mission to the third place in the list of data.

4-9-03
Modified the display to accomodate the Gate view.  Also unified the file open dialog box to jfilechooser.