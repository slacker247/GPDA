package dfki.protege.ontoviz_tab;


interface OptionsTableConstants {

  final static int FRAME = 0;
  final static int SUB = 1;
  final static int SUP = 2;
  final static int SLX = 3;
  final static int ISX = 4;
  final static int SLT = 5;
  final static int SLE = 6;
  final static int INS = 7;
  final static int SYS = 8;

  final static int itsLastColumn = SYS;

  final static String[] columnNames = new String[] {"frame", 
    "sub", "sup", "slx", "isx", "slt", "sle", "ins", "sys"};

  final static String[] tips = new String[] {"frames", 
    "subclass closure",
    "superclass closure",
    "slot extension",
    "inverse slot extension",
    "slots",
    "slot edges",
    "instances",
    "system frames"};

  final static int frameColWidth = 80;
  final static int colWidth = 25;

}


