package dfki.protege.oil_tab;

import java.util.*;
import java.io.*;
import java.awt.*;
import java.awt.event.*;
import java.applet.*;
import javax.swing.*;
import javax.swing.event.*;
import javax.swing.text.*;
import javax.swing.border.*;
import javax.swing.table.*;
import java.net.*;
import lispwrapper.*;
import org.omg.CORBA.*;
import org.omg.CosNaming.*;
import img.fact.cexpr.*;
import img.fact.*;
import img.fact.xml.*;
import img.util.*;


public class Fact {

  ClientHandler ch;
  Classifier cl;
  ClientConnector cc;

  PrintWriter latex = null;

  String defaultNSHost;
  String defaultNSPort;
  String defaultServerName;

  JTextArea report;

  final static FactTOP TOP = new FactTOP();
  final static FactBOTTOM BOTTOM = new FactBOTTOM();

  // for LaTeX export
  final static String LatexImpliesC = "\\sqsubseteq_{c}";
  final static String LatexImpliesR = "\\sqsubseteq_{r}";
  final static String LatexEqualC = "\\doteq_{c}";
  final static String LatexEqualR = "\\doteq_{r}";
  final static String LatexTrans = "\\mathbf{R}_{+}";
  final static String LatexFunct = "\\mathbf{R}_\\mbox{\\sl funct}";


// connection handling -----------------

/** Try and connect to the server */
public void connect() { 
  if (!connected()) {
    report("Connecting");
    if (cc==null) {
      cc = new ClientConnector(defaultNSHost,
			       defaultNSPort, 
			       defaultServerName,
			       "client");
    }
    ch = cc.connect(null); // parentFrame ...
    if (!connected())
      report("!! Failed !!");
  }
}


/** Release the client handler. If it's in a transaction, the
    transaction will be aborted */
  void release() {
    if (connected()) {
      if (ch.in_transaction()) {
	report ("Aborting transaction...");
	ch.abort_transaction();
      }
      report("Disconnecting");
      try {
	ch.release();
      } catch (Exception e) {
	System.err.println(e.toString());
      };
    }
    ch = null;
  }


/** Is the client connected to the server? */
  boolean connected() {
    return ch != null;
  }


/** Are we in a transaction? */
  boolean transaction() {
    return (connected() && ch.in_transaction());
  }


// set some defaults for the connection

public void setDefaultNSHost(String h) {
  defaultNSHost = h;
}
  
public void setDefaultNSPort(String h) {
  defaultNSPort = h;
}
  
public void setDefaultServerName(String h) {
  defaultServerName = h;
}


// transaction handling --------------------

/** Begin a transaction */
void beginTransaction() {
  if (connected()) {
    boolean res = ch.begin_transaction();
    if (res) {
      report("Transaction Started");
    } else {
      report("!! Transaction Start Failed !!");
    }
  }
}


/** End the current transaction */
void endTransaction() {
  if (connected()) {
    boolean res = ch.end_transaction();
    if (res) {
      report("Transaction Committed");
    } else {
      report("!! Transaction End Failed !!");
    }
  }
}


/** Reset the current transaction, throwing any facts away, but
    keeping the transaction open */
void resetTransaction() {
  if (connected()) {
    boolean res = ch.reset_transaction();
    if (res) {
      report("Transaction Reset");
    } else {
      report("!! Transaction Reset Failed !!");
    }
  }
}


/** Abort the transaction -- throw away all the facts and end the
    transaction */
void abortTransaction() {
  if (connected()) {
    boolean res = ch.abort_transaction();
    if (res) {
      report("Transaction Aborted");
    } else {
      report("!! Transaction Abort Failed !!");
    }
  }
}
  

// tells ----------------

/** Clear the knowledge base */
void clear() {
  if (connected()) {
    try {
      report("clear()");
      ch.clear();
    } catch (InternalException ex) {
      report (Short.toString(ex.code) + " " + ex.information);
    } catch (TransactionRequiredException ex) {
      report (Short.toString(ex.code) + " " + ex.information);
    } catch (OpUnimplementedException ex) {
      report (Short.toString(ex.code) + " " + ex.information);
    } catch (ExprErrorException ex) {
      report (Short.toString(ex.code) + " " + ex.information);
    }
  }
}
    

// concepts

void defconcept(String conceptName) {
  if (connected()) {
    try {
      report("defconcept(" + conceptName + ")");
      ch.defconcept(conceptName);
    } catch (Exception e) {
      report(e.toString());
    }
  }
}

void impliesC(String conceptName, FactConcept conceptDef) {
  impliesC(new FactPrimitiveConcept(conceptName), conceptDef);
}

void impliesC(FactConcept concept1, FactConcept concept2) {
  latexImpliesC(concept1, concept2);
  if (connected()) {
    String c1 = concept(concept1);
    String c2 = concept(concept2);
    try {
      report("impliesC(" + c1 + ", " + c2 + ")");
      ch.impliesC(c1, c2);
    } catch (Exception e) {
      report(e.toString());
    }
  }
}

void equalC(String conceptName, FactConcept conceptDef) {
  equalC(new FactPrimitiveConcept(conceptName), conceptDef);
}

void equalC(FactConcept concept1, FactConcept concept2) {
  latexEqualC(concept1, concept2);
  if (connected()) {
    String c1 = concept(concept1);
    String c2 = concept(concept2);
    try {
      report("equalC(" + c1 + ", " + c2 + ")");
      ch.equalC(c1, c2);
    } catch (Exception e) {
      report(e.toString());
    }
  }
}

/*
String concept(String name) {
  return "<CONCEPT><PRIMITIVE NAME=\"" + name + "\"/></CONCEPT>";
}
*/

String concept(FactConcept concept) {
  if (concept instanceof FactAndConcept)
    return "<CONCEPT>" + ((FactAndConcept)concept).toXML(true) + "</CONCEPT>";
  else
    return "<CONCEPT>" + concept.toXML() + "</CONCEPT>";
}


// roles

void defrole(String roleName) {
  if (connected()) {
    try {
      report("defrole(" + roleName + ")");
      ch.defrole(roleName);
    } catch (Exception e) {
      report(e.toString());
    }
  }
}

void impliesR(String roleName1, String roleName2) {
  impliesR(new FactPrimitiveRole(roleName1),
	   new FactPrimitiveRole(roleName2));
}

void impliesR(FactRole role1, FactRole role2) {
  latexImpliesR(role1, role2);
  if (connected()) {
    String r1 = role(role1);
    String r2 = role(role2);
    try {
      report("impliesR(" + r1 + ", " + r2 + ")");
      ch.impliesR(r1, r2);
    } catch (Exception e) {
      report(e.toString());
    }
  }
}

void equalR(FactRole role1, FactRole role2) {
  latexEqualR(role1, role2);
  if (connected()) {
    String r1 = role(role1);
    String r2 = role(role2);
    try {
      report("equalR(" + r1 + ", " + r2 + ")");
      ch.equalR(r1, r2);
    } catch (Exception e) {
      report(e.toString());
    }
  }
}

void transitive(FactRole role) {
  latexTransitive(role);
  if (connected()) {
    try {
      String r = role(role);
      report("transitive(" + r + ")");
      ch.transitive(r);
    } catch (Exception e) {
      report(e.toString());
    }
  }
}

void functional(FactRole role) {
  latexFunctional(role);
  if (connected()) {
    try {
      String r= role(role);
      report("functional(" + r+ ")");
      ch.functional(r);
    } catch (Exception e) {
      report(e.toString());
    }
  }
}

/*
String role(String name) {
  return "<ROLE><PRIMROLE NAME=\"" + name + "\"/></ROLE>";
}
*/

String role(FactRole role) {
  return "<ROLE>" + role.toXML() + "</ROLE>";
}


 
/* needed in future version
public String convertToDescription(String xmlName) {
  String desc = "";
  try {
    desc = xmlConverter.convert(xmlName);
  } catch (XMLConversionException e) {
    System.err.println(xmlName);
    desc = e.getMessage();
  }
  return desc;
}
*/
 

// asks -----------------


/** Get all classifier tells. */
String getAllTells() {
  if (connected()) {
    try {
      report("allTells()");
      String tells = ch.allTells();
      report("  -> " + tells.substring(0, 50) + " ...");
      return tells;
    } catch (Exception e) {
      report(e.toString());
      return null;
    }
  }
  return null;
}


/** Get the direct supers of a concept. */
Collection getDirectSuperConcepts(FactConcept concept) {
    // returns a Collection of FactConcepts
    // at the moment, only TOP, BOTTOM, and primitive concepts are handled!!!
    if (connected()) {
      String[][] res;
	try {
	  String conceptXML = concept.toXML();
	  report("directSupersC(" + conceptXML + ")");
	  res = ch.directSupersC(conceptXML);
	  Vector superConcepts = new Vector();
	  int l = res.length;
	  for (int i = 0; i < l; i++) {
	    String[] row = res[i];
	    int m = row.length;
	    for (int j = 0; j < m; j++) {
	      String xmlConcept = row[j];
	      // this is one big hack ....
	      // we need an XML parser here that also handles
	      // concept expressions
	      if (xmlConcept.startsWith("<PRIMITIVE")) {
	        StringTokenizer tokenizer = 
		  new StringTokenizer(xmlConcept, "\"");
		tokenizer.nextToken();
		String conceptName = tokenizer.nextToken();
		superConcepts.add(new FactPrimitiveConcept(conceptName));
	      } else if (xmlConcept.equals("<TOP/>")) {
		superConcepts.add(TOP);
	      } else if (xmlConcept.equals("<BOTTOM/>")) {
		superConcepts.add(BOTTOM);
	      } else {
		System.out.println("getDirectSuperConcepts(): concept expression not handled: \"" + xmlConcept + "\"");
	      }
	    }
	  }
	  report("  -> " + superConcepts);
	  return superConcepts;
	} catch (Exception e) {
	  report(e.toString());
	  return null;
	}
      }
    return null;
}


boolean satisfiable(FactConcept concept) {
  if (connected()) {
    String conceptXML = concept(concept);
    try {
      report("satisfiable(" + conceptXML + ")");
      boolean answer = ch.satisfiable(conceptXML);
      report("  -> " + answer);
      return answer;
    } catch (Exception e) {
      report(e.toString());
      return false;
    }
  } else
    return false;
}


// stuff for LaTeX output

void latexOutput(String filename) {
  try {
    latex = new PrintWriter(new BufferedWriter(new FileWriter(filename)));
  } catch (Exception e) {
    System.out.println(filename + " could not be opened: " + e);
    latex = null;
  }
  latexHeader();
}

boolean endLatexOutput() {
  // returns true if export was successfull
  if (latex != null)
    try { 
      latexFooter(); 
      latex.close(); 
      return true;
    } catch (Exception e) {
      return false;
    }
  else
    return false;
}

void latexHeader() {
  if (latex != null) {
    latex.println("\\documentclass{article}");
    latex.println("\\usepackage{amssymb}");
    latex.println();
    latex.println("\\begin{document}");
    latex.println();
    latex.println("\\begin{eqnarray*}");
  }
}

void latexFooter() {
  if (latex != null) {
    latex.println("\\end{eqnarray*}");
    latex.println();
    latex.println("\\end{document}");
    latex.println();
  }
}

void latexImpliesC(FactConcept c1, FactConcept c2) {
  if (latex != null) {
    latexL(c1);
    latex.print(" & " + LatexImpliesC + " & ");
    latexR(c2);
    latex.println(" \\\\");
  }
}

void latexEqualC(FactConcept c1, FactConcept c2) {
  if (latex != null) {
    latexL(c1);
    latex.print(" & " + LatexEqualC + " & ");
    latexR(c2);
    latex.println(" \\\\");
  }
}

void latexImpliesR(FactRole role1, FactRole role2) {
  if (latex != null) {
    latexL(role1);
    latex.print(" & " + LatexImpliesR + " & ");
    latexR(role2);
    latex.println(" \\\\");
  }
}

void latexEqualR(FactRole role1, FactRole role2) {
  if (latex != null) {
    latexL(role1);
    latex.print(" & " + LatexEqualR + " & ");
    latexR(role2);
    latex.println(" \\\\");
  }
}

void latexTransitive(FactRole role) {
  if (latex != null) {
    latexL(role);
    latex.print(" & \\in & " + LatexTrans);
    latex.println(" \\\\");
  }
}

void latexFunctional(FactRole role) {
  if (latex != null) {
    latexL(role);
    latex.print(" & \\in & " + LatexFunct);
    latex.println(" \\\\");
  }
}

void latexL(FactExpression expr) { // no line breaks (at the moment) !!!
  if (expr instanceof FactAndConcept)
    latex.print(((FactAndConcept)expr).toLaTeX(true, false));
  else if (expr instanceof FactOrConcept)
    latex.print(((FactOrConcept)expr).toLaTeX(true, false));
  else
    latex.print(expr.toLaTeX());
}

void latexR(FactExpression expr) {
  if (expr instanceof FactAndConcept)
    latex.print(((FactAndConcept)expr).toLaTeX(true, true));
  else if (expr instanceof FactOrConcept)
    latex.print(((FactOrConcept)expr).toLaTeX(true, true));
  else
    latex.print(expr.toLaTeX());
}

/*
void latexMbox(String name) {
  latex.print(mbox(name));
}
*/

static String mbox(String name) {
  String normalizedName = name.replace('_', '-'); // ...
  return("\\mbox{" + normalizedName + "}");
}


// reporting -------------------------

public void setReportArea(JTextArea textarea) {
  report = textarea;
}

/** Report something */
  void report(String str) {
    report.append(str + "\n");
  }
  
/** Report something */
  void reportNoNL(String str) {
    report.append(str);
  }


}


