package dfki.protege.oil_tab;

import java.util.*;

public class FactAtmostConcept extends FactConcept {

  int n;
  FactRole role;
  FactConcept concept;

  public FactAtmostConcept(int n, FactRole role, FactConcept concept) {
    this.n = n;
    this.role = role;
    this.concept = concept;
  }

  public int getN() {
    return n;
  }

  public FactRole getRole() {
    return role;
  }

  public FactConcept getConcept() {
    return concept;
  }

  public String toXML() {
    return "<ATMOST NUM=\"" + n + "\">" 
      + role.toXML() + concept.toXML() + "</ATMOST>";
  }

  public String toLaTeX() {
    return "\\leqslant\\! " + n + "\\ " + role.toLaTeX()
      + "." + concept.toLaTeX();
   }


}

