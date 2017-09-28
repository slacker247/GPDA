package dfki.protege.oil_tab;

import java.util.*;

public class FactAtleastConcept extends FactConcept {

  int n;
  FactRole role;
  FactConcept concept;

  public FactAtleastConcept(int n, FactRole role, FactConcept concept) {
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
    return "<ATLEAST NUM=\"" + n + "\">" 
      + role.toXML() + concept.toXML() + "</ATLEAST>";
  }

  public String toLaTeX() {
    return "\\geqslant\\! " + n + "\\ " + role.toLaTeX() 
      + "." + concept.toLaTeX();
  }

}

