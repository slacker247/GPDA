package dfki.protege.oil_tab;

import java.util.*;

public class FactSomeConcept extends FactConcept {

  FactRole role;
  FactConcept concept;

  public FactSomeConcept(FactRole role, FactConcept concept) {
    this.role = role;
    this.concept = concept;
  }

  public FactRole getRole() {
    return role;
  }

  public FactConcept getConcept() {
    return concept;
  }

  public String toXML() {
    return "<SOME>" + role.toXML() + concept.toXML() + "</SOME>";
  }

  public String toLaTeX() {
    return "\\exists " + role.toLaTeX() + "." + concept.toLaTeX();
  }

}

