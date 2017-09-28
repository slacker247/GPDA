package dfki.protege.oil_tab;

import java.util.*;

public class FactAllConcept extends FactConcept {

  FactRole role;
  FactConcept concept;

  public FactAllConcept(FactRole role, FactConcept concept) {
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
    return "<ALL>" + role.toXML() + concept.toXML() + "</ALL>";
  }

  public String toLaTeX() {
    return "\\forall " + role.toLaTeX() + "." + concept.toLaTeX();
  }

}

