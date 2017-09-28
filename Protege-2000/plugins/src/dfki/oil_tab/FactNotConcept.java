package dfki.protege.oil_tab;

import java.util.*;

public class FactNotConcept extends FactConcept {

  FactConcept concept;

  public FactNotConcept(FactConcept concept) {
    this.concept = concept;
  }

  public FactConcept getConcept() {
    return concept;
  }

  public String toXML() {
    return "<NOT>" + concept.toXML() + "</NOT>";
  }

  public String toLaTeX() {
    return "\\neg " + concept.toLaTeX();
  }

}

