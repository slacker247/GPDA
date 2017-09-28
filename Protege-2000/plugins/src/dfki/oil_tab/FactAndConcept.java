package dfki.protege.oil_tab;

import java.util.*;

public class FactAndConcept extends FactConcept {

  Collection concepts;

  public FactAndConcept(Collection concepts) {
    this.concepts = concepts;
  }

  public Collection getConcepts() {
    return concepts;
  }

  public String toXML() {
    return toXML(false);
  }

  public String toXML(boolean split) {
    // if split is true, add a newline after each conjunct
    StringBuffer buffer = new StringBuffer();
    buffer.append("<AND>");
    for (Iterator cIterator = concepts.iterator(); cIterator.hasNext();) {
      buffer.append(((FactConcept)cIterator.next()).toXML());
      if (split && cIterator.hasNext())
	buffer.append("\n    ");
    }
    buffer.append("</AND>");
    return buffer.toString();
  }

  public String toLaTeX() {
    return toLaTeX(false, false);
  }

  public String toLaTeX(boolean toplevel, boolean allowSplit) {
    boolean split = false;
    if (allowSplit && concepts.size() > 2) split = true;
    StringBuffer buffer = new StringBuffer();
    if (!toplevel) buffer.append("(");
    for (Iterator cIterator = concepts.iterator(); cIterator.hasNext();) {
      buffer.append(((FactConcept)cIterator.next()).toLaTeX());
      if (cIterator.hasNext()) {
	if (split) 
	  buffer.append(" \\\\\n    & & ");
	else
	  buffer.append(" \\,");
	buffer.append("\\sqcap\\, ");
      }
    }
    if (!toplevel) buffer.append(")");
    return buffer.toString();
  }

}

