package dfki.protege.oil_tab;

import java.util.*;

public class FactOrConcept extends FactConcept {

  Collection concepts;

  public FactOrConcept(Collection concepts) {
    this.concepts = concepts;
  }

  public Collection getConcepts() {
    return concepts;
  }

  public String toXML() {
    StringBuffer buffer = new StringBuffer();
    buffer.append("<OR>");
    for (Iterator cIterator = concepts.iterator(); cIterator.hasNext();)
      buffer.append(((FactConcept)cIterator.next()).toXML());
    buffer.append("</OR>");
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
	buffer.append("\\sqcup\\, ");
      }
    }
    if (!toplevel) buffer.append(")");
    return buffer.toString();
  }
}

