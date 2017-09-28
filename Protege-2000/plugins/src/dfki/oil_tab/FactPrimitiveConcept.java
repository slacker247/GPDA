package dfki.protege.oil_tab;

public class FactPrimitiveConcept extends FactConcept {

  String name;

  public FactPrimitiveConcept(String name) {
    this.name = name;
  }

  public String getName() {
    return name;
  }

  public String toXML() {
    return "<PRIMITIVE NAME=\"" + name + "\"/>";
  }

  public String toLaTeX() {
    return Fact.mbox(name);
  }

}

