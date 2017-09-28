package dfki.protege.oil_tab;

public class FactPrimitiveRole extends FactRole {

  String name;

  public FactPrimitiveRole(String name) {
    this.name = name;
  }

  public String getName() {
    return name;
  }

  public String toXML() {
    return "<PRIMROLE NAME=\"" + name + "\"/>";
  }

  public String toLaTeX() {
    return Fact.mbox(name);
  }

}

