package dfki.protege.oil_tab;

public class FactInvRole extends FactRole {

  FactRole role;

  public FactInvRole(FactRole role) {
    this.role = role;
  }

  public FactInvRole(String roleName) {
    this.role = new FactPrimitiveRole(roleName);
  }

  public FactRole getRole() {
    return role;
  }

  public String toXML() {
    return "<INVROLE>" + role.toXML() + "</INVROLE>";
  }

  public String toLaTeX() {
    return role.toLaTeX() + "^{-}";
  }

}

