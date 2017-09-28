package dfki.protege.oil_tab;

public abstract class FactExpression {

  public abstract String toXML();

  public abstract String toLaTeX();

  public String toString() {
    return toXML();
  }

}

