package jess.examples.pumps;
import jess.*;

/**
  An example of creating Beans in Java, then telling Jess about them.

  Don't run this from this directory! It expects you to run it from the
  JessXX distribution directory, like this:

    java jess.examples.pumps.MainInJava

  */

public class MainInJava
{
  public static void main(String[] argv) throws JessException
  {
    Rete rete = new Rete();
    
    // Read in the rules
    rete.executeCommand("(batch jess/examples/pumps/pumps-fromjava.clp)");
    rete.executeCommand("(reset)");
    
    // Create the Beans
    Tank t = new Tank("MAIN");
    Pump p = new Pump("MAIN", t);
    
    // Tell Jess about them
    Funcall f = new Funcall("definstance", rete);               
    f.add(new Value("pump", RU.ATOM));
    f.add(new Value(p));
    f.execute(rete.getGlobalContext()); 
    
    f = new Funcall("definstance", rete);               
    f.add(new Value("tank", RU.ATOM));
    f.add(new Value(t));
    f.execute(rete.getGlobalContext()); 
    
    while (t.isIntact())
      rete.executeCommand("(run)");
  }
}







