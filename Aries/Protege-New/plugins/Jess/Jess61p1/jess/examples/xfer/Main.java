/**
 * A simple example of using 'store' and 'fetch'.
 */

package jess.examples.xfer;

import jess.*;

import java.awt.Point;

public class Main
{
  public static void main(String[] argv)
  {
    try
      {
        // *************************************************************
        // Create engine; load in some extra functions

        Rete rete = new Rete();

        // *************************************************************
        // Send initial parameters to Jess

        rete.store("A", new Value(1, RU.INTEGER));
        rete.store("B", new Value(3.14159, RU.FLOAT));
        rete.store("C", new Point(10,10));

        // *************************************************************
        // Define some rules. The first rule fetches the inputs and asserts
        // them as facts; the second rule matches those facts and stores
        // a result based on the inputs. Note that the rules could just
        // as easily have come from a file, but I wanted to make this
        // example self-contained.

        rete.executeCommand("(defrule startup" +
                            " =>" +
                            "  (assert (input-1 (fetch A)))" +
                            "  (assert (input-2 (fetch B)))" +
                            "  (assert (input-3 (fetch C))))");

        rete.executeCommand("(defrule match-input" +
                            "  (input-1 ?a&:(< ?a 2))" +
                            "  (input-2 ?b)" +
                            "  (input-3 ?c&:(external-addressp ?c))" +
                            " =>" +
                            "  (store RESULT (+ ?a ?b)))");

        // *************************************************************
        // Run the rules

        rete.reset();
        rete.run();

        // *************************************************************
        // Fetch the result (4.14159) and print it out

        System.out.println("The answer is: " + rete.fetch("RESULT"));


      }
    catch (JessException re)
      {
        re.printStackTrace();
      }

  }
}


