
/**
 * Title:        Browser<p>
 * Description:  This allows you to browse a library of knowledge bases.<p>
 * Copyright:    Copyright (c) Warren Shen<p>
 * Company:      Stanford Medical Informatics<p>
 * @author Warren Shen
 * @version 1.0
 */
package browser.query;

import edu.stanford.smi.protege.model.*;
import java.util.*;

public abstract class AbstractSlotValueTest implements KBTest {
  private String clsName;
  private String instanceName;
  private String slotName;
  private Vector testValues;

  /**
   * One of clsName or instanceName may be null.  If instanceName is null, this will check all
   * the instances of the cls.  If instanceName is not null, this will only check that
   * particular instance.
   */
  public AbstractSlotValueTest(String clsName, String instanceName, String slotName, Vector testValues) {
    this.clsName = clsName;
    this.instanceName = instanceName;
    this.slotName = slotName;
    this.testValues = testValues;
  }

  public abstract boolean compareValues(Slot slot, Vector instances, Vector testValues);

  public boolean doTest(Project project) {
    Cls cls = project.getKnowledgeBase().getCls(clsName);
    Slot slot = project.getKnowledgeBase().getSlot(slotName);
    Instance instance = project.getKnowledgeBase().getInstance(instanceName);

    if(instance != null) {
      Vector v = new Vector();
      v.add(instance);
      return compareValues(slot, v, testValues);
    } else {
      return compareValues(slot, new Vector(cls.getInstances()), testValues);
    }
  }



}