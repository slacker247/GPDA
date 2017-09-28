
/**
 * Title:        Browser<p>
 * Description:  This allows you to browse a library of knowledge bases.<p>
 * Copyright:    Copyright (c) Warren Shen<p>
 * Company:      Stanford Medical Informatics<p>
 * @author Warren Shen
 * @version 1.0
 */
package browser.query;

import java.util.*;
import edu.stanford.smi.protege.model.*;
import edu.stanford.smi.protege.util.*;
import browser.model.*;

public class ContainsValueTest extends AbstractSlotValueTest {


  public ContainsValueTest(String className, String instance, String slotName, Vector testValues) {
    super(className, instance, slotName, testValues);
  }


  public boolean compareValues(Slot slot, Vector instances, Vector testValues) {
    int i, j;
    Vector projectValues;

    for(i = 0; i < instances.size(); i++) {
      projectValues = new Vector(((Instance)instances.elementAt(i)).getOwnSlotValues(slot));
      for(j = 0; j < projectValues.size(); j++) {
        if(testValues.contains(projectValues.elementAt(j))) {
          return true;
        }
      }
    }
    return false;
  }

}