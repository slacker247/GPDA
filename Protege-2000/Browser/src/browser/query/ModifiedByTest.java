
/**
 * Title:        Browser<p>
 * Description:  This allows you to browse a library of knowledge bases.<p>
 * Copyright:    Copyright (c) Warren Shen<p>
 * Company:      Stanford Medical Informatics<p>
 * @author Warren Shen
 * @version 1.0
 */
package browser.query;

import browser.model.*;
import java.util.*;

public class ModifiedByTest extends ContainsValueTest {

  public ModifiedByTest(Vector modifiers) {
    super(SummaryConstants.SUMMARY_CLASS_NAME, SummaryConstants.SUMMARY_INSTANCE_NAME,
          SummaryConstants.MODIFIER_SLOT_NAME, modifiers);
  }
}