
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
import browser.model.*;

public interface FrameTest {
  public Collection doTest(LibraryModel library);
}