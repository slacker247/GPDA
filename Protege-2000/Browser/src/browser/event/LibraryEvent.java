
/**
 * Title:        Browser<p>
 * Description:  This allows you to browse a library of knowledge bases.<p>
 * Copyright:    Copyright (c) Warren Shen<p>
 * Company:      Stanford Medical Informatics<p>
 * @author Warren Shen
 * @version 1.0
 */
package browser.event;

import edu.stanford.smi.protege.util.AbstractEvent;
import browser.model.*;
import java.util.*;

public class LibraryEvent extends AbstractEvent {
  private final static int BASE = 100;
  public final static int SELECTION_CHANGED = BASE + 1;
  public final static int LIBRARY_SAVED = BASE + 2;
  public final static int LIBRARY_CLOSED = BASE + 3;
  public final static int LIBRARY_CHANGED = BASE + 4;
  public final static int LIBRARY_MODIFIED = BASE + 5;

  public LibraryEvent(LibraryModel src, int type, Collection kbsummaries) {
    super(src, type, kbsummaries);
  }

  public Collection getKBSummaries() {
    return (Collection)getArgument();
  }

}