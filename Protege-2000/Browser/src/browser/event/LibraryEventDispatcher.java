
/**
 * Title:        Browser<p>
 * Description:  This allows you to browse a library of knowledge bases.<p>
 * Copyright:    Copyright (c) Warren Shen<p>
 * Company:      Stanford Medical Informatics<p>
 * @author Warren Shen
 * @version 1.0
 */
package browser.event;

import edu.stanford.smi.protege.util.EventDispatcher;
import java.util.Collection;
import browser.model.*;
import java.util.*;

public class LibraryEventDispatcher implements EventDispatcher {

  public LibraryEventDispatcher() {
  }

  public void postEvent(Collection listeners, Object source, int type, Object arg1, Object arg2, Object arg3) {
     LibraryEvent event = new LibraryEvent((LibraryModel) source, type, (Collection) arg1);
        Iterator i = listeners.iterator();
        while (i.hasNext()) {
            LibraryListener listener = (LibraryListener) i.next();
            switch (type) {
                case LibraryEvent.SELECTION_CHANGED:
                  listener.selectionChanged(event);
                  break;
                case LibraryEvent.LIBRARY_CLOSED:
                  listener.libraryClosed(event);
                  break;
                case LibraryEvent.LIBRARY_SAVED:
                  listener.librarySaved(event);
                  break;
                case LibraryEvent.LIBRARY_CHANGED:
                  listener.libraryChanged(event);
                  break;
                case LibraryEvent.LIBRARY_MODIFIED:
                  listener.libraryModified(event);
                  break;
            }
        }
  }
}