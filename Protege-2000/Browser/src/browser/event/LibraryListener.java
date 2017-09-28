
/**
 * Title:        Browser<p>
 * Description:  This allows you to browse a library of knowledge bases.<p>
 * Copyright:    Copyright (c) Warren Shen<p>
 * Company:      Stanford Medical Informatics<p>
 * @author Warren Shen
 * @version 1.0
 */
package browser.event;

import java.util.*;

public interface LibraryListener extends EventListener {
  public void selectionChanged(LibraryEvent event);
  public void libraryChanged(LibraryEvent event);
  public void librarySaved(LibraryEvent event);
  public void libraryClosed(LibraryEvent event);
  public void libraryModified(LibraryEvent event);


}