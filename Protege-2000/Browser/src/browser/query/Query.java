
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
import edu.stanford.smi.protege.model.*;

public class Query {

  private LibraryModel library;
  private Vector KBTests = new Vector();
  private Vector frameTests = new Vector();

  public Query(LibraryModel library) {
    this.library = library;
  }

  public void addKBTest(KBTest test) {
    KBTests.add(test);
  }

  public void removeKBTest(KBTest test) {
    KBTests.remove(test);
  }

  public Vector getResults() {
    Vector projectResults = new Vector();
    Vector frameResults = new Vector();
    Project p;
    int i, j ;
    boolean passes;

    for(i = 0; i < library.size(); i++) {
      //p = library.getIthProject(i);
      p = new Project("hi", new ArrayList());
      passes = true;
      for(j = 0; j < KBTests.size(); j++) {
        if(!((KBTest)KBTests.elementAt(j)).doTest(p)) {
          passes = false;
          break;
        }
      }
      if(passes) {
        projectResults.add(p);
      }
    }
    return projectResults;
    /*iter = KBTests.iterator();
    while(iter.hasNext()) {

      projectResults = ((QueryTest)iter.next()).doTest(projectResults);
    }
    return projectResults;*/
    /*
    iter = frameTests.iterator();
    while(iter.hasNext()) {
      frameResults = (((QueryTest)iter.next()).doTest(projectResults));
    }
    return frameResults;
    */
  }



}