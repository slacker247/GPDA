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


public class HasKeywordTest extends ContainsValueTest {

  public HasKeywordTest(Vector keywords) {
    super(SummaryConstants.SUMMARY_CLASS_NAME,
          SummaryConstants.SUMMARY_INSTANCE_NAME,
          SummaryConstants.KEYWORDS_SLOT_NAME, keywords);
  }

/*  private Vector keywords;

  public HasKeywordTest(Vector keywords) {
    super(KBSummary.SUMMARY_CLASS_NAME, KBSummary.KEYWORDS_SLOT_NAME, keywords);
    this.keywords = keywords;
  }

  public boolean compareValues(Slot slot, Vector instances, Vector testValues) {
      int i, j;
      Vector projectKeywords;

      for(i = 0; i < instances.size(); i++) {
        projectKeywords = new Vector(((Instance)instances.elementAt(i)).getOwnSlotValues(slot));
        for(j = 0; j < projectKeywords.size(); j++) {
          if(isKeyword((String)projectKeywords.elementAt(j), testValues)) {
            return true;
          }
        }
      }

      return false;
  }

*//*
  public boolean doTest(Project project) {
    Vector projectResults = new Vector();
    int i;
    System.out.println("doing test!!!!!");
    return (projectHasKeyword(project, keywords));
  }

  private boolean projectHasKeyword(Project p, Vector keywords) {
    Vector projectKeywords;
    Instance instance;
    Slot slot;
    int i;

    slot = p.getKnowledgeBase().getSlot(KBSummary.KEYWORDS_SLOT_NAME);
    instance = p.getKnowledgeBase().getInstance(KBSummary.SUMMARY_INSTANCE_NAME);
    //instance = (Instance)(CollectionUtilities.getFirstItem(slot.getReachableSimpleInstances()));

    projectKeywords = new Vector(instance.getOwnSlotValues(slot));
    System.out.println("Here are the keywords: ");
    for(i = 0; i < projectKeywords.size(); i++) {
      System.out.println("--" + projectKeywords.elementAt(i));
    }
    for(i = 0; i < projectKeywords.size(); i++) {
      if(isKeyword((String)(projectKeywords.elementAt(i)), keywords)) {
        return true;
      }
    }
    return false;
  }
*//*
  private boolean isKeyword(String word, Vector keywords) {
    return keywords.contains(word);
  }
  */
}