
/**
 * Title:        Browser<p>
 * Description:  This allows you to browse a library of knowledge bases.<p>
 * Copyright:    Copyright (c) Warren Shen<p>
 * Company:      Stanford Medical Informatics<p>
 * @author Warren Shen
 * @version 1.0
 */
package browser.model;

public class SummaryConstants {

  public static final String SUMMARY_CLASS_NAME = ":SUMMARY";
  public static final String SUMMARY_INSTANCE_NAME = ":SUMMARY_INSTANCE";
  public static final String CREATOR_SLOT_NAME = ":CREATOR";
  public static final String CREATION_TIMESTAMP_SLOT_NAME = ":CREATION-TIMESTAMP";
  public static final String SUBJECT_SLOT_NAME = ":SUBJECT";
  public static final String KEYWORDS_SLOT_NAME = ":KEYWORDS";
  public static final String TITLE_SLOT_NAME = ":PROJECT_NAME";
  public static final String DOCUMENTATION_SLOT_NAME = ":DOCUMENTATION";
  public static final String ORGANIZATION_SLOT_NAME = ":ORGANIZATION";
  public static final String MODIFICATION_TIMESTAMP_SLOT_NAME = ":MODIFICATION-TIMESTAMP";
  public static final String MODIFIER_SLOT_NAME = ":MODIFIER";
  public static final String URL_SLOT_NAME = ":URL";
  public static final String THUMBNAIL_FILE = ":THUMBNAIL_FILE";

  public static final String[] DEFAULT_SUMMARY_ATTRIBUTE_NAMES =
    { SummaryConstants.CREATOR_SLOT_NAME,
      SummaryConstants.CREATION_TIMESTAMP_SLOT_NAME, SummaryConstants.SUBJECT_SLOT_NAME };

  public static final String[] DEFAULT_ATTRIBUTE_NAMES = {
    SummaryConstants.TITLE_SLOT_NAME,
    SummaryConstants.SUBJECT_SLOT_NAME,
    SummaryConstants.CREATOR_SLOT_NAME,
    SummaryConstants.CREATION_TIMESTAMP_SLOT_NAME,
    SummaryConstants.ORGANIZATION_SLOT_NAME,
    SummaryConstants.URL_SLOT_NAME,
    SummaryConstants.MODIFIER_SLOT_NAME,
    SummaryConstants.MODIFICATION_TIMESTAMP_SLOT_NAME,
    SummaryConstants.KEYWORDS_SLOT_NAME,
    SummaryConstants.DOCUMENTATION_SLOT_NAME,
    SummaryConstants.THUMBNAIL_FILE
  };

  public SummaryConstants() {
  }
}