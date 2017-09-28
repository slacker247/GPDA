/**
 * Title:        LibraryItem.java<p>
 * Description:  This allows you to browse a library of knowledge bases.<p>
 * Copyright:    Copyright (c) Warren Shen<p>
 * Company:      Stanford Medical Informatics<p>
 * @author Warren Shen
 * @version 1.0
 *
 * This is an abstract class whose subclasses handle different types of
 * library items.  For example, the subclass KBSummary handles working
 * with Protege projects.  This class defines the set of methods that
 * need to be implemented for the Browser to be able to manage the
 * library item.
 */
package browser.model;

import java.util.*;
import javax.swing.*;

public abstract class LibraryItem {
  protected String _filePath = "", _name = "", _identifier = "";
  protected boolean _isActive = true, _isIncluded = false;

  public static final String FIND_ANY = "Any";

  /**
   * Default constructor.  IMPORTANT: When writing a
   * subclass of LibraryItem, do not write a constructor
   * for it.  The subclass should not be instantiated
   * directly, but will be instantiated via the
   * createNewLibraryItems() and getUserLibraryItems()
   * methods.
   */
  public LibraryItem() {}

  /**
   * Will create new library items based on the
   * identifiers given.  An identifier is a string
   * which contains all the information needed to
   * construct a new library item.  So, for example
   * the identifier could be a filename if that is
   * all that is needed to create a particular item.
   *  The subclass is responsible for providing and
   * interpreting its own identifiers.
   */
  public abstract Collection createNewLibraryItems(Collection identifiers, boolean isIncluded, boolean isActive);

  /**
   * Will prompt the user for input and return a
   * Collection of the selected LibraryItems.
   * This most likely will invoke a file chooser
   * (supplied in BrowserManager.java) to let the user pick a file.
   */
  public abstract Collection getUserLibraryItems();

  /**
   * Returns the name of this type.  For example,
   * if the LibraryItem were HTML documents,
   * this could return "HTML".
   */
  public abstract String getTypeName();

  /**
   * Returns an identifier that contains all the
   * information that is needed to construct a
   * new item.  For example, if you were representing
   * a certain file, the identifier could simply
   * be its file name.  If you were representing
   * an instance in a protege project, the identifier
   * would need to contain information about the file
   * as well as the specific instance.
   */
  public abstract String getIdentifier();

  /**
   * Returns a JComponent that displays a graphical
   * view of the item.  For KBSummary which represents
   * protege projects, it returns a JComponent containing
   * a graph of the knowledge base.  Another example could
   * be displaying the HTML file if the items were web
   * pages.
   */
  public abstract JComponent getThumbnail();

  /**
   * Returns the names of various query types
   * that this item supports.  For example, KBSummary
   * returns {"Attributes", "Slots", and "Classes"}.
   * When the user performs a query, it can specifiy
   * which type of query to perform.  It is up to the
   * subclass to perform the query based on the type
   * selected.
   */
  public abstract Collection getQueryTypeNames();

  /**
   * Returns the result from a query performed.
   * The query is the actual text the user searched
   * for, and the queryTypeName is one of the query
   * types provided in getQueryTypeNames().  The
   * parameter showIncluded indicates if the results
   * should contain values that are not explicitly
   * in an item but are present because the item
   * includes another item.  For example, if a
   * project does not contain a particular class
   * but includes a project that does, the results
   * will display that class only if showIncluded
   * is true.
   */
  public abstract Collection query(String query, String queryTypeName, boolean showIncluded);

  /**
   * This returns a sorted collection of summary
   * attribute names.  Summary attributes are a
   * subset of the attributes, which will be used
   * in the LibraryTable in the Library Overview tab.
   */
  public ArrayList getSummaryAttributeNames() {
    return new ArrayList();
  }

  /**
   * Returns a sorted list of attributes.  Attributes
   * are aspects or meta-information about the item,
   * such as filename, author, date, or subject.
   */
  public ArrayList getAttributeNames() {
    return new ArrayList();
  }

  /**
   * Given one of the attribute names provided
   * from getAttributeNames(), this returns the
   * actual values associated with that name.
   * So, if the attribute name was "file name",
   * the result could be the name of the file.
   */
  public Collection getAttributeValues(Object attributeName) {
    return new ArrayList();
  }

  /**
   * Returns a collection of Strings that are
   * identifiers for other LibraryItems of the
   * same type.  For example, for KBSummary, it would
   * return the filenames of all the protege
   * projects that were included in the protege
   * project represented by the item.
   */
  public Collection getDirectDependenciesIds() {
    return new ArrayList();
  }

  public Collection getAllDependenciesIds() {
    return new ArrayList();
  }

  /* Given and identifier, return the name of the item */
  public String getNameFromIdentifier(String id) {
    return id;
  }

  /**
   * An item is active if it is being browsed in
   * the LibraryTable.  It is used for nothing
   * more than deciding to display the item in the table
   * or not.
   */
  public boolean isActive() {
    return _isActive;
  }

  /**
   * An item is included if it is in the library but
   * the only it is there because another item includes it.
   * For example, if an item was in the library but its
   * dependencies were not, the dependencies would be hidden.
   */
  public boolean isIncluded(){
    return _isIncluded;
  }

  /**
   * Setter for _isActive.
   */
  public void setActive(boolean isActive) {
    _isActive = isActive;
  }

  /**
   * Setter for _isIncluded.
   */
  public void setIncluded(boolean isIncluded) {
    _isIncluded = isIncluded;
  }

  /**
   * Returns the name of the item.  For example, if the
   * item represented a protege project, the name would
   * be the name of the .pprj file.
   */
  public String getName() {
    return _name;
  }

  /**
   * Returns the file path of whatever it is this item
   * represents.
   */
  public String getFilePath() {
    return _filePath;
  }
}
