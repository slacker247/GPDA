
/**
 * Title:        Browser<p>
 * Description:  This allows you to browse a library of knowledge bases.<p>
 * Copyright:    Copyright (c) Warren Shen<p>
 * Company:      Stanford Medical Informatics<p>
 * @author Warren Shen
 * @version 1.0
 */
package browser.model;

import browser.ui.BrowserManager;
import javax.swing.*;
import java.util.*;
import java.io.*;
import java.net.*;
import edu.stanford.smi.protege.util.*;

public class HTMLSummary extends LibraryItem {
  private Collection _links;

  public HTMLSummary() {
    this(null, true);
  }

  public HTMLSummary(String filePath, boolean isHidden) {
    _isHidden = isHidden;
    _isActive = true;
    _isModified = false;
    _filePath = filePath;
    init();
  }

  private void init() {
    getLinks();
  }


  public Collection createNewLibraryItems(Collection identifiers, boolean isHidden, boolean isActive) {
    Iterator iter = identifiers.iterator();
    String identifier;
    LibraryItem item;
    ArrayList results = new ArrayList();
    while(iter.hasNext()) {
      identifier = (String)iter.next();
      String newFilePath = identifier;
      if(FileUtilities.getDirectory(identifier) == null) {
        newFilePath = FileUtilities.getDirectory(this.getFilePath());
        newFilePath = newFilePath.concat("\\" + identifier);
      }
      item = new HTMLSummary(newFilePath, isHidden);
      item.setActive(isActive);
      results.add(item);
    }
    return results;
  }

  private void getLinks() {
    Reader reader = null;
    _links = new ArrayList();
    try {
      reader = new FileReader(new File(_filePath));
    } catch (Exception e) {
      //System.out.println("Unable to get html links: " + _filePath);
      return;
    }
    //Reader reader = FileUtilities.getReader(_filePath);
    if(reader == null) {
      return;
    }
    BufferedReader r = new BufferedReader(reader);
    String tag = "";
    int c;
    int state;
    state = 0;
    try {
      while((c = r.read()) != -1) {
        if(c == '<') {
          state = 1;
          tag = "";
        } else {
          if(c == '>') {
            state = 0;
            parseTag(tag);
          }
        }
        if(c != '<' && c != '>') {
          if(state == 1) {
            //System.out.println("appending: " + String.valueOf((char)c) + " to " + tag);
            //System.out.print(String.valueOf((char)c));
            tag = tag.concat(String.valueOf((char)c));
          }
        }
      }
    } catch (IOException e) {
      e.printStackTrace();
    }
  }

  private void parseTag(String tag) {
    StringTokenizer t;
    String token;
    if(tag.toLowerCase().startsWith("a href")) {
      t = new StringTokenizer(tag,"\"", false);
      token = t.nextToken();
      token = t.nextToken();
      if(FileUtilities.getDirectory(token) == null) {
        token = FileUtilities.getDirectory(this.getFilePath()) + "\\\\" + token;
      }
      _links.add(token);
    }
  }

  public Collection getUserLibraryItems() {
    Iterator iter;
    ArrayList items = new ArrayList();
    iter = BrowserManager.launchFileChooser("HTML", ".html").iterator();
    while(iter.hasNext()) {
      items.add(iter.next());
    }
    return items;
  }

  public String getTypeName() {
    return "HTML";
  }

  public String getIdentifier() {
    return _filePath;
  }

  public Collection getAttributeValues(Object attributeName) {
    return new ArrayList();
  }
  public ArrayList getSummaryAttributeNames() {
    ArrayList list = new ArrayList();
    //list.add("title");
    return list;
  }
  public ArrayList getAttributeNames(){
    ArrayList list = new ArrayList();
    list.add("html attribute");
    return list;
  }
  public String getName() {
    return FileUtilities.getName(_filePath);
  }
  public String getFilePath() {
    return _filePath;
  }
  public Collection getDependencies() {
    return _links;
  }
  public JComponent getThumbnail() {
    JEditorPane pane = new JEditorPane();
    try {
      URL url = new URL("file:\\" + _filePath);
      pane = new JEditorPane(url);
    } catch (IOException e) {
      System.out.println("Could not read: " + _filePath);
      return new JPanel();
    }
    return ComponentFactory.createScrollPane(pane);
//    return pane;
  }

  public Collection getQueryTypeNames() {
    ArrayList list = new ArrayList();
    list.add("Links");
    return list;
  }
  public Collection query(String query, String queryTypeName, boolean showIncluded) {
    ArrayList results = new ArrayList();
    if(queryTypeName.equals(LibraryItem.FIND_ANY) || queryTypeName.equals("Links")) {
      Iterator iter = _links.iterator();
      String link;
      while(iter.hasNext()) {
        link = (String)iter.next();
        if(link.equals(query)) {
          results.add(link);
        }
      }
    }
    return results;
  }
}