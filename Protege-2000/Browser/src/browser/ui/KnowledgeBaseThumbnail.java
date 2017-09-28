
/**
 * Title:        Browser<p>
 * Description:  This allows you to browse a library of knowledge bases.<p>
 * Copyright:    Copyright (c) Warren Shen<p>
 * Company:      Stanford Medical Informatics<p>
 * @author Warren Shen
 * @version 1.0
 */
package browser.ui;

import javax.swing.*;
import java.awt.*;
import browser.model.*;
import browser.event.*;
import edu.stanford.smi.protege.util.*;
import edu.stanford.smi.protege.model.*;
import java.util.*;
import java.io.*;
import att.grappa.*;
import dfki.protege.ontoviz_tab.*;

public class KnowledgeBaseThumbnail extends AbstractLibraryView {
  private LabeledComponent _labeledComponent;
  private JPanel emptyPanel = new JPanel();

  public void initialize() {
    setLayout(new BorderLayout());

    setLayout(new BorderLayout());
    String text = "Thumbnail";

    _labeledComponent = new LabeledComponent(text, new GraphPanel());
    add(_labeledComponent, BorderLayout.CENTER);
  }

  public AbstractValidatableComponent getConfigurationPanel(){return null;}

  public String getName() {
    return "Thumbnail";
  }

  public void selectionChanged(LibraryEvent event) {
    if(_library.getSelectedKBSummaries().size() > 0) {
      _labeledComponent.setCenterComponent(((LibraryItem)CollectionUtilities.getFirstItem(_library.getSelectedKBSummaries())).getThumbnail());
    } else {
      _labeledComponent.setCenterComponent(emptyPanel);
    }
    repaint();
  }


  public void libraryModified(LibraryEvent event) {
    selectionChanged(event);
  }
  public void librarySaved(LibraryEvent event) {
  }
  public void libraryClosed(LibraryEvent event) {
    selectionChanged(event);
  }
  public void libraryChanged(LibraryEvent event) {
    selectionChanged(event);
  }

  public String getConfigurationString() {
    return "";
  }
  public void loadConfigurationString()
  {}
}