
/**
 * Title:        Browser<p>
 * Description:  This allows you to browse a library of knowledge bases.<p>
 * Copyright:    Copyright (c) Warren Shen<p>
 * Company:      Stanford Medical Informatics<p>
 * @author Warren Shen
 * @version 1.0
 */
package browser.ui;

import javax.swing.JComponent;
import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.event.*;
import java.util.*;
import browser.model.*;
import edu.stanford.smi.protege.util.*;
import edu.stanford.smi.protege.resource.*;

public class BrowserView extends JComponent {
  private LibraryModel _library;
  private JTabbedPane _tabbedPane;
  private KnowledgeBaseTable _kbsummary;
  private KnowledgeBaseThumbnail _kbthumbnail;
  private DependenciesLattice _lattice;
  private QueryPanel _queryPanel;
  private ArrayList _libraryViews;
  private ArrayList _libraryTables;

  public final static int WIDTH = 900;
  public final static int HEIGHT = 600;
  public final static int DIVIDER_LOCATION = (int)(HEIGHT * .6);

  public BrowserView(LibraryModel model) {
    _library = model;
    _libraryViews = new ArrayList();
    _libraryTables = new ArrayList();
    _tabbedPane = ComponentFactory.createTabbedPane(false);

    this.setPreferredSize(new Dimension(WIDTH, HEIGHT));

    initLibraryViews();

    initTabs();

    setLayout(new BorderLayout());
    add(_tabbedPane);
  }

  private void initTabs() {
    JSplitPane splitPane1, splitPane2, splitPane3, summarySplitPane;
    summarySplitPane = ComponentFactory.createTopBottomSplitPane(true);
    splitPane1 = ComponentFactory.createLeftRightSplitPane();
    splitPane2 = ComponentFactory.createLeftRightSplitPane();
    splitPane3 = ComponentFactory.createLeftRightSplitPane();

    summarySplitPane.setTopComponent(_kbsummary);
    summarySplitPane.setBottomComponent(_kbthumbnail);
    summarySplitPane.setDividerLocation(DIVIDER_LOCATION);
    splitPane1.setLeftComponent(getTable(false));
    splitPane1.setRightComponent(summarySplitPane);
    _tabbedPane.addTab("Summary", splitPane1);

    splitPane2.setLeftComponent(getTable(true));
    splitPane2.setRightComponent(_lattice);
    _tabbedPane.addTab("Dependencies", splitPane2);

    splitPane3.setLeftComponent(getTable(true));
    splitPane3.setRightComponent(_queryPanel);
    _tabbedPane.addTab("Library Query", Icons.getFindIcon(), splitPane3);

    splitPane1.setDividerLocation(WIDTH / 2);
    splitPane2.setDividerLocation(WIDTH / 2);
    splitPane3.setDividerLocation(WIDTH / 2);
  }

  private LibraryTable getTable(boolean checked) {
    LibraryTable table = new LibraryTable();
    LibraryTable temp;
    _library.addLibraryListener(table);
    _libraryViews.add(table);
    table.setHasChecks(checked);
    for(int i = 0; i < _libraryTables.size(); i++) {
      temp = (LibraryTable)_libraryTables.get(i);
      temp.addListener(table);
      table.addListener(temp);
    }
    _libraryTables.add(table);
    return table;
  }

  private void initLibraryViews() {
     _kbsummary = new KnowledgeBaseTable();
    _kbthumbnail = new KnowledgeBaseThumbnail();
    _lattice = new DependenciesLattice();
    _queryPanel = new QueryPanel();

    _library.addLibraryListener(_kbsummary);
    _library.addLibraryListener(_kbthumbnail);
    _library.addLibraryListener(_lattice);
    _library.addLibraryListener(_queryPanel);

    _libraryViews.add(_kbsummary);
    _libraryViews.add(_kbthumbnail);
    _libraryViews.add(_lattice);
    _libraryViews.add(_queryPanel);

  }

  public Collection getLibraryViews() {
    return _libraryViews;
  }

}