package dfki.protege.ontoviz_tab;

import java.awt.*;
import java.awt.event.*;
import java.util.*;
import java.io.*;
import javax.swing.*;
import javax.swing.table.*;
import edu.stanford.smi.protege.widget.*;
import edu.stanford.smi.protege.resource.*;
import edu.stanford.smi.protege.model.*;
import edu.stanford.smi.protege.ui.*;
import edu.stanford.smi.protege.util.*;
import att.grappa.*;


public class OntovizTab extends AbstractWidget {

    InstanceClsesPanel itsClsesPanel;
    GraphPanel itsGraphPanel;
    ExportDot itsLastExport;
    String itsDotFontsize;

    // options
    OptionsTable itsOptionsTable;
    JFrame itsGlobalOptionsFrame;
    SlotConfigTable itsSlotConfigTable;
    JFrame itsSlotConfigFrame;
    JCheckBox itsSaveAsGifCB;
    JCheckBox itsStandardClassCB;
    JCheckBox itsSystemOwnSlotsCB;
    JCheckBox itsInstancesOnlyCB;
    JComboBox itsSlotExtensionDepthInstCombo;
    JComboBox itsSlotExtensionDepthClsCombo;
    JComboBox itsMaxValuesCombo;
    JComboBox itsMaxSlotsCombo;
    JCheckBox itsSlideModeCB;
    JCheckBox itsSlotEdgesDashedCB;
    JCheckBox itsDefaultSlotsHiddenCB;
    JColorChooser itsClassNodeColorChooser;
    JColorChooser itsInstanceNodeColorChooser;
    JColorChooser itsISAEdgeColorChooser;
    JColorChooser itsIOEdgeColorChooser;
    JColorChooser itsInstanceSlotEdgeColorChooser;
    JColorChooser itsClassSlotEdgeColorChooser;


    public void initialize() {
        // setIcon(Icons.getInstancesIcon()); // icon!!!
        setLabel("Ontoviz");
	itsDotFontsize = ApplicationProperties.getString("dot.fontsize", null);
	createGlobalOptionsFrame();
	itsDefaultSlotsHiddenCB = new JCheckBox();
        add(createOntovizSplitter());
        transmitSelection();
    }


    void createGlobalOptionsFrame() {

      itsGlobalOptionsFrame = new JFrame("Ontoviz Global Options");

      JTabbedPane optionsTabs = new JTabbedPane();

      JPanel optionsPanel = new JPanel(new GridLayout(0,1,3,3));

      itsSaveAsGifCB = new JCheckBox("save as gif");
      optionsPanel.add(itsSaveAsGifCB);

      itsStandardClassCB = new JCheckBox("show io :STANDARD-CLASS edges");
      optionsPanel.add(itsStandardClassCB);

      itsSystemOwnSlotsCB = new JCheckBox("show system own slots");
      optionsPanel.add(itsSystemOwnSlotsCB);

      itsSlotEdgesDashedCB = new JCheckBox("slot edges dashed");
      optionsPanel.add(itsSlotEdgesDashedCB);

      itsInstancesOnlyCB = new JCheckBox("show instances only");
      optionsPanel.add(itsInstancesOnlyCB);

      Vector depths = new Vector();
      depths.add(new Integer(1));
      depths.add(new Integer(2));
      depths.add(new Integer(3));
      depths.add(new Integer(4));
      depths.add(new Integer(5));
      depths.add(new Integer(6));
      depths.add(new Integer(7));
      depths.add(new Integer(8));
      depths.add(new Integer(9));
      depths.add("inf");

      Panel slxiPanel = new Panel();
      itsSlotExtensionDepthInstCombo = new JComboBox(depths);
      itsSlotExtensionDepthInstCombo.setSelectedIndex(2);
      itsSlotExtensionDepthInstCombo.setLightWeightPopupEnabled(false);
      itsSlotExtensionDepthInstCombo.setMaximumRowCount(depths.size());
      slxiPanel.add(itsSlotExtensionDepthInstCombo);
      JLabel slxiLabel = 
	new JLabel("maximum depth for slot extension (instances)");
      slxiLabel.setForeground(Color.black);
      slxiPanel.add(slxiLabel);
      optionsPanel.add(slxiPanel);

      Panel slxcPanel = new Panel();
      itsSlotExtensionDepthClsCombo = new JComboBox(depths);
      itsSlotExtensionDepthClsCombo.setLightWeightPopupEnabled(false);
      itsSlotExtensionDepthClsCombo.setMaximumRowCount(depths.size());
      slxcPanel.add(itsSlotExtensionDepthClsCombo);
      JLabel slxcLabel = 
	new JLabel("maximum depth for slot extension (classes)   ");
      slxcLabel.setForeground(Color.black);
      slxcPanel.add(slxcLabel);
      optionsPanel.add(slxcPanel);

      Panel mvPanel = new Panel();
      itsMaxValuesCombo = new JComboBox(depths);
      itsMaxValuesCombo.setSelectedIndex(2);
      itsMaxValuesCombo.setLightWeightPopupEnabled(false);
      itsMaxValuesCombo.setMaximumRowCount(depths.size());
      mvPanel.add(itsMaxValuesCombo);
      JLabel mvLabel = 
	new JLabel("maximum displayed values per slot                    ");
      mvLabel.setForeground(Color.black);
      mvPanel.add(mvLabel);
      optionsPanel.add(mvPanel);

      Panel msPanel = new Panel();
      itsMaxSlotsCombo = new JComboBox(depths);
      itsMaxSlotsCombo.setSelectedIndex(4);
      itsMaxSlotsCombo.setLightWeightPopupEnabled(false);
      itsMaxSlotsCombo.setMaximumRowCount(depths.size());
      msPanel.add(itsMaxSlotsCombo);
      JLabel msLabel = 
	new JLabel("maximum displayed slots per node                     ");
      msLabel.setForeground(Color.black);
      msPanel.add(msLabel);
      optionsPanel.add(msPanel);

      optionsTabs.addTab("general", optionsPanel);

      // colors

      JPanel colorPanel = new JPanel(new GridLayout(0,1));

      itsClassNodeColorChooser = new JColorChooser(Color.black);
      colorPanel.add(setupColorChooser(itsClassNodeColorChooser,
	"pick a color for class nodes", "class node color"));

      itsInstanceNodeColorChooser = new JColorChooser(Color.red);
      colorPanel.add(setupColorChooser(itsInstanceNodeColorChooser,
	"pick a color for instance nodes", "instance node color"));

      itsISAEdgeColorChooser = new JColorChooser(Color.black);
      colorPanel.add(setupColorChooser(itsISAEdgeColorChooser,
	"pick a color for isa edges", "isa edge color"));

      itsIOEdgeColorChooser = new JColorChooser(Color.red);
      colorPanel.add(setupColorChooser(itsIOEdgeColorChooser,
	"pick a color for io edges", "io edge color"));

      itsInstanceSlotEdgeColorChooser = new JColorChooser(Color.blue);
      colorPanel.add(setupColorChooser(itsInstanceSlotEdgeColorChooser,
	"pick a color for instance slot edges", 
	"instance slot edge color"));

      itsClassSlotEdgeColorChooser = new JColorChooser(Color.red);
      colorPanel.add(setupColorChooser(itsClassSlotEdgeColorChooser,
	"pick a color for class slot edges", 
	"class slot edge color"));

      itsSlideModeCB = new JCheckBox("slide mode (inverse colors)");
      colorPanel.add(itsSlideModeCB);

      optionsTabs.addTab("colors", colorPanel);

      itsGlobalOptionsFrame.getContentPane().add(optionsTabs);

      itsGlobalOptionsFrame.pack();

    }


    JButton setupColorChooser(final JColorChooser chooser,
        String title, String buttonTitle) {
      final JButton chooserButton = new JButton(buttonTitle);
      chooserButton.setBackground(Color.white);
      chooserButton.setForeground(chooser.getColor());
      final JDialog colorDialog = JColorChooser.createDialog(
	itsGlobalOptionsFrame, title, false, chooser,
	new ActionListener() { // OK pressed
	  public void actionPerformed(ActionEvent ae) {
	    chooserButton.setForeground(chooser.getColor());
	  }
	},
	new ActionListener() { // CANCEL pressed
	  public void actionPerformed(ActionEvent ae) {
	    // restore the old color from chooserButton (is this a hack?):
	    chooser.setColor(chooserButton.getForeground());
	  }
	});
      chooserButton.addActionListener(new ActionListener() {
	public void actionPerformed(ActionEvent ae) {
	  colorDialog.show();
	}
      });
      return chooserButton;
    }


    JComponent createOntovizSplitter() {
      JSplitPane pane = createLeftRightSplitPane("OntovizTab.left_right", 200);
      pane.setLeftComponent(createClsSplitter());
      pane.setRightComponent(createGraphPanel());
      return pane;
    }


    JComponent createGraphPanel() {
      itsGraphPanel = new GraphPanel();
      /* if we need header buttons:
      ....addHeaderButton(
	new AbstractAction("Create", Icons.getCopyIcon()) {
        public void actionPerformed(ActionEvent event) {
	}
      });
      */
      return itsGraphPanel;
    }


    JComponent createClsSplitter() {
      JPanel clsSplitter = new JPanel(new BorderLayout());
      clsSplitter.add(createOntovizPropertiesPanel(), BorderLayout.NORTH);
      clsSplitter.add(createClsesPanel(), BorderLayout.CENTER);
      return clsSplitter;
    }


    JComponent createOntovizPropertiesPanel() {

      itsOptionsTable = new OptionsTable();
      
      // set up options table
      final JTable optionsTableView = new JTable(itsOptionsTable);
      itsOptionsTable.setupView(optionsTableView);

      JScrollPane optionsTableViewSP = new JScrollPane(optionsTableView);
      // optionsTableViewSP.setBorder(new BevelBorder(BevelBorder.LOWERED));
      optionsTableViewSP.setPreferredSize(new Dimension(200, 150));

      LabeledComponent propsComponent = 
	new LabeledComponent("Config", optionsTableViewSP);

      propsComponent.addHeaderButton(
	new AbstractAction("add class", OntovizIcons.getAddClsIcon()) {
          public void actionPerformed(ActionEvent event) {
	    Collection selection = itsClsesPanel.getSelection();
	    itsOptionsTable.addFrames(selection);
	  }
	}
      );

      propsComponent.addHeaderButton(
	new AbstractAction("add instance", OntovizIcons.getAddInstanceIcon()) {
          public void actionPerformed(ActionEvent event) {
	    Collection classes = itsClsesPanel.getSelection();
	    HashSet instances = new HashSet();
	    for (Iterator cIterator = classes.iterator();
		 cIterator.hasNext();) {
	      Cls cls = (Cls)cIterator.next();
	      instances.addAll(cls.getDirectInstances());
	    }
	    if (!instances.isEmpty()) {
	      showAddableInstances(classes); // instances is not used :-(
	    }
	  }
	}
      );

      propsComponent.addHeaderButton(
	new AbstractAction("remove class", Icons.getRemoveIcon()) {
          public void actionPerformed(ActionEvent event) {
	    int[] selection = optionsTableView.getSelectedRows();
	    itsOptionsTable.deleteRows(selection);
	  }
	}
      );

      /* we first need a nice icon for this ("--"?)
      propsComponent.addHeaderButton(
	new AbstractAction("remove all classes", Icons.getDeleteIcon()) {
          public void actionPerformed(ActionEvent event) {
	    itsOptionsTable.deleteAll();
	  }
	}
      );
      */

      propsComponent.addHeaderButton(
	new AbstractAction("configure slots",
	    OntovizIcons.getConfigureSlotsIcon()) {
          public void actionPerformed(ActionEvent event) {
	    configureSlots();
	  }
	}
      );

      propsComponent.addHeaderButton(
	new AbstractAction("edit global options", 
			   OntovizIcons.getOptionsIcon()) {
          public void actionPerformed(ActionEvent event) {
	    itsGlobalOptionsFrame.show();
	  }
	}
      );

      propsComponent.addHeaderButton(
	new AbstractAction("Create Graph", Icons.getCreateIcon()) {
	 public void actionPerformed(ActionEvent ae) {

          ExportDot export = new ExportDot(getKnowledgeBase());
	  export.setStandardClass(itsStandardClassCB.isSelected());
	  export.setSystemOwnSlots(itsSystemOwnSlotsCB.isSelected());
	  export.setInstancesOnly(itsInstancesOnlyCB.isSelected());
	  export.setSlotExtensionDepthInst(
	    itsSlotExtensionDepthInstCombo.getSelectedItem());
	  export.setSlotExtensionDepthCls(
	    itsSlotExtensionDepthClsCombo.getSelectedItem());
	  export.setMaxValues(itsMaxValuesCombo.getSelectedItem());
	  export.setMaxSlots(itsMaxSlotsCombo.getSelectedItem());
	  export.setSlotConfigTable(itsSlotConfigTable);
	  export.setDefaultSlotsHidden(itsDefaultSlotsHiddenCB.isSelected());
	  export.setSlideMode(itsSlideModeCB.isSelected());
	  export.setSlotEdgesDashed(itsSlotEdgesDashedCB.isSelected());
	  export.setClassNodeColor(itsClassNodeColorChooser.getColor());
	  export.setInstanceNodeColor(itsInstanceNodeColorChooser.getColor());
	  export.setISAEdgeColor(itsISAEdgeColorChooser.getColor());
	  export.setIOEdgeColor(itsIOEdgeColorChooser.getColor());
	  export.setInstanceSlotEdgeColor(itsInstanceSlotEdgeColorChooser.getColor());
	  export.setClassSlotEdgeColor(itsClassSlotEdgeColorChooser.getColor());
	  // ... add other global options ... !!!
	  export.setOptionsTable(itsOptionsTable);

	  // get file name (without suffix)
	  String file = getProject().getProjectFilePath();
	  if (file == null)
	    file = "ontology";
	  else { // drop suffix (usually .pprj)
	    PathSplitter splitter = new PathSplitter(file);
	    file = splitter.getAbsoluteDirectory()
	      + File.separator
	      + splitter.getSimpleName();
	  }

	  String inputFile = file + ".dot-input";
	  String dotFile = file + ".dot";

          try {
	    export.export(inputFile);
	    export.dot(inputFile, dotFile);
	    if (itsSaveAsGifCB.isSelected()) {
	      String gifFile = file + ".gif";
	      export.dot(inputFile, gifFile, "-Tgif", false); // don't wait
	    }
            FileInputStream input = new FileInputStream(dotFile);
            Parser graphParser = new Parser(input, System.err);
	    graphParser.parse();
            Graph newGraph = graphParser.getGraph();
	    if (itsDotFontsize != null)
	      newGraph.setGrappaAttribute(
	        GrappaConstants.GRAPPA_FONTSIZE_ADJUSTMENT_ATTR, itsDotFontsize);
	    itsGraphPanel.setGraph(newGraph);
	    itsGraphPanel.addGrappaListener(
	      new OntovizGrappaAdapter(OntovizTab.this));
	    itsLastExport = export;
          } catch (Exception e) {
	    // System.out.println(e);
	    e.printStackTrace();
	    itsGraphPanel.setGraph(null);
	    itsLastExport = null;
          }
	}
      });

      return propsComponent;

    }


    void showAddableInstances(Collection classes) {

      final JFrame frame = new JFrame("Select Instances");
      JPanel panel = new JPanel(new BorderLayout());
      frame.getContentPane().add(panel);

      // instances list
      final DirectInstancesList instancesList = 
	new DirectInstancesList(getProject());
      instancesList.setClses(classes);
      panel.add(instancesList, BorderLayout.CENTER);

      // buttons
      JPanel buttons = new JPanel();

      JButton addButton = new JButton("add");
      addButton.addActionListener(new ActionListener() {
	public void actionPerformed(ActionEvent ae) {
	  Collection selection = instancesList.getSelection();
	  itsOptionsTable.addFrames(selection);
	}
      });
      buttons.add(addButton);

      JButton closeButton = new JButton("close");
      closeButton.addActionListener(new ActionListener() {
	public void actionPerformed(ActionEvent ae) {
	  frame.hide();
	  frame.dispose();
	}
      });
      buttons.add(closeButton);

      panel.add(buttons, BorderLayout.SOUTH);

      // show
      frame.pack();
      frame.show();

    }


    JComponent createClsesPanel() {
        itsClsesPanel = new InstanceClsesPanel(getProject());
        itsClsesPanel.addSelectionListener(new SelectionListener() {
            public void selectionChanged(SelectionEvent event) {
                transmitSelection();
            }
        });
        return itsClsesPanel;
    }


    void grappaSelect(Collection frames) {
      Vector ids = new Vector();
      for (Iterator frameIterator = frames.iterator();
	   frameIterator.hasNext();) {
	Instance frame = (Instance)frameIterator.next();
	ids.addElement(frame.getName());
      }
      itsGraphPanel.selectMultiple(ids);
    }


    void transmitSelection() {
        Collection selection = itsClsesPanel.getSelection();
	grappaSelect(selection);
    }


    public void makeClassVisible(String id) {
      // id is either a class or an instance id!
      itsClsesPanel.makeClassVisible(id);
    }


    void configureSlots() {

      if (itsSlotConfigFrame != null) { // close old one
	itsSlotConfigFrame.hide();
	itsSlotConfigFrame.dispose();
      }

      itsSlotConfigFrame = new JFrame("Ontoviz Slot Configuration");

      JPanel panel = new JPanel(new BorderLayout());

      JPanel topPanel = new JPanel();
      topPanel.add(itsDefaultSlotsHiddenCB);
      JLabel label = new JLabel("hide slots as default");
      label.setForeground(Color.black);
      topPanel.add(label);
      panel.add(topPanel, BorderLayout.NORTH);

      itsSlotConfigTable = 
	new SlotConfigTable(getKnowledgeBase(), itsSlotConfigTable);
      JTable slotConfigTableView = new JTable(itsSlotConfigTable);
      itsSlotConfigTable.setupView(slotConfigTableView, itsSlotConfigFrame);

      JScrollPane scrollPane = new JScrollPane(slotConfigTableView);
      scrollPane.setPreferredSize(new Dimension(300,200));

      panel.add(scrollPane, BorderLayout.CENTER);

      JPanel buttonPanel = new JPanel();

      JButton setDefault = new JButton("default");
      setDefault.addActionListener(new ActionListener() {
	public void actionPerformed(ActionEvent ae) {
	  itsSlotConfigTable.setDefault();
	}
      });
      buttonPanel.add(setDefault);

      JButton ok = new JButton("close");
      ok.addActionListener(new ActionListener() {
	public void actionPerformed(ActionEvent ae) {
	  itsSlotConfigFrame.hide();
	}
      });
      buttonPanel.add(ok);

      panel.add(buttonPanel, BorderLayout.SOUTH);

      itsSlotConfigFrame.getContentPane().add(panel);

      // show frame

      itsSlotConfigFrame.pack();
      itsSlotConfigFrame.show();

    }



/*inner*/ class InstanceClsesPanel extends SelectableContainer {

    Project itsProject;
    SelectableTree itsClsTree;

    public InstanceClsesPanel(Project project) {
        itsProject = project;
        this.add(createClsesPanel());
        setSelectable(itsClsTree);
    }

    JComponent createClsesPanel() {
        Cls root = itsProject.getKnowledgeBase().getRootCls();
	itsClsTree = ComponentFactory.createSelectableTree(null, new ParentChildRoot(root));
	FrameRenderer renderer = new FrameRenderer();
	renderer.setDisplayDirectInstanceCount(true);
	itsClsTree.setCellRenderer(renderer);
	itsClsTree.setSelectionRow(0);
	LabeledComponent c = new LabeledComponent("Classes", new JScrollPane(itsClsTree));
        c.addHeaderButton(getViewClsAction());
	c.setFooterComponent(new ClsTreeFinder(itsProject.getKnowledgeBase(), itsClsTree, "Find Class"));
	return c;
    }

    Action getViewClsAction() {
      return new AbstractAction("View Selected Class", Icons.getViewIcon()) {
       	public void actionPerformed(ActionEvent event) {
       	  Iterator i = ComponentUtilities.getSelection(itsClsTree).iterator();
          while (i.hasNext()) {
       	    Cls cls = (Cls) i.next();
            itsProject.show(cls);
          }
        }
      };
    }

    void makeClassVisible(String id) {
      Instance instance = getKnowledgeBase().getInstance(id);
      Cls cls = null;
      if (instance instanceof Cls)
	cls = (Cls)instance;
      else if (instance != null) {
	cls = instance.getDirectType();
      }
      if (cls != null)
	makeClassVisible(cls);
    }

    void makeClassVisible(Cls cls) {
      // find path of classes from root to cls
      ArrayList clses = new ArrayList();
      getPathToRoot(cls, clses);
      Collections.reverse(clses);
      ComponentUtilities.setSelectedObjectPath(itsClsTree, clses);
    }

    void getPathToRoot(Cls cls, Collection clses) {
	// copied from ClsTreeFinder :-( !!!
        Collection rootClses = (Collection) 
	  ((LazyTreeNode)itsClsTree.getModel().getRoot()).getUserObject();
        clses.add(cls);
        Collection superclasses = cls.getDirectSuperclasses();
        Cls parent = (Cls) CollectionUtilities.getFirstItem(superclasses);
	if (itsLastExport != null) {
	  // find a parent that was accepted on last export
	  for (Iterator scIterator = superclasses.iterator();
	       scIterator.hasNext();) {
	    Cls pcls = (Cls)scIterator.next();
	    if (itsLastExport.acceptFrame(pcls)) {
	      // use this class instead of the first parent
	      // so the JTree shows a path that is visible in the graph
	      // (one would need to compute ALL paths to find the best =
	      // longest but this greedy algorithm should suffice)
	      parent = pcls;
	      break;
	    }
	  }
	}
        if (parent == null) {
            // Log.error("no parents", this, "getPathToRoot", cls);
        } else if (rootClses.contains(parent)) {
            clses.add(parent);
        } else {
            getPathToRoot(parent, clses);
        }
    }

} // inner class


}


