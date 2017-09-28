package wekavis;

import weka.core.Instances;
import weka.core.FastVector;
import weka.core.Attribute;
import weka.classifiers.evaluation.MarginCurve;
import weka.classifiers.evaluation.ThresholdCurve;
import weka.classifiers.evaluation.CostCurve;
import weka.gui.visualize.VisualizePanel;
import weka.gui.visualize.PlotData2D;
import weka.gui.visualize.Plot2D;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;
import java.awt.event.InputEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import javax.swing.JPopupMenu;
import javax.swing.JMenu;
import javax.swing.JMenuItem;

import weka.gui.explorer.ClassifierPanel;
import java.awt.BorderLayout;
import weka.gui.treevisualizer.PlaceNode2;

/**
 * <p>Title: Rule Tree Panel</p>
 * <p>Description: Allows WekaVis to compute and display the Rule Tree</p>
 * <p>Copyright: Copyright (c) 2002</p>
 * <p>Company: </p>
 * @author unascribed
 * @version 1.0
 */

public class ClassViz extends ClassifierPanel {

  weka.classifiers.j48.J48 Classifier = new weka.classifiers.j48.J48();

  public ClassViz() {
    super();
  }

  public void m_start()
  {
    startClassifier();
  }

  public void m_setClassifier()
  {
    String szTemp[] = new String[4];
    szTemp[0] = "-C";
    szTemp[1] = "0.25";
    szTemp[2] = "-M";
    szTemp[3] = "2";

    try
    {
      Classifier.setOptions(szTemp);
    }catch(Exception ioe)
    {
      ioe.printStackTrace();
    }
    m_ClassifierEditor.setClassType(Classifier.getClass());
    m_ClassifierEditor.setValue(new weka.classifiers.j48.J48());

    /**
     * Possibly capture the right mouse click and perform the desired action
     * from there.
     */

   m_History.getList().addMouseListener(new MouseAdapter() {
       public void mouseClicked(MouseEvent e) {
         if ((e.getModifiers() & InputEvent.BUTTON1_MASK)
             == InputEvent.BUTTON1_MASK) {
         } else {
           int index = m_History.getList().locationToIndex(e.getPoint());
           if (index != -1) {
             String name = m_History.getNameAtIndex(index);
             visualize1(name, e.getX(), e.getY());
           }
         }
       }
     });
  }

  public String getTreeGraph() throws Exception
  {
    return null;
  }

  public void saveData(String fileName)
  {
    saveBuffer(fileName);
  }

      /**
       * Handles constructing a popup menu with visualization options.
       * @param name the name of the result history list entry clicked on by
       * the user
       * @param x the x coordinate for popping up the menu
       * @param y the y coordinate for popping up the menu
       */
      protected void visualize1(String name, int x, int y) {
        final String selectedName = name;
        JPopupMenu resultListMenu = new JPopupMenu();

        JMenuItem visMainBuffer = new JMenuItem("View in main window");
        visMainBuffer.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
              m_History.setSingle(selectedName);
            }
          });
        resultListMenu.add(visMainBuffer);

        JMenuItem visSepBuffer = new JMenuItem("View in separate window");
        visSepBuffer.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
              m_History.openFrame(selectedName);
            }
          });
        resultListMenu.add(visSepBuffer);

        JMenuItem saveOutput = new JMenuItem("Save result buffer");
        saveOutput.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
              saveBuffer(selectedName);
            }
          });
        resultListMenu.add(saveOutput);
        resultListMenu.addSeparator();

        FastVector o = (FastVector)m_History.getNamedObject(selectedName);

        if (o != null) {
          VisualizePanel temp_vp = null;
          String temp_grph = null;
          FastVector temp_preds = null;
          Attribute temp_classAtt = null;

          for (int i = 0; i < o.size(); i++) {
            Object temp = o.elementAt(i);
            if (temp instanceof VisualizePanel) { // normal errors
              temp_vp = (VisualizePanel)temp;
            } else if (temp instanceof String) { // graphable output
              temp_grph = (String)temp;
            } else if (temp instanceof FastVector) { // predictions
              temp_preds = (FastVector)temp;
            } else if (temp instanceof Attribute) { // class attribute
              temp_classAtt = (Attribute)temp;
            }
          }

          final VisualizePanel vp = temp_vp;
          final String grph = temp_grph;
          final FastVector preds = temp_preds;
          final Attribute classAtt = temp_classAtt;

          JMenuItem visErrors = new JMenuItem("Visualize classifer errors");
          if (vp != null) {
            visErrors.addActionListener(new ActionListener() {
                public void actionPerformed(ActionEvent e) {
                  visualizeClassifierErrors(vp);
                }
              });
            resultListMenu.add(visErrors);
          }
          JMenuItem visTree = new JMenuItem("Visualize tree 1");
          if (grph != null) {
            visTree.addActionListener(new ActionListener() {
                public void actionPerformed(ActionEvent e) {
                  visualizeTree1(grph,vp.getName());
                }
              });
            resultListMenu.add(visTree);
          }
          JMenuItem visMargin = new JMenuItem("Visualize margin curve");
          if (preds != null) {
            visMargin.addActionListener(new ActionListener() {
                public void actionPerformed(ActionEvent e) {
                  try {
                    MarginCurve tc = new MarginCurve();
                    Instances result = tc.getCurve(preds);
                    VisualizePanel vmc = new VisualizePanel();
                    vmc.setName(result.relationName());
                    vmc.setLog(m_Log);
                    PlotData2D tempd = new PlotData2D(result);
                    tempd.setPlotName(result.relationName());
                    tempd.addInstanceNumberAttribute();
                    vmc.addPlot(tempd);
                    visualizeClassifierErrors(vmc);
                  } catch (Exception ex) {
                    ex.printStackTrace();
                  }
                }
              });
            resultListMenu.add(visMargin);
          }
          JMenu visThreshold = new JMenu("Visualize threshold curve");
          if (preds != null && classAtt != null) {
            for (int i = 0; i < classAtt.numValues(); i++) {
              JMenuItem clv = new JMenuItem(classAtt.value(i));
              final int classValue = i;
              clv.addActionListener(new ActionListener() {
                  public void actionPerformed(ActionEvent e) {
                    try {
                      ThresholdCurve tc = new ThresholdCurve();
                      Instances result = tc.getCurve(preds, classValue);
                      VisualizePanel vmc = new VisualizePanel();
                      vmc.setLog(m_Log);
                      vmc.setName(result.relationName()+". Class value "+
                                  classAtt.value(classValue)+")");
                      PlotData2D tempd = new PlotData2D(result);
                      tempd.setPlotName(result.relationName());
                      tempd.addInstanceNumberAttribute();
                      vmc.addPlot(tempd);
                      visualizeClassifierErrors(vmc);
                    } catch (Exception ex) {
                      ex.printStackTrace();
                    }
                  }
                });
              visThreshold.add(clv);
            }
            resultListMenu.add(visThreshold);
          }
          JMenu visCost = new JMenu("Visualize cost curve");
          if (preds != null && classAtt != null) {
            for (int i = 0; i < classAtt.numValues(); i++) {
              JMenuItem clv = new JMenuItem(classAtt.value(i));
              final int classValue = i;
              clv.addActionListener(new ActionListener() {
                  public void actionPerformed(ActionEvent e) {
                    try {
                      CostCurve cc = new CostCurve();
                      Instances result = cc.getCurve(preds, classValue);
                      VisualizePanel vmc = new VisualizePanel();
                      vmc.setLog(m_Log);
                      vmc.setName(result.relationName()+". Class value "+
                                  classAtt.value(classValue)+")");
                      PlotData2D tempd = new PlotData2D(result);
                      tempd.m_displayAllPoints = true;
                      tempd.setPlotName(result.relationName());
                      boolean [] connectPoints =
                        new boolean [result.numInstances()];
                      for (int jj = 1; jj < connectPoints.length; jj+=2) {
                        connectPoints[jj] = true;
                      }
                      tempd.setConnectPoints(connectPoints);
                      //		  tempd.addInstanceNumberAttribute();
                      vmc.addPlot(tempd);
                      visualizeClassifierErrors(vmc);
                    } catch (Exception ex) {
                      ex.printStackTrace();
                    }
                  }
                });
              visCost.add(clv);
            }
            resultListMenu.add(visCost);
          }
        }
        resultListMenu.show(m_History.getList(), x, y);
      }

      /**
       * Pops up a TreeVisualizer for the classifier from the currently
       * selected item in the results list
       * @param dottyString the description of the tree in dotty format
       * @param treeName the title to assign to the display
       */
      protected void visualizeTree1(String dottyString, String treeName) {
        final javax.swing.JFrame jf =
          new javax.swing.JFrame("Weka Classifier Tree Visualizer 1: "+treeName);
        jf.setSize(500,400);
        jf.getContentPane().setLayout(new BorderLayout());
        TreeVis tv = new TreeVis(null,
                                    dottyString,
                                    new PlaceNode2());

        tv.diffInNodes();
        tv.nodesToFile();

        jf.getContentPane().add(tv, BorderLayout.CENTER);
        jf.addWindowListener(new java.awt.event.WindowAdapter() {
            public void windowClosing(java.awt.event.WindowEvent e) {
              jf.dispose();
            }
          });

        jf.setVisible(true);
      }
}