package com.appliedminds.hmv;

import com.appliedminds.core.config.GridBagPanel;
import com.appliedminds.hmv.dsg.DSGViewer;
import com.appliedminds.martini.*;
import com.appliedminds.martini.io.GMLOutput;

import java.awt.Frame;
import java.awt.Insets;
import java.awt.event.*;
import java.awt.geom.Rectangle2D;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.*;
import java.net.MalformedURLException;
import java.net.URL;
import javax.swing.*;


/**
 * A custom dialog box to view/edit supporting icon properties.
 *
 * @author daepark@apmindsf.com
 */
public class SupportIconPropertiesDialog extends MyDialog {
  private static final String SUPPORT_DOCUMENT = "document";
  private static final String SUPPORT_GRAPH = "graph";
  private static final String[] SUPPORT_TYPES = {
    SUPPORT_DOCUMENT,
    SUPPORT_GRAPH,
  };
  private static final Insets EMPTY_INSETS = new Insets(0, 0, 0, 0);
  private static final Insets MY_INSETS = new Insets(4, 4, 4, 4);

  private URL _url = null;
  private JOptionPane _optionPane;
  private JComboBox _typeComboBox;
  private JTextField _urlField;

  private Frame _parent;

  public SupportIconPropertiesDialog(Frame parent) {
    super(parent, true);
    _parent = parent;
    setTitle("Supporting Document Properties");

    //
    // Support type row
    //
    GridBagPanel panel = new GridBagPanel();
    panel.add(new JLabel("Type"),
              0, 0, 1, 1,
              GridBagPanel.WEST, GridBagPanel.NONE, 0, 0);

    // Add padding
    panel.add(Box.createHorizontalStrut(5),
              1, 0, 1, 1,
              GridBagPanel.CENTER, GridBagPanel.NONE, 0, 0);

    _typeComboBox = new JComboBox(SUPPORT_TYPES);
    panel.add(_typeComboBox,
              2, 0, 1, 1,
              GridBagPanel.WEST, GridBagPanel.HORIZONTAL, 1, 0);

    //
    // URL field row
    //
    panel.add(new JLabel("URL"),
              0, 1, 1, 1,
              GridBagPanel.WEST, GridBagPanel.NONE, 0, 0);

    // Add padding
    panel.add(Box.createHorizontalStrut(5),
              1, 1, 1, 1,
              GridBagPanel.CENTER, GridBagPanel.NONE, 0, 0);

    _urlField = new JTextField();
    panel.add(_urlField,
              2, 1, 1, 1,
              GridBagPanel.WEST, GridBagPanel.HORIZONTAL, 1, 0);

    Icon openIcon = new ImageIcon(HMVSelectTool.class.getClassLoader().getResource("com/appliedminds/hmv/resources/open.gif"));
    Icon newIcon = new ImageIcon(HMVSelectTool.class.getClassLoader().getResource("com/appliedminds/hmv/resources/circular.gif"));

    final JButton openButton = new MyButton(openIcon);
    final JButton newButton = new MyButton(newIcon);
    openButton.setToolTipText("Find a file");
    newButton.setToolTipText("Create a new supporting graph");
    newButton.setEnabled(false);
    panel.add(openButton,
              3, 1, 1, 1,
              GridBagPanel.WEST, GridBagPanel.NONE, 0, 0);
    panel.add(newButton,
              4, 1, 1, 1,
              GridBagPanel.WEST, GridBagPanel.NONE, 0, 0);

    final String btnString1 = "Ok";
    final String btnString2 = "Cancel";
    final Object[] options = { btnString1, btnString2 };

    _optionPane = new JOptionPane(panel,
                                   JOptionPane.PLAIN_MESSAGE,
                                   JOptionPane.YES_NO_OPTION,
                                   null,
                                   options,
                                   options[0]);

    setContentPane(_optionPane);
    setDefaultCloseOperation(JDialog.DO_NOTHING_ON_CLOSE);
    addWindowListener(new WindowAdapter() {
        public void windowClosing(WindowEvent we) {
          /*
           * Instead of directly closing the window,
           * we're going to change the JOptionPane's
           * value property.
           */
          _optionPane.setValue(new Integer(JOptionPane.CLOSED_OPTION));
        }
      });

    openButton.addActionListener(new ActionListener() {
        private JFileChooser fchooser = null;

        public void actionPerformed(ActionEvent e) {
          if (fchooser == null) {
            fchooser = new JFileChooser(System.getProperty("user.dir"));
            fchooser.setDialogTitle("Choose File");
          }

          File file = null;
          while (true) {
            int retVal = fchooser.showDialog(_parent, "Ok");

            if (retVal == JFileChooser.APPROVE_OPTION) {
              file = fchooser.getSelectedFile();

              if (file.isFile() && file.exists()) {
                try {
                  _urlField.setText(file.toURI().toURL().toString());
                }
                catch (MalformedURLException ignore) {
                  // this should not happen if it's a valid file
                }
              }
              else {
                JOptionPane.showMessageDialog(null, "Please select a valid existing file");
                continue;
              }
            }
            break;
          }
        }
      });


    newButton.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          SupportIconPropertiesDialog.this.setVisible(false);
          final DSGViewer viewer = new DSGViewer();

          final DialogHandler dialogHandler = new DialogHandlerAdapter() {
              public void clickedOk() {
                //
                // For now, always save graph as a new file (by
                // timestamp) in the user's hmv home directory
                //
                boolean fileSaved = false;
                String fileName = System.currentTimeMillis() + ".gml";
                File file = HMV.createNewHomeFile(fileName);

                if (file != null) {
                  DrawableGraph g =
                    viewer.getGraphPanel().getDrawableGraph();
                  DrawableGraphContext ctx =
                    viewer.getGraphPanel().getDrawableGraphContext();
                  BufferedOutputStream out = null;
                  try {
                    out = new BufferedOutputStream(new FileOutputStream(file));
                    GMLOutput.writeGML(g, out,
                                       GMLOutput.ROYERE_HACK, ctx);
                    fileSaved = true;
                  }
                  catch(IOException err) {
                    err.printStackTrace();
                  }
                  finally {
                    try {
                      out.flush();
                      out.close();
                    }
                    catch(Exception ignore) { }
                  }
                }


                //
                // Set url field to saved file
                //
                if (fileSaved && file != null) {
                  try {
                    _urlField.setText(file.toURI().toURL().toString());
                  }
                  catch (MalformedURLException ignore) {
                    // this should not happen if it's a valid file
                  }
                }

                //
                // Close viewer
                //
                viewer.setVisible(false);
                //viewer = null;
                SupportIconPropertiesDialog.this.setVisible(true);
              }

              public void clickedCancel() {
                viewer.setVisible(false);
                //viewer = null;
                SupportIconPropertiesDialog.this.setVisible(true);
              }
            };
          viewer.addDialogHandler(dialogHandler);

          viewer.addWindowListener(new WindowAdapter() {
              public void windowClosing(WindowEvent we) {
                dialogHandler.clickedCancel();
              }
            });

          Rectangle2D bounds = _parent.getBounds().getBounds2D();
          viewer.setBounds((int)(bounds.getCenterX() - 300),
                           (int)(bounds.getCenterY() - 300),
                           600, 600);
          viewer.setVisible(true);
          viewer.setMyGraph(new DrawableGraph());
        }
      });

    _typeComboBox.addItemListener(new ItemListener() {
        public void itemStateChanged(ItemEvent e) {
          String item = (String)e.getItem();
          if (SUPPORT_GRAPH.equals(item)) {
            // graph support type
            switch (e.getStateChange()) {
            case ItemEvent.DESELECTED:
              newButton.setEnabled(false);
              break;
            case ItemEvent.SELECTED:
              newButton.setEnabled(true);
              break;
            default:
              newButton.setEnabled(false);
            }
          }
        }
      });

    _urlField.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          _optionPane.setValue(btnString1);
        }
      });

    _optionPane.addPropertyChangeListener(new PropertyChangeListener() {
        public void propertyChange(PropertyChangeEvent e) {
          String prop = e.getPropertyName();

          if (isVisible()
              && (e.getSource() == _optionPane)
              && (prop.equals(JOptionPane.VALUE_PROPERTY) ||
                  prop.equals(JOptionPane.INPUT_VALUE_PROPERTY))) {
            Object value = _optionPane.getValue();

            if (value == JOptionPane.UNINITIALIZED_VALUE) {
              //ignore reset
              return;
            }

            // Reset the JOptionPane's value.  If you don't do this,
            // then if the user presses the same button next time,
            // no property change event will be fired.
            _optionPane.setValue(JOptionPane.UNINITIALIZED_VALUE);

            if (value.equals(btnString1)) {
              String urlText = _urlField.getText();
              try {
                _url = new URL(urlText);
                SupportIconPropertiesDialog.this.setVisible(false);
                SupportIconPropertiesDialog.this.fireClickedOk();
              }
              catch (MalformedURLException ex) {
                // text was invalid
                _urlField.selectAll();
                JOptionPane.showMessageDialog(SupportIconPropertiesDialog.this,
                                              "Please enter a valid URL.",
                                              "Try Again",
                                              JOptionPane.ERROR_MESSAGE);
              }
            }
            else { // user closed dialog or clicked cancel
              SupportIconPropertiesDialog.this.setVisible(false);
              SupportIconPropertiesDialog.this.fireClickedCancel();
            }
          }
        }
      });

  }

  public void update(String type, URL url) {
    _typeComboBox.setSelectedIndex(0);
    if (type != null) {
      for (int i=0; i<SUPPORT_TYPES.length; i++) {
        if (SUPPORT_TYPES[i].equals(type)) {
          _typeComboBox.setSelectedIndex(i);
          break;
        }
      }
    }

    _url = url;
    if (_url == null) {
      _urlField.setText("");
    }
    else {
      _urlField.setText(_url.toString());
      _urlField.setCaretPosition(0);
    }

    Rectangle2D bounds = _parent.getBounds().getBounds2D();
    setBounds((int)(bounds.getCenterX() - 250),
              (int)(bounds.getCenterY() - 75),
              500, 150);
  }

  public String getType() {
    return ((String)_typeComboBox.getSelectedItem());
  }

  public URL getURL() {
    return (_url);
  }

  /**
   * JButton with empty insets.
   */
  private class MyButton extends JButton {

    public MyButton(Icon icon) {
      super(icon);
    }

    public Insets getInsets() {
      return (MY_INSETS);
    }

  } // end class "MyButton"

} // end class "SupportIconPropertiesDialog"


