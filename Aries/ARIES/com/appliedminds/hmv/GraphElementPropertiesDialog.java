package com.appliedminds.hmv;

import java.awt.Frame;
import java.awt.event.WindowEvent;
import java.awt.event.WindowAdapter;
import java.awt.geom.Rectangle2D;
import javax.swing.JDialog;

import com.appliedminds.core.config.ConfigPanel;
import com.appliedminds.core.config.Configurable;
import com.appliedminds.core.config.ConfigBean;


/**
 * A Dialog box to show graph element properties which are represented
 * as beans. It uses the
 * com.appliedminds.martinix.ui.config.ConfigPanel framework to
 * generate editors for each graph element property depending on their
 * type.
 *
 * @see ConfigPanel
 * @author daepark@apmindsf.com
 */
public class GraphElementPropertiesDialog
  extends MyDialog
  implements Configurable
{

  private GraphElementProperties _elementProps;
  private ConfigPanel _configPanel;
  private GraphElementProperties _tmpProps;
  private boolean _changedConfiguration = false;

  public GraphElementPropertiesDialog(Frame parent) {
    super(parent, true);
    setTitle("GraphElement Properties");
    _configPanel = new ConfigPanel(this);
    setContentPane(_configPanel);
    setDefaultCloseOperation(JDialog.DO_NOTHING_ON_CLOSE);
    addWindowListener(new WindowAdapter() {
        public void windowClosing(WindowEvent we) {
          /*
           * Instead of directly closing the window,
           * we're going to change the JOptionPane's
           * value property.
           */
          cancelConfig();
        }
      });

    Rectangle2D bounds = parent.getBounds().getBounds2D();
    setBounds((int)(bounds.getCenterX() - 210),
              (int)(bounds.getCenterY() - 100),
              420, 200);
  }


  /**
   * Need to set the current element properties to display before
   * displaying this dialog.
   */
  public void setGraphElementProperties(GraphElementProperties elementProps) {
    _elementProps = elementProps;
    _configPanel.loadBeans(getConfigBean());
  }


  //
  // begin Configurable interface
  //

  public ConfigBean getConfigBean() {
    // copy the original in case of cancellation
    if (_elementProps != null) {
      _tmpProps = _elementProps.replicate();
    }

    return (_elementProps);
  }

  public void setConfigBean(ConfigBean bean) {
    if (_tmpProps == null && _elementProps != null) {
      // copy the original in case of cancellation
      _tmpProps = _elementProps.replicate();
    }

    _elementProps = (GraphElementProperties)bean;
  }

  public void okConfig() {
    // update current configuration
    _configPanel.updateBeans();

    // apply new configuration
    applyConfiguration(_elementProps);

    // notify dialog handlers
    fireClickedOk();
  }

  public void applyConfig() {
    // update current configuration
    _configPanel.updateBeans();

    // try out the new configuration
    applyConfiguration(_elementProps);

    // notify dialog handlers
    fireClickedApply();
  }

  public void cancelConfig() {
    // revert back to our original configuration
    _elementProps = _tmpProps;
    if (_changedConfiguration) {
      // reset to original configuration
      applyConfiguration(_elementProps);
    }

    // notify dialog handlers
    fireClickedCancel();
  }

  //
  // end Configurable interface
  //


  /**
   * Have any of the GraphElementProperties changed?
   */
  public boolean propertiesChanged() {
    return (_changedConfiguration);
  }


  /**
   * Apply the given configuration to the current element.
   */
  private void applyConfiguration(GraphElementProperties configuration) {

    // apply configuration to the graph panel.
    configuration.applyConfiguration();

    _changedConfiguration = true;
  }

} // end class "GraphElementPropertiesDialg"
