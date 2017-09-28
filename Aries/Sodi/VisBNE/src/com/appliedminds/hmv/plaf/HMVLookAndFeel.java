package com.appliedminds.hmv.plaf;

import com.appliedminds.hmv.HMV;
import java.awt.Component;
import java.util.logging.Logger;
import javax.swing.LookAndFeel;
import javax.swing.SwingUtilities;
import javax.swing.UIManager;
import javax.swing.border.CompoundBorder;
import javax.swing.border.EmptyBorder;
import javax.swing.plaf.ColorUIResource;
import javax.swing.plaf.metal.MetalLookAndFeel;


/**
 * <b>HMVLookAndFeel</b> is not a true javax.swing.LookAndFeel, but
 * know how to apply the look and feel of the HMV which is an extended
 * version of Java's metal look and feel.
 *
 * @author daepark@apmindsf.com
 */
public class HMVLookAndFeel {

  /**
   * Installs the the MetalLookAndFeel using the HMVTheme.
   */
  public void installLookAndFeel(Component app) {
    HMVTheme theme = new HMVTheme();
    MetalLookAndFeel.setCurrentTheme(theme);
    RoundedBorder roundBorder = new RoundedBorder();
    EmptyBorder padding = new EmptyBorder(3, 15, 3, 15);

    try {
      UIManager.setLookAndFeel("javax.swing.plaf.metal.MetalLookAndFeel");

      Object[] uiDefaults = {
        "Button.border", new CompoundBorder(roundBorder, padding),
        "ToolBar.background", theme.getToolBarBackground(),
        "ScrollBar.background", theme.getScrollBarBackground(),
        "ScrollBar.thumb", theme.getScrollBarForeground(),
        "OptionPane.errorIcon", LookAndFeel.makeIcon(getClass(), "icons/Error.gif"),
        "OptionPane.informationIcon", LookAndFeel.makeIcon(getClass(), "icons/Inform.gif"),
        "OptionPane.warningIcon", LookAndFeel.makeIcon(getClass(), "icons/Warn.gif"),
        "OptionPane.questionIcon", LookAndFeel.makeIcon(getClass(), "icons/Question.gif"),
      };
      UIManager.getDefaults().putDefaults(uiDefaults);
      SwingUtilities.updateComponentTreeUI(app);
    }
    catch (Exception e) {
      Logger.getLogger(HMV.LOG_NAME).severe("Unable to load the HMV look and feel: " + e.toString());
    }
  }

} // end class "HMVLookAndFeel"
