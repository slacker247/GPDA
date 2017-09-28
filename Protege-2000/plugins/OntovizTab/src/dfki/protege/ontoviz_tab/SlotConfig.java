package dfki.protege.ontoviz_tab;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import edu.stanford.smi.protege.model.Slot;


class SlotConfig implements SlotConfigConstants {

  Slot itsSlot;
  int itsState; // see SlotConfigConstants
  Color itsColor;
  int itsDirection;


  SlotConfig(Slot slot) {
    itsSlot = slot;
    itsState = DEFAULT;
    itsColor = null;
    itsDirection = DIR_DEFAULT;
  }


  Slot getSlot() {
    return itsSlot;
  }

  String getSlotName() {
    return itsSlot.getBrowserText();
  }

  boolean isSystemSlot() {
    return itsSlot.isSystem();
  }

  int getState() {
    return itsState;
  }

  String getStateName() {
    return state[itsState];
  }

  boolean is(int state) {
    return itsState == state;
  }

  void setState(int state) {
    itsState = state;
  }

  Color getColor() {
    return itsColor;
  }

  void setColor(Color color) {
    itsColor = color;
  }

  int getDirection() {
    return itsDirection;
  }

  void setDirection(int direction) {
    itsDirection = direction;
  }

  public String toString() {
    return itsSlot.getBrowserText() + ":" + state[itsState];
  }


  void configure(JFrame frame, JColorChooser colorChooser) {
    setState(CONFIGURED);
    if (itsColor == null) itsColor = Color.black;
    // modal dialog for configuring the color, direction, etc.
    final JDialog dialog = 
      new JDialog(frame, "configure slot " + getSlotName(), true);
    JPanel panel = new JPanel(new BorderLayout());
    JPanel misc = new JPanel();
    JComboBox directionCombo = new JComboBox(directions);
    directionCombo.setSelectedIndex(itsDirection);
    JLabel directionLabel = new JLabel("preferred arrow direction:");
    directionLabel.setForeground(Color.black);
    misc.add(directionLabel);
    misc.add(directionCombo);
    panel.add(misc, BorderLayout.NORTH);
    colorChooser.setColor(itsColor);
    panel.add(colorChooser, BorderLayout.CENTER);
    // Close button
    JPanel buttons = new JPanel();
    JButton close = new JButton("Close");
    close.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent ae) { dialog.hide(); }
    });
    buttons.add(close);
    panel.add(buttons, BorderLayout.SOUTH);
    dialog.getContentPane().add(panel);
    dialog.pack();
    /* old version with color chooser only
    JDialog dialog = JColorChooser.createDialog(component,
      "configure slot " + getSlotName(), true, colorChooser, null, null);
    */
    dialog.show();
    dialog.dispose();
    setColor(colorChooser.getColor());
    setDirection(directionCombo.getSelectedIndex());
  }


}


