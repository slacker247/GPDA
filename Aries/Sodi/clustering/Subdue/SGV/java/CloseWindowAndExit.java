import java.awt.event.*;
import java.awt.*;

public class CloseWindowAndExit extends WindowAdapter {

  public void windowClosing(WindowEvent e) {
    Window curWind = e.getWindow();
    curWind.dispose();
    
  }
}

