import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.border.*;
import javax.swing.BorderFactory;
import java.lang.System.*;
import java.util.Properties;

public class Splash extends JPanel
{
  // Panels
  private JLabel     splashPanel = new JLabel
    (new ImageIcon("images/SplashScreen.jpg"));

  private Color      panelColor = new Color(184, 181, 120);

  //----------------------------------------------------------------------------

  public Splash()
  {
    BorderLayout  lo = new BorderLayout();
    BorderLayout  flo = new BorderLayout();

    lo.setVgap(25);
    setLayout(lo);
    setBorder(BorderFactory.createEmptyBorder(25, 12, 25, 12)); // t, l, b, r

    splashPanel.addMouseListener( new MouseListener() {
        public void mouseClicked( MouseEvent e) {
           try {
             Runtime rt = Runtime.getRuntime();
             String cmd = "java Dashboard";
             Process proc = rt.exec(cmd);
           }
           catch(Exception rte) { };
        }
        public void mousePressed( MouseEvent e) { }
        public void mouseReleased( MouseEvent e) { }
        public void mouseEntered( MouseEvent e) { }
        public void mouseExited( MouseEvent e) { }
    } );

    add(splashPanel, BorderLayout.CENTER);
  }

  //----------------------------------------------------------------------------

  public static void main (String args[])
  {
    JFrame frame = new JFrame ("ARIES Splash");
    JPanel pane = new Splash();

    frame.addWindowListener(new WindowAdapter()
    {
      public void windowClosing(WindowEvent e)
      {
        System.exit(0);
      }
    });

    frame.getContentPane().add (pane, BorderLayout.CENTER);
    frame.pack();
    frame.show();
  }
}
