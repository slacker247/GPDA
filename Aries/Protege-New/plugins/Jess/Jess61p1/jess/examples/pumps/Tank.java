//
// Demo tank bean for Jess
// $Id: Tank.java,v 1.2 2003/01/11 01:45:21 ejfried Exp $
//

package jess.examples.pumps;
import javax.swing.JProgressBar;
import java.awt.Component;
import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;
import java.io.Serializable;

public class Tank implements Runnable, Serializable
{
  private int m_level;
  private String m_name;
  private JProgressBar m_bar;

  public Tank(String name)
  {
    m_name = name;
    m_level = 500;
    m_bar = new JProgressBar(JProgressBar.VERTICAL, 0, 1000);
    new Thread(this).start();
  }

  public Component getComponent() { return m_bar; }

  public String getName()
  {
    return m_name;
  }

  public int getLevel()
  {
    return m_level;
  }

  public boolean isHigh()
  {
    return m_level > 750;
  }

  public boolean isLow()
  {
    return m_level < 250;
  }

  public boolean isIntact()
  {
    return (m_level < 1000 && m_level > 0);
  }

  public void addWater(int amt)
  {
    if (amt != 0)
      {
        String name = "level";
        boolean hi = isHigh();
        boolean lo = isLow();
        boolean intact = isIntact();

        int tmp = m_level;
        m_level += amt;

        // Check if any other properties were affected
        if (hi != isHigh() || lo != isLow() || intact != isIntact())
          name = null;

        pcs.firePropertyChange(name, new Integer(tmp),
                               new Integer(m_level));
        // System.out.println("Tank " + getName() + " level now " + m_level);
        m_bar.setValue(m_level);

      }
  }

  public void run()
  {
    while (isIntact())
      {
        addWater(-1);
        try { Thread.sleep(25); } catch (InterruptedException ie) { break; }
      }

    if (m_level >= 1000)
      System.out.println("Tank exploded!");
    else if (m_level <= 0)
      System.out.println("Tank ran dry and caught fire!");
  }


  private PropertyChangeSupport pcs = new PropertyChangeSupport(this);
  public void addPropertyChangeListener(PropertyChangeListener pcl)
  {
    pcs.addPropertyChangeListener(pcl);
  }
  public void removePropertyChangeListener(PropertyChangeListener pcl)
  {
    pcs.removePropertyChangeListener(pcl);
  }

}
