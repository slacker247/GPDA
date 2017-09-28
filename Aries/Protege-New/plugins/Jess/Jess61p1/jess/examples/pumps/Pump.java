//
// Demo pump bean for Jess
// $Id: Pump.java,v 1.2 2003/01/11 01:45:21 ejfried Exp $
//

package jess.examples.pumps;
import javax.swing.JProgressBar;
import java.awt.Component;
import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;
import java.io.Serializable;

public class Pump implements Runnable, Serializable
{
  private int m_flowRate;
  private Tank m_tank;
  private String m_name;
  private JProgressBar m_bar;

  public Pump(String name, Tank tank)
  {
    m_name = name;
    m_tank = tank;
    m_flowRate = 0;
    m_bar = new JProgressBar(JProgressBar.VERTICAL, 0, 30);
    new Thread(this).start();
  }

  public Component getComponent() { return m_bar; }

  public String getName()
  {
    return m_name;
  }

  public int getFlow()
  {
    return m_flowRate;
  }

  public void setFlow(int flowRate)
  {
    if (flowRate >= 0 && flowRate != m_flowRate)
      {
        int tmp = m_flowRate;
        m_flowRate = flowRate;
        m_bar.setValue(m_flowRate);

        pcs.firePropertyChange("flow", new Integer(tmp),
                               new Integer(flowRate));
      }
  }

  public void run()
  {
    while (m_tank.isIntact())
      {
        m_tank.addWater(m_flowRate);
        try { Thread.sleep(90); } catch (InterruptedException ie) { return; }
      }
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
