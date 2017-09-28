package wekavis;

import weka.gui.explorer.ClustererPanel;

/**
 * <p>Title: </p>
 * <p>Description: </p>
 * <p>Copyright: Copyright (c) 2002</p>
 * <p>Company: </p>
 * @author unascribed
 * @version 1.0
 */

public class ClusterViz extends ClustererPanel {

  public ClusterViz() {
    super();
  }

  public void m_Display(String tree, int x, int y)
  {
    visualizeClusterer(tree, x, y);
  }

  public Object getClusterName()
  {
    return m_ClustererEditor.getValue();
  }

  public void m_start()
  {
    startClusterer();
  }
}