package wekavis;

import weka.gui.explorer.ClassifierPanel;

/**
 * <p>Title: </p>
 * <p>Description: </p>
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
    //m_ClassifierEditor.setClassType((new weka.classifiers.j48.J48()).getClass());
  }
}