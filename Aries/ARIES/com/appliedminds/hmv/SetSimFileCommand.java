package com.appliedminds.hmv;

import java.io.File;
import java.io.IOException;
import java.io.FileNotFoundException;
import java.util.*;
import javax.swing.JFileChooser;
import javax.swing.JOptionPane;
import javax.swing.filechooser.FileFilter;
import com.appliedminds.martini.*;
import com.appliedminds.martinix.fader.FaderUtil;


/**
 * HMWCommand implementation that knows how to set a real time simulation
 * file to selected nodes.
 *
 * @author will@apmindsf.com
 */
public class SetSimFileCommand implements HMVCommand
{
  /** the tool controlling this command */
  private HMVSelectTool2 _tool;

  private JFileChooser _fileChooser;
  private ArrayList _path;


  /**
   * Creates a new instance
   *
   * @param tool the controlling tool
   * @param nodes the Collection of nodes to be assigned a sim file.
   *  we assume this is a reference to the Collection in the tool if
   *  this instance is to be reused.
   */
  public SetSimFileCommand(HMVSelectTool2 tool)
  {
    _tool = tool;
    _path = new ArrayList();
  }


  /**
   * Let the user select a real time simulation file and assign it to
   * the current nodes.
   */
  public void execute()
  {
    HMVSelectionManager mgr = _tool.getApp().getSelectionManager();

    if (mgr == null)
    {
      return;
    }

    if (mgr.selectedNodesCount() == 0)
    {
      return;
    }
    
    // open a file chooser
    JFileChooser fc = getFileChooser();

    GraphPanel panel = _tool.getApp().getGraphPanel();

    while (true)
    {
      int res = fc.showOpenDialog(_tool.getApp());

      if (res == JFileChooser.APPROVE_OPTION)
      {
        File file = fc.getSelectedFile();

        if (file.isFile() && file.exists())
        {
          try
          {
            // parse the file and assign the dataset to the current
            // nodes
            SimulationDataSet simData =
              SimulationDataSet.createSimulationDataSet(file);
            for (Iterator itr=mgr.getSelectedNodes(); itr.hasNext();)
            {
              DrawableNode n = (DrawableNode) itr.next();
              n.setProperty(HMV.PROP_SIMDATA, simData);
              HMVMath.initCalculationPath2(panel.getDrawableGraph(),
                                           n,
                                           _path,
                                           true);
              FaderUtil.setSliderValue(n, 0);
              HMVMath.recalculateProbabilityUsingPath2(n, _path, panel);
            }
            _tool.getApp().prepareSimulation();
          }
          catch (InvalidSimFileException e)
          {
            JOptionPane.showMessageDialog
              (null, "Please select a valid simulation file. (" + 
               e.getMessage() + ")");
            continue;
          }
          catch (IOException e)
          {
            JOptionPane.showMessageDialog
              (null, "Error opening file: " + e.getMessage());
            continue;
          }
        }
        else
        {
          JOptionPane.showMessageDialog
            (null, "Please select a valid simulation file.");
        }
      }

      break;
    }
  }


  /**
   * Not yet implemented - does nothing
   */
  public void undo() {}


  /**
   * @return false - undo is not yet enabled
   */
  public boolean isUndoable() { return false; }


  private JFileChooser getFileChooser()
  {
    if (_fileChooser == null) {
      _fileChooser = new JFileChooser(System.getProperty("user.dir"));
      _fileChooser.setFileFilter(new SimFileFilter());
      _fileChooser.setAcceptAllFileFilterUsed(false);
    }
    return (_fileChooser);
  }


  private static String getExtension(File f)
  {
    String ext = null;
    String s = f.getName();
    int i = s.lastIndexOf('.');

    if (i > 0 && i < s.length() - 1) {
      ext = s.substring(i+1).toLowerCase();
    }
    return ext;
  }


  private class SimFileFilter extends FileFilter
  {
    /**
     * Accept all directories and all .log files.
     */
    public boolean accept(File f)
    {
      if (f.isDirectory()) {
        return (true);
      }

      String extension = getExtension(f);
      if (extension != null) {
        if (extension.equals("log")) {
          return (true);
        }
      }
      return (false);
    }

    /**
     * @return the description of this filter
     */
    public String getDescription()
    {
      return ("HMV simulation files (*.log)");
    }
  }

} // end class SelectSimFileCommand
