package com.appliedminds.hmv;

import java.awt.*;
import java.awt.geom.*;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import com.appliedminds.martini.*;
import com.appliedminds.martinix.fader.FaderUI2;
import com.appliedminds.martinix.fader.FaderUtil;
import com.appliedminds.martinix.ui.TextScreenData;

/**
 * HMWCommand implementation that knows how to run a series of
 * probability "simulations".
 *
 * @author will@apmindsf.com
 */
public class RunSimulationCommand implements HMVCommand, Runnable
{
  /** how much delay between each frame, in msecs */
  private static final long DELAY = 1000;

  /** how many frames are in the simulation. this corresponds to
   *  the number of seconds in the input files  */
  private long _frames;

  private Thread _animator = null;
  private volatile boolean _animating = false;
  private volatile boolean _paused = false;
  private int _frame = 0; // current frame count

  /** the controlling tool */
  private HMVSelectTool2 _hmvTool;

  /** nodes to be manipulated */
  private List _simNodes;

  private long _firstFrameTime;
  private long _lastFrameTime;
  private long _frameInterval = 1000;
  private ArrayList _path;

  private HashMap [] _frameList;

  /**
   * Creates a new RunSimulationCommand instance
   *
   * @param hmvTool the tool that's invoking this command
   */
  public RunSimulationCommand(HMVSelectTool2 hmvTool)
  {
    _hmvTool = hmvTool;
    _simNodes = new ArrayList();
    _path = new ArrayList();
  }


  public void updateTimeline(NodeIterator itr)
  {
    _simNodes.clear();
    _firstFrameTime = -1;
    _lastFrameTime = -1;

    while (itr.hasNext())
    {
      DrawableNode n = itr.next();
      Object dataObj = n.getPropertyObj(HMV.PROP_SIMDATA);
      if (dataObj != null)
      {
        _simNodes.add(n);
        SimulationDataSet dataSet = (SimulationDataSet) dataObj;
        if ((_firstFrameTime < 0) ||
            (_firstFrameTime > dataSet.getFirstEventTime()))
        {
          _firstFrameTime = dataSet.getFirstEventTime();
        }
        if ((_lastFrameTime < 0) ||
            (_lastFrameTime < dataSet.getLastEventTime()))
        {
          _lastFrameTime = dataSet.getLastEventTime();
        }
      }
    }

    // now we know the time range of events. assign them all to
    // each frame "bucket"
    //_frameInterval = (_lastFrameTime - _firstFrameTime) / _frames;
    _frames = (_lastFrameTime - _firstFrameTime) / _frameInterval;

    System.err.println(_frames + " seconds of animation detected");

    _frameList = new HashMap[(int)_frames];
    for (int i=0; i<_frames; i++) {
      _frameList[i] = new HashMap();
    }

    for (Iterator itr2=_simNodes.iterator(); itr2.hasNext();)
    {
      DrawableNode node = (DrawableNode) itr2.next();
      SimulationDataSet dataSet =
        (SimulationDataSet) node.getPropertyObj(HMV.PROP_SIMDATA);

      SimulationDataSet.SimulationData prevData = dataSet.getFirstEventData();

      for(Iterator itr3=dataSet.iterator(); itr3.hasNext();)
      {
        SimulationDataSet.SimulationData data =
          (SimulationDataSet.SimulationData) itr3.next();

        /*
        int frameIdx = (int)((data._date.getTime() - _firstFrameTime) /
          _frameInterval);
        */
        int frameIdx = (int) ((data._date.getTime() - _firstFrameTime) / 1000);

        if (frameIdx == _frames) { // possible for the last event
          frameIdx--;
        }

        /*
        System.err.println("node " + FaderUtil.getNodeLabel(node) +
                           " event placed at frame " + frameIdx);
        */

        // a later value in the same frame will replace the earlier value.
        // (in this case the granularity of frames is too big to resolve
        // the events. either increase the number of frames or fix the
        // input data.)
        _frameList[frameIdx].put(node, new FrameData(node, data));

        // save the previous value in the frame before, for when
        // animated backwards
        if ((frameIdx > 0) && (_frameList[frameIdx - 1].get(node) == null)) {
          _frameList[frameIdx - 1].put(node, new FrameData(node, prevData));
        }

        prevData = data;
      }

      // make sure first frame and last frame contain values for this node
      if (_frameList[0].get(node) == null)
      {
        _frameList[0].put(node,
                          new FrameData(node, dataSet.getFirstEventData()));
      }

      int lastFrame = ((int) _frames) - 1;
      if (_frameList[lastFrame].get(node) == null) 
      {
        _frameList[lastFrame].put(node,
                              new FrameData(node, dataSet.getLastEventData()));
      }
    }
  }


  public boolean isAnimating()
  {
    return _animating;
  }


  public synchronized void haltAnimation()
  {
    _animating = false;
    _paused = false;
    _animator = null;
    setSimulationMenuStateHalted();
    notify();
  }


  public synchronized void pauseAnimation()
  {
    _paused = true;
    _hmvTool.getApp()._runSim.setEnabled(false);
    if ((_frame + 1) < _frames) {
      _hmvTool.getApp()._sfSim.setEnabled(true);
    }
    if (_frame > 0) {
      _hmvTool.getApp()._sbSim.setEnabled(true);
    }
    _hmvTool.getApp()._pauseSim.setEnabled(false);
    _hmvTool.getApp()._contSim.setEnabled(true);
    _hmvTool.getApp()._stopSim.setEnabled(true);

    //_hmvTool.getApp().repaint();
    /*
    _hmvTool.getApp().updateTimeCode(_frame+1,
                                     new Date(_firstFrameTime +
                                              (_frame * _frameInterval)));
    */
    gotoFrame(_frame);
    _hmvTool.getApp().getGraphPanel().paintImmediately();
    notify();
  }


  public synchronized void continueAnimation()
  {
    _animating = true;
    _paused = false;
    _hmvTool.getApp()._runSim.setEnabled(false);
    _hmvTool.getApp()._sfSim.setEnabled(false);
    _hmvTool.getApp()._sbSim.setEnabled(false);
    _hmvTool.getApp()._pauseSim.setEnabled(true);
    _hmvTool.getApp()._contSim.setEnabled(false);
    _hmvTool.getApp()._stopSim.setEnabled(true);
    notify();
  }


  public void stepForward()
  {
    if ((_frame + 1) < (int)_frames) {
      _frame++;
      gotoFrame(_frame);

      _hmvTool.getApp()._sbSim.setEnabled(true);
      if ((_frame + 1) >= _frames) {
        _hmvTool.getApp()._sfSim.setEnabled(false);
      }
      else {
        _hmvTool.getApp()._sfSim.setEnabled(true);
      }
      _hmvTool.getApp().getGraphPanel().paintImmediately();
    }
  }


  public void stepBackward()
  {
    if (_frame > 0) {
      _frame--;
      gotoFrame(_frame);

      _hmvTool.getApp()._sfSim.setEnabled(true);
      if (_frame <= 0) {
        _hmvTool.getApp()._sbSim.setEnabled(false);
      }
      else {
        _hmvTool.getApp()._sbSim.setEnabled(true);
      }
      _hmvTool.getApp().getGraphPanel().paintImmediately();
    }
  }


  /**
   * Run simulation scripts if there's any.
   */
  public synchronized void execute()
  {
    if (!_animating)
    {
      _animating = true;
      _paused = false;
      _frame = 0;
      _hmvTool.getApp()._runSim.setEnabled(false);
      _hmvTool.getApp()._sfSim.setEnabled(false);
      _hmvTool.getApp()._sbSim.setEnabled(false);
      _hmvTool.getApp()._pauseSim.setEnabled(true);
      _hmvTool.getApp()._contSim.setEnabled(false);
      _hmvTool.getApp()._stopSim.setEnabled(true);

      if (_animator == null) {
        _animator = new Thread(this);
      }
      _animator.start();
      notify();
    }
  }


  public void run()
  {
    zeroOutSimNodes();
    GraphPanel panel = _hmvTool.getApp().getGraphPanel();
    DrawableGraph graph = panel.getDrawableGraph();

    long start, end;
    Thread thisThread = Thread.currentThread();

    for (; (_frame<_frames) && (thisThread == _animator); _frame++)
    {
      if (!_animating)
      {
        break;
      }

      try {
        if (_paused) {
          synchronized (this) {
            while (_paused) {
              wait();
            }
          }
        }
      } catch (InterruptedException e) {}

      _hmvTool.getApp()._timeCodeLabel.setVisible(false);
      boolean needRedraw = gotoFrame(_frame);

      // redraw
      start = System.currentTimeMillis();
      if (needRedraw) {
        _hmvTool.getApp().getGraphPanel().paintImmediately();
      }
      _hmvTool.getApp()._timeCodeLabel.setVisible(true);

      end = System.currentTimeMillis();
      //System.err.println("redraw took " +  (end - start) + " msecs\n");

      long delta = end - start;
      if (delta < 0) {
        delta = 0;
      }
      long delay = DELAY - delta;
      if (delay < 0) {
        delay = 0;
      }

      // wait a bit
      try {
        Thread.sleep(delay);
      }
      catch (InterruptedException e){
        System.err.println("  who poked me?!");
      }
    }

    _animating = false;
    _paused = false;
    _animator = null;
    setSimulationMenuStateHalted();
  }


  /**
   * Not implemented.
   */
  public void undo() {}


  /**
   * @return false - undo is not enabled
   */
  public boolean isUndoable() {
    return false;
  }


  public Date getFirstFrameDate()
  {
    return (new Date(_firstFrameTime));
  }


  private void setSimulationMenuStateHalted()
  {
    _hmvTool.getApp()._runSim.setEnabled(true);
    _hmvTool.getApp()._sfSim.setEnabled(false);
    _hmvTool.getApp()._sbSim.setEnabled(false);
    _hmvTool.getApp()._pauseSim.setEnabled(false);
    _hmvTool.getApp()._contSim.setEnabled(false);
    _hmvTool.getApp()._stopSim.setEnabled(false);
  }


  private boolean gotoFrame(int frame)
  {
    long curTime = _firstFrameTime + (frame * _frameInterval);

    //System.err.println("frame " + frame + " at " + new Date(curTime));

    boolean graphChanged = false;
    GraphPanel panel = _hmvTool.getApp().getGraphPanel();
    DrawableGraph graph = panel.getDrawableGraph();

    for (Iterator itr=_frameList[frame].values().iterator(); itr.hasNext();)
    {
      FrameData frameData = (FrameData) itr.next();
      FaderUtil.setSliderValue(frameData._node, frameData._data._prob);
      if (frameData._node.hasChanged())
      {
        HMVMath.initCalculationPath2(graph, frameData._node, _path, false);
        HMVMath.recalculateProbabilityUsingPath2(frameData._node, _path,
                                                 panel);
        graphChanged = true;
      }
    }

    _hmvTool.getApp().updateTimeCode(_frame + 1, new Date(curTime));
    return graphChanged;
  }


  private class FrameData
  {
    DrawableNode _node;
    SimulationDataSet.SimulationData _data;

    public FrameData(DrawableNode node, SimulationDataSet.SimulationData data)
    {
      _node = node;
      _data = data;
    }
  }

  private void zeroOutSimNodes()
  {
    GraphPanel panel = _hmvTool.getApp().getGraphPanel();
    DrawableGraph graph = panel.getDrawableGraph();

    for (Iterator itr=_simNodes.iterator(); itr.hasNext();)
    {
      DrawableNode n = (DrawableNode) itr.next();
      HMVMath.initCalculationPath2(graph, n, _path, false);
      FaderUtil.setSliderValue(n, 0);
      HMVMath.recalculateProbabilityUsingPath2(n, _path, panel);
    }
  }

} // end class RunSimulationCommand
