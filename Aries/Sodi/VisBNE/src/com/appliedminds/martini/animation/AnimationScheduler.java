/**
 * Copyright (C) 2001-@year@ by University of Maryland, College Park,
 * MD 20742, USA All rights reserved.
 */
package com.appliedminds.martini.animation;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import javax.swing.Timer;


/**
 * <b>AnimationScheduler</b> schedules the frames of Animation
 * objects. It uses a <b>javax.swing.Timer</b> as an event dispatch
 * thread. Each time the timer fires, the sheduler animates one frame
 * from all animations that have been scheduled and are ready to
 * animate at that time.
 *
 * <p>Normally you will not need to use this class directly, unless
 * you are creating your own custom subclass of Animation. This class
 * is a singleton, only one instance should ever exist at the same
 * time. This instance is accessed through the <code>getInstance</code>
 * method.
 *
 * @author Jesse Grosjean
 * @author daepark@apmindsf.com
 */
public class AnimationScheduler implements ActionListener {

  /** the one and only instance of the scheduler */
  private static AnimationScheduler _instance;
  private static byte[] _lock = new byte[0];

  private Timer _timer;
  private int  _frameDelay;
  private long  _currentTime;
  private long  _currentFrame;
  private List  _processList;

  private PriorityQueue _timeConditionQueue;
  private PriorityQueue _frameConditionQueue;


  /**
   * Get the one and only instance of the AnimationScheduler
   */
  public static AnimationScheduler getInstance() {
    if (_instance == null) {
      synchronized (_lock) {
        if (_instance == null) {
          _instance = new AnimationScheduler();
        }
      }
    }
    return (_instance);
  }


  /**
   * Return the current time for the animation scheduler. All
   * animations should use this instead of
   * System.currentTimeMillis(). This will make sure that all
   * animations in the system are synchronized.
   */
  public long getCurrentTime() {
    return (_currentTime);
  }


  /**
   * Return the current frame the animation scheduler is or will
   * soon be drawing.
   */
  public long getCurrentFrame() {
    return (_currentFrame);
  }


  /**
   * Return the delay between each frame that gets animated. If the
   * animations take a long time to animate the actual frame delay may
   * be greater then this value.
   */
  public int getFrameDelay() {
    return (_frameDelay);
  }


  /**
   * Set the delay between each frame that gets animated. If the
   * animations take a long time to animate the actual frame delay may
   * be greater then this value.
   */
  public void setFrameDelay(int frameDelay) {
    boolean wasRunning = isRunning();

    if (wasRunning) {
      stopAnimationScheduler();
      // _timer.stop();
    }

    _frameDelay = frameDelay;
    _timer.setDelay(_frameDelay);

    if (wasRunning) {
      startAnimationScheduler();
      // _timer.start();
    }
  }


  /**
   * Return true if the animation scheduler is running.
   */
  public boolean isRunning() {
    return (_timer.isRunning());
  }


  /**
   * This method is called when the timer fires. This method
   * in turn calls <code>processAnimations</code> with the current time.
   */   
  public void actionPerformed(ActionEvent e) {
    processAnimations(System.currentTimeMillis());
  }


  /**
   * Schedule an animation together with its frame condition. Normally
   * this will be done automatically by
   * </code>Animation.play</code>. NOTE the current implementation
   * requires that the condition parameter inherits from either
   * NextFrameOnElapsedTime or NextFrameOnElapsedFrames.
   */
  public void scheduleAnimation(Animation animation, 
                                NextFrameCondition condition)
  {
    if (condition.isTimeCondition()) {
      _timeConditionQueue.insert(new ScheduledAnimation(animation, 
                                                        condition));
    }
    else if (condition.isFrameCondition()) {
      _frameConditionQueue.insert(new ScheduledAnimation(animation, 
                                                         condition));
    }

    if (!isRunning()) {
      startAnimationScheduler();
    }
  }


  protected synchronized void startAnimationScheduler() {
    _timer.start();
  }


  protected synchronized void stopAnimationScheduler() {
    _timer.stop();
  }
        

  /**
   * Gets all animations that are ready to animate at the specified
   * time or for the current frame and animates one frame for each
   * one. Last of all it increments the current frame.
   */
  protected void processAnimations(long currentTime) {
    _currentTime = currentTime;

    Iterator itr = getAnimationsToProcessForCurrentFrame();
    while (itr.hasNext()) {
      Animation animation = (Animation)itr.next();

      if (!animation.isStopped()) {
        animation.animateFrameForTime(_currentTime);
      }
    } 

    if (_timeConditionQueue.isEmpty() && _frameConditionQueue.isEmpty()) {
      stopAnimationScheduler();
    }
                
    _currentFrame++;
  }


  /**
   * Return an iterator for all animations that should be animated for
   * the current frame.  The NextFrameCondition associate with the
   * animation uses the values of <code>getCurrentFrame</code> and
   * <code>getCurrentTime</code> to determine if an animation is ready
   * to be animated for the current frame. Since all animations use
   * these common values, animations that are scheduled to start at the
   * same time will always start animating in the same frame.
   */
  protected Iterator getAnimationsToProcessForCurrentFrame() {
    _processList.clear();

    while (!_timeConditionQueue.isEmpty()) {
      ScheduledAnimation sa = (ScheduledAnimation)_timeConditionQueue.first();
      if (sa.isReadyToAnimate()) {
        _timeConditionQueue.extractFirst();
        _processList.add(sa.getAnimation());
      }
      else {
        break;
      }
    }

    while (!_frameConditionQueue.isEmpty()) {
      ScheduledAnimation sa = (ScheduledAnimation)_frameConditionQueue.first();
      if (sa.isReadyToAnimate()) {
        _frameConditionQueue.extractFirst();
        _processList.add(sa.getAnimation());
      }
      else {
        break;
      }
    }

    return (_processList.iterator());
  }


  /**
   * Private constructor. Use <code>getInstance</code> to get an instance
   * of an AnimationScheduler.
   */
  private AnimationScheduler() {
    _frameDelay = 20;
    _currentTime = 0;
    _currentFrame = 0;
    _processList = new ArrayList();
    _timer = new Timer(_frameDelay, this);
    _timeConditionQueue = new PriorityQueue();
    _frameConditionQueue = new PriorityQueue();
  }

        
  /**
   * A ScheduledAnimation always has a NextFrameCondition associated
   * with it.
   */
  private class ScheduledAnimation implements Comparable {

    private Animation           __animation;
    private NextFrameCondition __condition;

    public ScheduledAnimation(Animation animation, 
                              NextFrameCondition condition) 
    {
      __animation = animation;
      __condition = condition;
    }

    public boolean isReadyToAnimate() {
      return (__condition.isReadyToAnimate());
    }

    public Animation getAnimation() {
      return (__animation);
    }

    public NextFrameCondition getNextFrameCondition() {
      return (__condition);
    }

    public int compareTo(Object o) {
      ScheduledAnimation other = (ScheduledAnimation) o;
      return (__condition.compareTo(other.getNextFrameCondition()));
    }

  } // end class ScheduledAnimation

} // end class AnimationScheduler
