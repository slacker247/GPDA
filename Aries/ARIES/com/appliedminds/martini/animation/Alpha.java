/**
 * Copyright (C) 2001-@year@ by University of Maryland, College Park,
 * MD 20742, USA All rights reserved.
 */
package com.appliedminds.martini.animation;


/**
 * <b>Alpha</b> provides a uniform way to generate alpha values
 * (floats that range from 0.0 to 1.0) over time. This classes
 * interface is heavily based on the class javax.media.j3d.Alpha used
 * with Java3D. You can read about that class here for supplemental
 * documention here. http://java.sun.com/products/java-media/3D/.  
 *
 * <p>Each Animation is associated with a Alpha. Animations and the
 * animation system in general use alpha objects in two ways.  
 *
 * <p> Its first purpose is to generate alpha values that animation classes
 * use to interpolate from one value to another. For example the
 * TransformAnimation uses a Alpha to interpolate between its source
 * and destination transforms over time. By changing the alpha
 * function you it is easy to change the rate that these
 * interpolations happen at. The methodes concenrted with the alpha
 * function aspect are: 
 *
 * <p>
 * <ul>
 * <li><code>setIncreasingAlphaDuration</code> - Time spent going from 0 to 1
 * <li><code>setIncreasingAlphaRampDuration</code> - Time spend accelerating and decelearting during the increasing phase.
 * <li><code>setAlphaAtOneDuration</code> - Time spent at value 1 after the increasing phase.
 * <li><code>setDecreasingAlphaDuration</code> - Time spent going from 1 to 0.
 * <li><code>setDecreasingAlphaRampDuration</code> - Time spend accelerating and decelearting during the decreasing phase.
 * <li><code>setAlphaAtZeroDuration</code> - Time spent at value 0 after the decreasing phase.
 * </ul>
 *
 * <p> The second way that alphas are used is to schedule when the
 * animation will start when it will stop. The methods concerned with
 * the scheduling aspect are: 
 *
 * <p>
 * <ul>
 * <li><code>setMode</code> - ALPHA_INCREASING = (0 to 1); ALPHA_DECREASING = (1 to 0); ALPHA_INCREASING_AND_DECREASING = (0 to 1 to 0).
 * <li><code>setLoopCount</code> - Number of times to loop through all alpha values.
 * <li><code>setTriggerTime</code> - Time to start the alpha.
 * <li><code>setPhaseDelayDuration</code> - Time to wait after the trigger time until really starting the alpha.
 * <li><code>isStarted</code> - Returns true if the alpha has started for the given time parameter.
 * <li><code>isFinished</code> - Returns true if the alpha has finished for the given time parameter.
 * </ul>
 *
 * <p> Alpha objects can seem difficult to create with their many
 * parameters and options. The convienience methods
 * <code>Alpha.createStandardAlpha</code> and
 * <code>Alpha.createStandardSlowInSlowOutAlpha</code> allow for
 * quick and easy object creation. The following example code shows
 * how to construct more complicated alpha objects.
 *
 * <p>
 * <code>
 * <pre>
 * // Create a new Alpha that starts imediatly, runs once, 
 * // changes linearly from 0 to 1, has a duration of one second.
 * int loopCount = 1;
 * long increasingDuration = 1000;
 * alpha = new Alpha(loopCount, increasingDuration);
 *
 *
 * // Create a new Alpha that starts two seconds after it is constructed,
 * // runs once, changes linearly from 0 to 1, has a duration of one second.
 * int loopCount = 1;
 * long triggerTime = System.currentTime() + 1000;
 * long phaseDelay = 0;
 * long increasingDuration = 1000;
 * alpha = new Alpha(loopCount, triggerTime, phaseDelay, increasingDuration);
 *
 * // Create a new Alpha that starts two seconds after it is constructed,
 * // runs twice, changes from 0 to 1 (accelerating durring the 
 * // first 250 milliseconds,
 * // decelearting during the last 250 milliseconds) with a duration 
 * // of one second, then pauses
 * // at value 1 for 500 milliseconds before starting the next loop.
 * int loopCount = 2;
 * long triggerTime = System.currentTimeMillis() + 1000;
 * long phaseDelay = 1000;
 * long increasingDuration = 1000;
 * long increasingRampDuration = 250;
 * long alphaAtOneDuration = 500;
 * alpha = new Alpha(loopCount,
 *                    triggerTime,
 *                    phaseDelay,
 *                    increasingDuration,
 *                    increasingRampDuration,
 *                    alphaAtOneDuration);
 * </pre>
 * </code>
 * @see Animation
 * @author Jesse Grosjean
 */
public class Alpha {

	public static final int ALPHA_INCREASING = 1;
	public static final int ALPHA_DECREASING = 2;
	public static final int ALPHA_INCREASING_AND_DECREASING = 3;

	private int fLoopCount;
	private int fMode;
	private long fTriggerTime;
	private long fPhaseDelayDuration;
	private long fIncreasingAlphaDuration;
	private long fIncreasingAlphaRampDuration;
	private long fAlphaAtOneDuration;
	private long fDecreasingAlphaDuration;
	private long fDecreasingAlphaRampDuration;
	private long fAlphaAtZeroDuration;
	private long fStopTime;

	/**
	 * Return a new Alpha that changes from 0 to 1, starting at the
	 * time this method is called and lasting for the supplied duration
	 * parameter. The alpha will change linearly.
	 */
	public static Alpha createStandardAlpha(long aIncreasingAlphaDuration) {
		return new Alpha(1, aIncreasingAlphaDuration);
	}

	/**
	 * Return a new Alpha that changes from 0 to 1, starting at the
	 * time this method is called and lasting for the supplied duration
	 * parameter. The alpha will change with slow in slow out behavior.
	 */
	public static Alpha createStandardSlowInSlowOutAlpha
		(long aIncreasingAlphaDuration) 
	{
		return new Alpha(1,
											System.currentTimeMillis(),
											0,
											aIncreasingAlphaDuration,
											aIncreasingAlphaDuration / 2,
											0);
	}

	/**
	 * Construct a new Alpha using default parameters.
	 */
	public Alpha() {
		fLoopCount = -1;
		fMode = ALPHA_INCREASING;
		fTriggerTime = System.currentTimeMillis();
		fPhaseDelayDuration = 0;
		fIncreasingAlphaRampDuration = 0;
		fAlphaAtOneDuration = 0;
		fDecreasingAlphaDuration = 0;
		fDecreasingAlphaRampDuration = 0;
		fAlphaAtZeroDuration = 0;
		fIncreasingAlphaDuration = 0;
		updateStopTime();
	}

	/**
	 * Construct a new Alpha that will start immediately in
	 * ALPHA_INCREASING mode.
	 *
	 * @param aLoopCount The number of times this alpha should loop over
	 * its full range of values.
	 * @param aIncreasingAlphaDuration The amount of time that the alpha
	 * should spend in its increasing section.
	 */
	public Alpha(int aLoopCount, long aIncreasingAlphaDuration) {
		this();
		fLoopCount = aLoopCount;
		fIncreasingAlphaDuration = aIncreasingAlphaDuration;
		updateStopTime();
	}

	/**
	 * Construct a new Alpha in ALPHA_INCREASING mode whose alpha
	 * values change linearly.
	 *
	 * @param aLoopCount The number of times this alpha should loop over
	 * its full range of values.
	 * @param aTriggerTime The time that the alpha should start
	 * generating values.
	 * @param aPhaseDelayDuration The delay that the alpha should wait
	 * after trigger time before actually generating values.
	 * @param aIncreasingAlphaDuration The amount of time that the alpha
	 * should spend in its increasing section.
	 */
	public Alpha(int aLoopCount,
								long aTriggerTime,
								long aPhaseDelayDuration,
								long aIncreasingAlphaDuration) {

		this(aLoopCount, aIncreasingAlphaDuration);
		fTriggerTime = aTriggerTime;
		fPhaseDelayDuration = aPhaseDelayDuration;
		updateStopTime();
	}

	/**
	 * Construct a new Alpha with all parameter needed for
	 * ALPHA_INCREASING mode.
	 *
	 * @param aLoopCount The number of times this alpha should loop over
	 * its full range of values.
	 * @param aTriggerTime The time that the alpha should start
	 * generating values.
	 * @param aPhaseDelayDuration The delay that the alpha should wait
	 * after trigger time before actually generating values.
	 * @param aIncreasingAlphaDuration The amount of time that the alpha
	 * should spend in its increasing section.
	 * @param aIncreasingAlphaRampDuration The amount of time that the
	 * alpha should accelerate and then decelerate in its increasing
	 * section.
	 * @param aAlphaAtOneDuration The amount of time that the alpha
	 * should spend at 1 in its increasing section.
	 */
	public Alpha(int aLoopCount,
								long aTriggerTime,
								long aPhaseDelayDuration,
								long aIncreasingAlphaDuration,
								long aIncreasingAlphaRampDuration,
								long aAlphaAtOneDuration) {

		this(aLoopCount, aTriggerTime, 
				 aPhaseDelayDuration, aIncreasingAlphaDuration);
		fIncreasingAlphaRampDuration = aIncreasingAlphaRampDuration;
		fAlphaAtOneDuration = aAlphaAtOneDuration;
		updateStopTime();
	}

	/**
	 * Construct a new Alpha with all parameters.
	 *
	 * @param aLoopCount The number of times this alpha should loop over
	 * its full range of values.
	 * @param aMode The mode alpha should use to generate values,
	 * ALPHA_INCREASING, ALPHA_DECREASING,
	 * ALPHA_INCREASING_AND_DECREASING.
	 * @param aTriggerTime The time that the alpha should start
	 * generating values.
	 * @param aPhaseDelayDuration The delay that the alpha should wait
	 * after trigger time before actually generating values.
	 * @param aIncreasingAlphaDuration The amount of time that the alpha
	 * should spend in its increasing section.
	 * @param aIncreasingAlphaRampDuration The amount of time that the
	 * alpha should accelerate and then decelerate in its increasing
	 * section.
	 * @param aAlphaAtOneDuration The amount of time that the alpha
	 * should spend at 1 in its increasing section.
	 * @param aDecreasingAlphaDuration The amount of time that the alpha
	 * should spend in its decreasing section.
	 * @param aDecreasingAlphaRampDuration The amount of time that the
	 * alpha should accelerate and then decelerate in its decreasing
	 * section.
	 * @param aAlphaAtZeroDuration The amount of time that the alpha
	 * should spend at 0 in its decreasing section.
	 */
	public Alpha(int aLoopCount,
								int aMode,
								long aTriggerTime,
								long aPhaseDelayDuration,
								long aIncreasingAlphaDuration,
								long aIncreasingAlphaRampDuration,
								long aAlphaAtOneDuration,
								long aDecreasingAlphaDuration,
								long aDecreasingAlphaRampDuration,
								long aAlphaAtZeroDuration) {

		this(aLoopCount,
				 aTriggerTime,
				 aPhaseDelayDuration,
				 aIncreasingAlphaDuration,
				 aIncreasingAlphaRampDuration,
				 aAlphaAtOneDuration);

		fMode = aMode;
		fDecreasingAlphaDuration = aDecreasingAlphaDuration;
		fDecreasingAlphaRampDuration = aDecreasingAlphaRampDuration;
		fAlphaAtZeroDuration = aAlphaAtZeroDuration;
		updateStopTime();
	}

	/**
	 * Returns the number of times this alpha should run over its full
	 * range. A value of -1 means it should run indefinitely. The
	 * default loopCount value is -1.
	 */
	public int getLoopCount() {
		return fLoopCount;
	}

	/**
	 * Sets the number of times this alpha should run over its full
	 * range. A value of -1 means it should run indefinitely. The
	 * default loopCount value is -1.
	 */
	public void setLoopCount(int aLoopCount) {
		fLoopCount = aLoopCount;
		if (fLoopCount >= 0) {
			updateStopTime();
		}
	}

	/**
	 * Return the mode that this alpha is using to generate its
	 * values. Possible choices are
	 * <ul>
	 * <li>Alpha.ALPHA_INCREASING - alpha values will go from 0 to 1 over time.
	 * <li>Alpha.ALPHA_DECREASING - alpha values will go from 1 to 0 over time.
	 * <li>Alpha.ALPHA_INCREASING_AND_DECREASING - alpha values will go from 0 to 1 to 0 over time.
	 * </ul>
	 *
	 * <p>The default mode for alpha objects is ALPHA_INCREASING. By
	 * default alpha values will be generated from 0 to 1 for the
	 * increasingAlphaDuration and then 1 values will be generated for
	 * alphaAtOneDuration. Then the alpha will finish, or start another
	 * loop generating values.
	 */
	public int getMode() {
		return fMode;
	}

	/**
	 * Sets the mode that this alpha is using to generate its
	 * values. Possible choices are
	 * <ul>
	 * <li>Alpha.ALPHA_INCREASING - alpha values will go from 0 to 1 over time.
	 * <li>Alpha.ALPHA_DECREASING - alpha values will go from 1 to 0 over time.
	 * <li>Alpha.ALPHA_INCREASING_AND_DECREASING - alpha values will go from 0 to 1 to 0 over time.
	 * </ul>
	 *
	 * <p> The default mode for alpha objects is ALPHA_INCREASING. By
	 * default alpha values will be generated from 0 to 1 for the
	 * increasingAlphaDuration and then 1 values will be generated for
	 * alphaAtOneDuration. Then the alpha will finish, or start another
	 * loop generating values.
	 */
	public void setMode(int aMode) {
		fMode = aMode;
		updateStopTime();
	}

	/**
	 * Returns the trigger time for the alpha in milliseconds since
	 * midnight, January 1, 1970 UTC. This trigger time can be offset
	 * using <code>setPhaseDelayDuration</code>. The alpha will start
	 * running as soon as possible after
	 * <code>System.currentTimeMillis() >= triggerTime +
	 * phaseDelayDuration</code>.  
	 *
	 * <p> The default value for trigger
	 * time is the value of System.currentTimeMillis() when the alpha
	 * object is constructed. This means that normally as soon as an
	 * alpha is constructed it will also be started.
	 */
	public long getTriggerTime() {
		return fTriggerTime;
	}

	/**
	 * Sets the trigger time for the alpha in milliseconds since
	 * midnight, January 1, 1970 UTC. This trigger time can be offset
	 * using <code>setPhaseDelayDuration</code>. The alpha will start
	 * running as soon as possible after
	 * <code>System.currentTimeMillis() >= triggerTime +
	 * phaseDelayDuration</code>.  
	 *
	 * <p> The default value for trigger
	 * time is the value of System.currentTimeMillis() when the alpha
	 * object is constructed. This means that normally as soon as an
	 * alpha is constructed it will also be started.
	 */
	public void setTriggerTime(long aTriggerTime) {
		fTriggerTime = aTriggerTime;
		updateStopTime();
	}

	/**
	 * Return the number of milliseconds after trigger time to wait
	 * before starting this alpha. The default value is 0, the alpha
	 * will start at triggerTime.
	 */
	public long getPhaseDelayDuration() {
		return fPhaseDelayDuration;
	}

	/**
	 * Set the number of milliseconds after trigger time to wait before
	 * starting this alpha. The default value is 0, the alpha will start
	 * at triggerTime.
	 */
	public void setPhaseDelayDuration(long aPhaseDelayDuration) {
		fPhaseDelayDuration = aPhaseDelayDuration;
		updateStopTime();
	}

	/**
	 * Return the number of milliseconds that the alpha should take in
	 * its increasing section where it moves from value 0 to 1.
	 */
	public long getIncreasingAlphaDuration() {
		return fIncreasingAlphaDuration;
	}

	/**
	 * Set the number of milliseconds that the alpha should take in its
	 * increasing section where it moves from value 0 to 1.
	 */
	public void setIncreasingAlphaDuration(long aIncreasingAlphaDuration) {
		fIncreasingAlphaDuration = aIncreasingAlphaDuration;
		updateStopTime();
	}

	/**
	 * Return the number of milliseconds that alpha accelerates at the
	 * beginning of its increasingAlphaDuration and decelerates at the
	 * end of its increasingAlphaDuration. This value is clamped to half
	 * the increasingAlphaDuration. If the value is zero alpha will
	 * change linearly, at a constant velocity.  
	 *
	 * <p> The default value is 0, increasing alpha changes linearly.
	 */
	public long getIncreasingAlphaRampDuration() {
		return fIncreasingAlphaRampDuration;
	}

	/**
	 * Set the number of milliseconds that alpha accelerates at the
	 * beginning of its increasingAlphaDuration and decelerates at the
	 * end of its increasingAlphaDuration. This value is clamped to half
	 * the increasingAlphaDuration. If the value is zero alpha will
	 * change linearly, at a constant velocity.  
	 *
	 * <p> The default value is 0, increasing alpha changes linearly.
	 */
	public void setIncreasingAlphaRampDuration
		(long aIncreasingAlphaRampDuration) 
	{
		fIncreasingAlphaRampDuration = aIncreasingAlphaRampDuration;

		// Clamp ramp duration to half total duration.
		long halfIncreasingAlphaDuration = fIncreasingAlphaDuration / 2;
		if (fIncreasingAlphaRampDuration > halfIncreasingAlphaDuration) {
			fIncreasingAlphaRampDuration = halfIncreasingAlphaDuration;
		}
	}

	/**
	 * Return the number of milliseconds that alpha will remain at value
	 * 1 after the increasingAlphaDuration has finished, but before the
	 * next section starts.  
	 *
	 * <p> The default value is 0, increasing alpha does not pause at 1.
	 */
	public long getAlphaAtOneDuration() {
		return fAlphaAtOneDuration;
	}

	/**
	 * Set the number of milliseconds that alpha will remain at value 1
	 * after the increasingAlphaDuration has finished, but before the
	 * next section starts.  
	 *
	 * <p> The default value is 0, increasing alpha does not pause at 1.
	 */
	public void setAlphaAtOneDuration(long aAlphaAtOneDuration) {
		fAlphaAtOneDuration = aAlphaAtOneDuration;
		updateStopTime();
	}

	/**
	 * Return the number of milliseconds that the alpha should take in
	 * its decreasing section where it moves from value 1 to 0.
	 */
	public long getDecreasingAlphaDuration() {
		return fDecreasingAlphaDuration;
	}

	/**
	 * Set the number of milliseconds that the alpha should take in its
	 * decreasing section where it moves from value 1 to 0.
	 */
	public void setDecreasingAlphaDuration(long aDecreasingAlphaDuration) {
		fDecreasingAlphaDuration = aDecreasingAlphaDuration;
		updateStopTime();
	}

	/**
	 * Return the number of milliseconds that alpha accelerates at the
	 * beginning of its decreasingAlphaDuration and decelerates at the
	 * end of its decreasingAlphaDuration. This value is clamped to half
	 * the decreasingAlphaDuration. If the value is zero alpha will
	 * change linearly, at a constant velocity.  
	 *
	 * <p> The default value is 0, decreasing alpha changes linearly.
	 */
	public long getDecreasingAlphaRampDuration() {
		return fDecreasingAlphaRampDuration;
	}

	/**
	 * Set the number of milliseconds that alpha accelerates at the
	 * beginning of its decreasingAlphaDuration and decelerates at the
	 * end of its decreasingAlphaDuration. This value is clamped to half
	 * the decreasingAlphaDuration. If the value is zero alpha will
	 * change linearly, at a constant velocity.  
	 *
	 * <p> The default value is 0, decreasing alpha changes linearly.
	 */
	public void setDecreasingAlphaRampDuration
		(long aDecreasingAlphaRampDuration) 
	{
		fDecreasingAlphaRampDuration = aDecreasingAlphaRampDuration;

		// Clamp ramp duration to half total duration.
		long halfDecreasingAlphaDuration = fDecreasingAlphaDuration / 2;
		if (fDecreasingAlphaRampDuration > halfDecreasingAlphaDuration) {
			fDecreasingAlphaRampDuration = halfDecreasingAlphaDuration;
		}
	}

	/**
	 * Return the number of milliseconds that alpha will remain at value
	 * 0 after the decreasingAlphaDuration has finished, but before the
	 * next section starts.  
	 *
	 * <p> The default value is 0, decreasing alpha does not pause at 0.
	 */
	public long getAlphaAtZeroDuration() {
		return fAlphaAtZeroDuration;
	}

	/**
	 * Set the number of milliseconds that alpha will remain at value 0
	 * after the decreasingAlphaDuration has finished, but before the
	 * next section starts.  
	 *
	 * <p> The default value is 0, decreasing alpha does not pause at 0.
	 */
	public void setAlphaAtZeroDuration(long aAlphaAtZeroDuration) {
		fAlphaAtZeroDuration = aAlphaAtZeroDuration;
		updateStopTime();
	}

	/**
	 * Return true if alpha has started for the given time in
	 * milliseconds. This is determined by testing if <code>aTime >=
	 * triggerTime + phaseDelayDuration</code>. If asked for an alpha
	 * value before it has been started it will return the first alpha
	 * value.
	 */
	public boolean isStarted(long aTime) {
		if (aTime >= (fTriggerTime + fPhaseDelayDuration)) {
			return true;
		}
		return false;
	}

	/**
	 * Return true if alpha has finished for the given time in
	 * milliseconds. This will always return false if the
	 * <code>loopCount == -1</code>, alpha is running indefinitaly.  If
	 * asked for an alpha value after it has finished it will return the
	 * last alpha value.
	 */
	public boolean isFinished(long aTime) {
		if (getLoopCount() == -1) {
			return false;
		}

		return aTime > getStopTime();
	}

	/**
	 * Returns the stop time for the alpha in milliseconds since
	 * midnight, January 1, 1970 UTC.
	 */
	public long getStopTime() {
		return fStopTime;
	}

	/**
	 * Update the time in milliseconds that this alpha should stop
	 * running.
	 */
	private void updateStopTime() {
		fStopTime = fTriggerTime + fPhaseDelayDuration;

		switch (fMode) {
		case ALPHA_INCREASING:
			fStopTime += fLoopCount * (fIncreasingAlphaDuration +
																 fAlphaAtOneDuration);
			break;
		case ALPHA_DECREASING:
			fStopTime += fLoopCount * (fDecreasingAlphaDuration +
																 fAlphaAtZeroDuration);
			break;
		case ALPHA_INCREASING_AND_DECREASING:
			fStopTime += fLoopCount * (fIncreasingAlphaDuration +
																 fAlphaAtOneDuration +
																 fDecreasingAlphaDuration +
																 fAlphaAtZeroDuration);
			break;
		}
	}

	/**
	 * Linearly interpolate between the currentTime and totalTime.
	 */
	protected float lerp(long currentTime, long totalTime) {
		return (float) currentTime / (float) totalTime;
	}

	/**
	 * Slow in slow out interpolate between the currentTime and
	 * totalTime.
	 */
	protected float sisoLerp(long aTotalTime, 
													 long aCurrentTime, 
													 long aRampTime) 
	{
		float result = 0;

		float currentTime = lerp(aCurrentTime, aTotalTime);
		float accelerationTime = lerp(aRampTime, aTotalTime);

		// Find out the acceleration required so that we end up in the right place
		// when we are finished.
		float acceleration = 
			1.0f / (accelerationTime - (accelerationTime * accelerationTime));

		// Accelerating
		if(currentTime < accelerationTime) {
			result = (acceleration * currentTime * currentTime) / 2;

			// Constant velocity
		} else if(currentTime < 1 - accelerationTime) {
			float valueForAccelerationPhase = 
				(acceleration * accelerationTime * accelerationTime) / 2;
			float velocityAtEndOfAccelerationPhase = acceleration * accelerationTime;
			float timeAtConstantVelocity = currentTime - accelerationTime;
			result = valueForAccelerationPhase + 
				(velocityAtEndOfAccelerationPhase * timeAtConstantVelocity);

			// Decelerating.
		} else {
			float valueForAccelerationPhase = 
				(acceleration * accelerationTime * accelerationTime) / 2;
			float velocityAtEndOfAccelerationPhase = acceleration * accelerationTime;
			float timeAtConstantVelocity = currentTime - accelerationTime;
			float valueForConstantVelocityPhase = 
				valueForAccelerationPhase + 
				(velocityAtEndOfAccelerationPhase * timeAtConstantVelocity);
			float timeInDeceleration = (currentTime - (1 - accelerationTime));
			result = valueForConstantVelocityPhase + 
				(-acceleration * timeInDeceleration * timeInDeceleration) / 2;
		}

		return result;
	}

	/**
	 * Return the alpha value, from 0 to 1, based on the time to alpha
	 * mapping parameters set for this interpolator. If the time to
	 * alpha mapping has not yet started then the starting alpha value
	 * is returned. If it has finished then the ending alpha value is
	 * returned.
	 */
	public float value(long aCurrentTime) {
		aCurrentTime -= fTriggerTime;

		long aWaveDelay = fPhaseDelayDuration;
		long aWaveLength = getWaveLength();
		long aWaveRelativeTime = (aCurrentTime - aWaveDelay) % aWaveLength;

		if(aCurrentTime < aWaveDelay)
			aWaveRelativeTime = Long.MIN_VALUE;

		if(fLoopCount != -1 && (fTriggerTime + aCurrentTime) >= fStopTime)
			aWaveRelativeTime = Long.MAX_VALUE;

		// At this point we know where we are (aWaveRelativeTime) on 
		// the wave 0 - aWaveLength.
		switch (fMode) {
		case ALPHA_INCREASING:
			return valueAlphaIncreasing(aWaveRelativeTime);

		case ALPHA_DECREASING:
			return valueAlphaDecreasing(aWaveRelativeTime);

		case ALPHA_INCREASING_AND_DECREASING:
			return valueAlphaIncreasingAndDecreasing(aWaveRelativeTime);

		default:
			return 0.0f;
		}
	}

	/**
	 * Return the alpha value for the given wave relative time
	 * (normalized to the start of the wave) using the mode
	 * ALPHA_INCREASING.
	 */
	protected float valueAlphaIncreasing(long aWaveRelativeTime) {
		if (aWaveRelativeTime <= 0)
			return 0;

		if (aWaveRelativeTime >= fIncreasingAlphaDuration)
			return 1;

		if (fIncreasingAlphaRampDuration == 0) {
			return lerp(aWaveRelativeTime, fIncreasingAlphaDuration);
		} else {
			return sisoLerp(fIncreasingAlphaDuration,
											aWaveRelativeTime,
											fIncreasingAlphaRampDuration);
		}
	}

	/**
	 * Return the alpha value for the given wave relative time
	 * (normalized to the start of the wave) using the mode
	 * ALPHA_DECREASING.
	 */
	protected float valueAlphaDecreasing(long aWaveRelativeTime) {
		if (aWaveRelativeTime <= 0)
			return 1;

		if (aWaveRelativeTime >= fDecreasingAlphaDuration)
			return 0;

		if (fDecreasingAlphaRampDuration == 0) {
			return 1 - lerp(aWaveRelativeTime, fDecreasingAlphaDuration);
		} else {
			return 1 - sisoLerp(fDecreasingAlphaDuration,
													aWaveRelativeTime,
													fDecreasingAlphaRampDuration);
		}
	}

	/**
	 * Return the alpha value for the given wave relative time
	 * (normalized to the start of the wave) using the mode
	 * ALPHA_INCREASING_AND_DECREASING.
	 */
	protected float valueAlphaIncreasingAndDecreasing(long aWaveRelativeTime) {
		long aIncreasingWaveLength = 
			fIncreasingAlphaDuration + fAlphaAtOneDuration;
		long aDecreasingWaveLength = 
			fDecreasingAlphaDuration + fAlphaAtZeroDuration;

		if (aWaveRelativeTime <= 0) {
			return 0;
		}

		if (aWaveRelativeTime >= (aIncreasingWaveLength + aDecreasingWaveLength)) {
			return 0;
		}

		if (aWaveRelativeTime <= aIncreasingWaveLength) {
			return valueAlphaIncreasing(aWaveRelativeTime);
		} else {
			return valueAlphaDecreasing(aWaveRelativeTime - aIncreasingWaveLength);
		}
	}

	/**
	 * Return the amount of time in milliseconds that on iteration over
	 * the values of this alpha takes.
	 */
	protected long getWaveLength() {
		switch (fMode) {
		case ALPHA_INCREASING:
			return fIncreasingAlphaDuration + fAlphaAtOneDuration;

		case ALPHA_DECREASING:
			return fDecreasingAlphaDuration + fAlphaAtZeroDuration;

		case ALPHA_INCREASING_AND_DECREASING:
			return fIncreasingAlphaDuration + fAlphaAtOneDuration +
				fDecreasingAlphaDuration + fAlphaAtZeroDuration;
		}
		return 0;
	}
}
