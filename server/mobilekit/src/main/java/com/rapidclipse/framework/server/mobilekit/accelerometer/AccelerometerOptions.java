
package com.rapidclipse.framework.server.mobilekit.accelerometer;

/**
 * Parameters to customize the retrieval of the acceleration.
 *
 * @author XDEV Software
 *
 */
public class AccelerometerOptions
{
	public static AccelerometerOptions withFrequency(final int milliseconds)
	{
		return new AccelerometerOptions().frequency(milliseconds);
	}
	
	private int frequency = 1000;
	
	public AccelerometerOptions()
	{
	}
	
	/**
	 * The frequency of updates in milliseconds.
	 */
	public int getFrequency()
	{
		return this.frequency;
	}
	
	/**
	 * The frequency of updates in milliseconds.
	 */
	public AccelerometerOptions frequency(final int frequency)
	{
		this.frequency = frequency;
		return this;
	}
}
