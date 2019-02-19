
package com.rapidclipse.framework.server.mobilekit.accelerometer;

/**
 * Contains Accelerometer data captured at a specific point in time.
 * Acceleration values include the effect of gravity (9.81 m/s^2), so that when
 * a device lies flat and facing up, x, y, and z values returned should be 0, 0,
 * and 9.81.
 *
 * @author XDEV Software
 *
 */
public interface Acceleration
{
	/**
	 * Amount of acceleration on the x-axis. (in m/s^2)
	 */
	public double getX();
	
	/**
	 * Amount of acceleration on the y-axis. (in m/s^2)
	 */
	public double getY();
	
	/**
	 * Amount of acceleration on the z-axis. (in m/s^2)
	 */
	public double getZ();
	
	/**
	 * Creation timestamp in milliseconds.
	 */
	public long getTimestamp();
}
