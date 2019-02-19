
package com.rapidclipse.framework.server.mobilekit.geolocation;

/**
 * A Coordinates object is attached to a {@link Position} object that is
 * available to callback functions in requests for the current position. It
 * contains a set of properties that describe the geographic coordinates of a
 * position.
 *
 * @author XDEV Software
 *
 */
public interface Coordinates
{
	/**
	 * Latitude in decimal degrees.
	 */
	public double getLatitude();
	
	/**
	 * Longitude in decimal degrees.
	 */
	public double getLongitude();
	
	/**
	 * Height of the position in meters above the ellipsoid.
	 */
	public double getAltitude();
	
	/**
	 * Accuracy level of the latitude and longitude coordinates in meters.
	 */
	public double getAccuracy();
	
	/**
	 * Accuracy level of the altitude coordinate in meters.
	 */
	public double getAltitudeAccuracy();
	
	/**
	 * Direction of travel, specified in degrees counting clockwise relative to
	 * the true north.
	 */
	public double getHeading();
	
	/**
	 * Current ground speed of the device, specified in meters per second.
	 */
	public double getSpeed();
}
