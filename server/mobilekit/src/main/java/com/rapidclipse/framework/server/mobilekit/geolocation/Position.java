
package com.rapidclipse.framework.server.mobilekit.geolocation;

/**
 * Contains position coordinates and timestamp created by the
 * {@link GeolocationService}.
 *
 * @author XDEV Software
 *
 */
public interface Position
{
	/**
	 * A set of geographic coordinates.
	 */
	public Coordinates getCoordinates();
	
	/**
	 * Creation timestamp.
	 */
	public long getTimestamp();
}
