
package com.rapidclipse.framework.server.mobilekit.geolocation;

import java.io.Serializable;


/**
 * Optional parameters to customize the retrieval of the geolocation.
 *
 * @author XDEV Software
 *
 */
public class GeolocationOptions implements Serializable
{
	public static GeolocationOptions withHighAccuracy()
	{
		return new GeolocationOptions().enableHighAccuracy(true);
	}
	
	public static GeolocationOptions withLowAccuracy()
	{
		return new GeolocationOptions().enableHighAccuracy(false);
	}
	
	private Boolean enableHighAccuracy;
	private Integer timeout;
	private Integer maximumAge;
	
	public GeolocationOptions()
	{
	}
	
	/**
	 * @return the enable high accuracy setting
	 * @see #enableHighAccuracy(boolean)
	 */
	public Boolean getEnableHighAccuracy()
	{
		return this.enableHighAccuracy;
	}
	
	/**
	 * Provides a hint that the application needs the best possible results. By
	 * default, the device attempts to retrieve a {@link Position} using
	 * network-based methods. Setting this property to <code>true</code> tells
	 * the framework to use more accurate methods, such as satellite
	 * positioning.
	 */
	public GeolocationOptions enableHighAccuracy(final boolean enableHighAccuracy)
	{
		this.enableHighAccuracy = enableHighAccuracy;
		return this;
	}
	
	/**
	 * @return the timeout setting
	 * @see #timeout(long)
	 */
	public Integer getTimeout()
	{
		return this.timeout;
	}
	
	/**
	 * The maximum length of time (milliseconds) that is allowed to pass from
	 * the call to
	 * {@link GeolocationService#getCurrentPosition(java.util.function.Consumer, java.util.function.Consumer)}
	 * or
	 * {@link GeolocationService#watchPosition(java.util.function.Consumer, java.util.function.Consumer, int)}
	 * until the corresponding success callback executes. If the success
	 * callback is not invoked within this time, the geolocationError callback
	 * is passed a {@link GeolocationServiceError.Reason#TIMEOUT} error code.
	 * (Note that when used in conjunction with
	 * {@link GeolocationService#watchPosition(java.util.function.Consumer, java.util.function.Consumer, int)},
	 * the error callback could be called on an interval every timeout
	 * milliseconds!)
	 */
	public GeolocationOptions timeout(final int timeout)
	{
		this.timeout = timeout;
		return this;
	}
	
	/**
	 * @return the maximum age setting
	 * @see #maximumAge(long)
	 */
	public Integer getMaximumAge()
	{
		return this.maximumAge;
	}
	
	/**
	 * Accept a cached position whose age is no greater than the specified time
	 * in milliseconds.
	 */
	public GeolocationOptions maximumAge(final int maximumAge)
	{
		this.maximumAge = maximumAge;
		return this;
	}
}
