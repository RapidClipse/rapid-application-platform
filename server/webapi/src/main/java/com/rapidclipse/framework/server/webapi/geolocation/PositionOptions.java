
package com.rapidclipse.framework.server.webapi.geolocation;

import java.io.Serializable;


/**
 *
 *
 * @author XDEV Software
 * @since 10.02.00
 */
public class PositionOptions implements Serializable
{
	private Boolean enableHighAccuracy;
	private Integer timeout;
	private Integer maximumAge;
	
	public static PositionOptions Default()
	{
		return new PositionOptions(true, 10_000, 1_000);
	}
	
	public PositionOptions(final Boolean enableHighAccuracy, final Integer timeout, final Integer maximumAge)
	{
		this.enableHighAccuracy = enableHighAccuracy;
		this.timeout            = timeout;
		this.maximumAge         = maximumAge;
	}
	
	public PositionOptions()
	{
		super();
	}
	
	public Boolean getEnableHighAccuracy()
	{
		return this.enableHighAccuracy;
	}
	
	public PositionOptions setEnableHighAccuracy(final Boolean enableHighAccuracy)
	{
		this.enableHighAccuracy = enableHighAccuracy;
		return this;
	}
	
	public Integer getTimeout()
	{
		return this.timeout;
	}
	
	public PositionOptions setTimeout(final Integer timeout)
	{
		this.timeout = timeout;
		return this;
	}
	
	public Integer getMaximumAge()
	{
		return this.maximumAge;
	}
	
	public PositionOptions setMaximumAge(final Integer maximumAge)
	{
		this.maximumAge = maximumAge;
		return this;
	}
}
