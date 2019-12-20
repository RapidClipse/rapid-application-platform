
package com.rapidclipse.framework.server.webapi.geolocation;

import java.io.Serializable;


/**
 *
 *
 * @author XDEV Software
 * @since 10.02.00
 */
public class PositionError implements Serializable
{
	/**
	 * From MDN:<br>
	 * <b>1 PERMISSION_DENIED</b> The acquisition of the geolocation information failed because the page didn't have the
	 * permission to do it.<br>
	 * <b>2 POSITION_UNAVAILABLE</b> The acquisition of the geolocation failed because at least one internal source of
	 * position
	 * returned an internal error.<br>
	 * <b>3 TIMEOUT</b> The time allowed to acquire the geolocation, defined by PositionOptions.timeout information was
	 * reached
	 * before the information was obtained.
	 */
	private short  code;
	private String message;

	public PositionError()
	{
		super();
	}

	public short getCode()
	{
		return this.code;
	}

	public PositionError setCode(final short code)
	{
		this.code = code;
		return this;
	}

	public String getMessage()
	{
		return this.message;
	}

	public PositionError setMessage(final String message)
	{
		this.message = message;
		return this;
	}
}
