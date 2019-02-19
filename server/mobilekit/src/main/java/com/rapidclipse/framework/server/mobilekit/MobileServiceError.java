
package com.rapidclipse.framework.server.mobilekit;

/**
 * @author XDEV Software
 *
 */
public class MobileServiceError
{
	private final MobileService source;
	private final String        message;
	
	public MobileServiceError(final MobileService source, final String message)
	{
		this.source  = source;
		this.message = message;
	}
	
	public MobileService getSource()
	{
		return this.source;
	}
	
	public String getMessage()
	{
		return this.message;
	}
	
	@Override
	public String toString()
	{
		return getClass().getName() + " [source=" + this.source + ", message=" + this.message + "]";
	}
}
