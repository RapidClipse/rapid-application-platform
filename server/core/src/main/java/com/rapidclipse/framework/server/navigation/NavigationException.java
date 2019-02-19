
package com.rapidclipse.framework.server.navigation;

/**
 * @author XDEV Software
 *
 */
public class NavigationException extends RuntimeException
{
	/**
	 *
	 */
	public NavigationException()
	{
		super();
	}
	
	/**
	 * @param message
	 * @param cause
	 * @param enableSuppression
	 * @param writableStackTrace
	 */
	public NavigationException(
		final String message,
		final Throwable cause,
		final boolean enableSuppression,
		final boolean writableStackTrace)
	{
		super(message, cause, enableSuppression, writableStackTrace);
	}
	
	/**
	 * @param message
	 * @param cause
	 */
	public NavigationException(final String message, final Throwable cause)
	{
		super(message, cause);
	}
	
	/**
	 * @param message
	 */
	public NavigationException(final String message)
	{
		super(message);
	}
	
	/**
	 * @param cause
	 */
	public NavigationException(final Throwable cause)
	{
		super(cause);
	}
}
