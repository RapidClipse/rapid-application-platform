
package com.rapidclipse.framework.server.reports;

/**
 * @author XDEV Software
 *
 */
public class ReportException extends RuntimeException
{
	public ReportException()
	{
		super();
	}
	
	public ReportException(
		final String message,
		final Throwable cause,
		final boolean enableSuppression,
		final boolean writableStackTrace)
	{
		super(message, cause, enableSuppression, writableStackTrace);
	}
	
	public ReportException(final String message, final Throwable cause)
	{
		super(message, cause);
	}
	
	public ReportException(final String message)
	{
		super(message);
	}
	
	public ReportException(final Throwable cause)
	{
		super(cause);
	}
}
