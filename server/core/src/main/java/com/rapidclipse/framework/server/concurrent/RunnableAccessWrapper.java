
package com.rapidclipse.framework.server.concurrent;

/**
 * Runnable wrapper for threads outside vaadin's session scope, e.g. web
 * services.
 *
 * @author XDEV Software
 */
public class RunnableAccessWrapper extends AccessWrapper implements Runnable
{
	public static void execute(final Runnable runnable)
	{
		new RunnableAccessWrapper(runnable).run();
	}

	private final Runnable delegate;
	
	public RunnableAccessWrapper(final Runnable delegate)
	{
		this.delegate = delegate;
	}
	
	@Override
	public void run()
	{
		before();

		try
		{
			this.delegate.run();
		}
		finally
		{
			after();
		}
	}
}
