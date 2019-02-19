
package com.rapidclipse.framework.server.concurrent;

import java.util.concurrent.Callable;


/**
 * Callable wrapper for threads outside vaadin's session scope, e.g. web
 * services.
 *
 * @author XDEV Software
 */
public class CallableAccessWrapper<V> extends AccessWrapper implements Callable<V>
{
	public static <T> T execute(final Callable<T> callable) throws Exception
	{
		return new CallableAccessWrapper<>(callable).call();
	}
	
	private final Callable<V> callable;
	
	public CallableAccessWrapper(final Callable<V> callable)
	{
		this.callable = callable;
	}
	
	@Override
	public V call() throws Exception
	{
		before();
		
		try
		{
			return this.callable.call();
		}
		finally
		{
			after();
		}
	}
}
