/*
 * Copyright (C) 2013-2019 by XDEV Software, All Rights Reserved.
 *
 * This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License 2.0
 * which accompanies this distribution, and is available at
 * https://www.eclipse.org/legal/epl-2.0/
 *
 * SPDX-License-Identifier: EPL-2.0
 *
 * Contributors:
 *     XDEV Software Corp. - initial API and implementation
 */

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
