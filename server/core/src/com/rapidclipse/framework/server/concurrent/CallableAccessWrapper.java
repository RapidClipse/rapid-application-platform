/*
 * Copyright (C) 2013-2018 by XDEV Software, All Rights Reserved.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 *
 * For further information see
 * <http://www.rapidclipse.com/en/legal/license/license.html>.
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
