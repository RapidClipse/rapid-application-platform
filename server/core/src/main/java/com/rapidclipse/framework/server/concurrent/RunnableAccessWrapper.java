/*-
 * ---
 * Rapid Application Platform / Server / Core
 * --
 * Copyright (C) 2013 - 2019 XDEV Software Corp.
 * --
 * This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License 2.0
 * which accompanies this distribution, and is available at
 * https://www.eclipse.org/legal/epl-2.0/
 * 
 * SPDX-License-Identifier: EPL-2.0
 * 
 * Contributors:
 *     XDEV Software Corp. - initial API and implementation
 * ---
 */

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
