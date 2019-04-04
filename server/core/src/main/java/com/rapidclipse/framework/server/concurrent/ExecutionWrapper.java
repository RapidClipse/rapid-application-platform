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

import com.rapidclipse.framework.server.util.ServiceLoader;


/**
 * Wrapper for threads outside vaadin's session scope, e.g. web
 * services.
 *
 * @author XDEV Software
 */
public interface ExecutionWrapper
{
	public static interface Participant
	{
		public void before();

		public void after();
	}

	public static interface RunnableWrapper extends Runnable, ExecutionWrapper
	{
	}

	public static interface CallableWrapper<V> extends Callable<V>, ExecutionWrapper
	{
	}

	public static RunnableWrapper Wrap(final Runnable runnable)
	{
		if(runnable instanceof RunnableWrapper)
		{
			return (RunnableWrapper)runnable;
		}

		return () -> {

			final ServiceLoader<Participant> serviceLoader = ServiceLoader.forType(Participant.class);
			serviceLoader.services().forEach(Participant::before);

			try
			{
				runnable.run();
			}
			finally
			{
				serviceLoader.services().forEach(Participant::after);
			}
		};
	}
	
	public static <V> CallableWrapper<V> Wrap(final Callable<V> callable)
	{
		if(callable instanceof CallableWrapper)
		{
			return (CallableWrapper<V>)callable;
		}
		
		return () -> {

			final ServiceLoader<Participant> serviceLoader = ServiceLoader.forType(Participant.class);
			serviceLoader.services().forEach(Participant::before);

			try
			{
				return callable.call();
			}
			finally
			{
				serviceLoader.services().forEach(Participant::after);
			}
		};
	}
}
