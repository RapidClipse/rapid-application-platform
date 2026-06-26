/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
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

			final Iterable<Participant> participants = ServiceLoader.forType(Participant.class)
				.servicesUncached();

			try
			{
				participants.forEach(Participant::before);

				runnable.run();
			}
			finally
			{
				participants.forEach(Participant::after);
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

			final Iterable<Participant> participants = ServiceLoader.forType(Participant.class)
				.servicesUncached();

			try
			{
				participants.forEach(Participant::before);

				return callable.call();
			}
			finally
			{
				participants.forEach(Participant::after);
			}
		};
	}
}
