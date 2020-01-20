/*
 * Copyright (C) 2013-2020 by XDEV Software, All Rights Reserved.
 *
 * This file is part of the RapidClipse Application Platform (RAP).
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
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
