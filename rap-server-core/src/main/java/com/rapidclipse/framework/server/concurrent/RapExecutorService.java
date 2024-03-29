/*
 * Copyright (C) 2013-2023 by XDEV Software, All Rights Reserved.
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
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *
 * Contributors:
 *     XDEV Software - initial API and implementation
 */
package com.rapidclipse.framework.server.concurrent;

import java.util.concurrent.Callable;
import java.util.concurrent.Executor;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.RejectedExecutionException;
import java.util.concurrent.ThreadFactory;

import jakarta.servlet.ServletContext;

import com.rapidclipse.framework.server.Rap;


/**
 * An {@link Executor} that provides methods to manage termination and methods
 * that can produce a {@link Future} for tracking progress of one or more
 * asynchronous tasks.
 *
 * <p>
 * Method {@code submit} extends base method {@link Executor#execute(Runnable)}
 * by creating and returning a {@link Future} that can be used to cancel
 * execution and/or wait for completion. Methods {@code invokeAny} and
 * {@code invokeAll} perform the most commonly useful forms of bulk execution,
 * executing a collection of tasks and then waiting for at least one, or all, to
 * complete.
 *
 * <p>
 * The instantiation and the shutdown process is handled by the framework. To
 * use the executor just call one of the static get methods.
 *
 * @see Rap#getExecutorService()
 *
 * @author XDEV Software
 */
public interface RapExecutorService extends Executor
{
	public static interface Factory
	{
		public RapExecutorService createExecutorService(final ServletContext context);
	}

	public static final String FACTORY_INIT_PARAMETER           = "rap.executorService.factory";
	public static final String THREAD_COUNT_INIT_PARAMETER      = "rap.executorService.threadCount";
	public static final String GRACEFUL_SHUTDOWN_INIT_PARAMETER = "rap.executorService.gracefulShutdown";

	/**
	 * Submits a Runnable task for execution and returns a Future representing
	 * that task. The Future's {@code get} method will return {@code null} upon
	 * <em>successful</em> completion.
	 *
	 * @param task
	 *            the task to submit
	 * @return a Future representing pending completion of the task
	 * @throws RejectedExecutionException
	 *             if the task cannot be scheduled for execution
	 * @throws NullPointerException
	 *             if the task is null
	 */
	public Future<?> submit(Runnable task);

	/**
	 * Submits a Runnable task for execution and returns a Future representing
	 * that task. The Future's {@code get} method will return the given result
	 * upon successful completion.
	 *
	 * @param task
	 *            the task to submit
	 * @param result
	 *            the result to return
	 * @param <T>
	 *            the type of the result
	 * @return a Future representing pending completion of the task
	 * @throws RejectedExecutionException
	 *             if the task cannot be scheduled for execution
	 * @throws NullPointerException
	 *             if the task is null
	 */
	public <T> Future<T> submit(Runnable task, T result);

	/**
	 * Submits a value-returning task for execution and returns a Future
	 * representing the pending results of the task. The Future's {@code get}
	 * method will return the task's result upon successful completion.
	 *
	 * <p>
	 * If you would like to immediately block waiting for a task, you can use
	 * constructions of the form {@code result = exec.submit(aCallable).get();}
	 *
	 * @param task
	 *            the task to submit
	 * @param <T>
	 *            the type of the task's result
	 * @return a Future representing pending completion of the task
	 * @throws RejectedExecutionException
	 *             if the task cannot be scheduled for execution
	 * @throws NullPointerException
	 *             if the task is null
	 */
	public <T> Future<T> submit(Callable<T> task);

	/**
	 * Initiates a shutdown of the executor service. This is done automatically
	 * when the corresponding servlet context is destroyed and therefor
	 * shouldn't be called by the user.
	 */
	public void shutdown();

	public static RapExecutorService New(final ServletContext context)
	{
		return new Default(context);
	}

	/**
	 * Default implementation of the {@link RapExecutorService} contract.
	 *
	 */
	public static class Default implements RapExecutorService
	{
		private ExecutorService executorService;
		private boolean         gracefulShutdown;

		protected Default(final ServletContext context)
		{
			this.gracefulShutdown = Boolean
				.valueOf(context.getInitParameter(GRACEFUL_SHUTDOWN_INIT_PARAMETER));

			int threadCount = 10;
			try
			{
				threadCount = Integer
					.parseInt(context.getInitParameter(THREAD_COUNT_INIT_PARAMETER));
			}
			catch(final NumberFormatException localNumberFormatException)
			{
			}

			final ThreadFactory defaultThreadFactory = Executors.defaultThreadFactory();
			final ThreadFactory daemonThreadFactory  = runnable -> {
															final Thread t = defaultThreadFactory.newThread(runnable);
															t.setDaemon(true);
															return t;
														};

			if(threadCount <= 1)
			{
				this.executorService = Executors.newSingleThreadExecutor(daemonThreadFactory);
			}
			else
			{
				this.executorService = Executors.newFixedThreadPool(threadCount,
					daemonThreadFactory);
			}
		}

		@Override
		public void execute(final Runnable command)
		{
			this.executorService.execute(ExecutionWrapper.Wrap(command));
		}

		@Override
		public Future<?> submit(final Runnable task)
		{
			return this.executorService.submit(ExecutionWrapper.Wrap(task));
		}

		@Override
		public <T> Future<T> submit(final Runnable task, final T result)
		{
			return this.executorService.submit(ExecutionWrapper.Wrap(task), result);
		}

		@Override
		public <T> Future<T> submit(final Callable<T> task)
		{
			return this.executorService.submit(ExecutionWrapper.Wrap(task));
		}

		@Override
		public void shutdown()
		{
			if(this.gracefulShutdown)
			{
				this.executorService.shutdown();
			}
			else
			{
				this.executorService.shutdownNow();
			}
		}
	}
}
