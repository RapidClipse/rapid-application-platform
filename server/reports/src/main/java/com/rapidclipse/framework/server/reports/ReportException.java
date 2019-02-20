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
