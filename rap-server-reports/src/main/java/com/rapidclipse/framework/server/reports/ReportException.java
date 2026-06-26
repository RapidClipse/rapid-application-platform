/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
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
