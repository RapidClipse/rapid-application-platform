/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
 */
package com.rapidclipse.framework.server.util;

/**
 * @author XDEV Software
 *
 */
public final class StacktraceUtils
{
	public static final <T extends Throwable> T cutStacktraceByOne(final T throwable)
	{
		final StackTraceElement[] st1, st2;
		System.arraycopy(st1 = throwable.getStackTrace(), 1,
			st2 = new StackTraceElement[st1.length - 1], 0, st1.length - 1);
		throwable.setStackTrace(st2);
		return throwable;
	}
	
	private StacktraceUtils()
	{
		throw new Error();
	}
}
