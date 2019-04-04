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
