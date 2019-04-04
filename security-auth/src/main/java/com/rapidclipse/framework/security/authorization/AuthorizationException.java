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
package com.rapidclipse.framework.security.authorization;

/**
 * An exception type indicating that an authorization was not successful. The cause of such an exception does
 * not necessarily have to be a problem, but is usually just a control flow information, that validation of
 * credentials failed on the business-logical level.
 *
 * @author XDEV Software (TM)
 */
public class AuthorizationException extends RuntimeException
{
	///////////////////////////////////////////////////////////////////////////
	// constructors //
	/////////////////
	
	public AuthorizationException()
	{
		super();
	}
	
	public AuthorizationException(
		final String message,
		final Throwable cause,
		final boolean enableSuppression,
		final boolean writableStackTrace)
	{
		super(message, cause, enableSuppression, writableStackTrace);
	}
	
	public AuthorizationException(final String message, final Throwable cause)
	{
		super(message, cause);
	}
	
	public AuthorizationException(final String message)
	{
		super(message);
	}
	
	public AuthorizationException(final Throwable cause)
	{
		super(cause);
	}
	
}
