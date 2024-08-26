/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
 */
package com.rapidclipse.framework.security.authentication;

/**
 * Exception type to indicate that an authentication attempt failed on a business-logical level
 * (e.g. wrong username/password).
 *
 * @author XDEV Software (TM)
 */
public class AuthenticationFailedException extends RuntimeException
{
	///////////////////////////////////////////////////////////////////////////
	// constructors //
	/////////////////
	
	public AuthenticationFailedException()
	{
		super();
	}
	
	public AuthenticationFailedException(
		final String message,
		final Throwable cause,
		final boolean enableSuppression,
		final boolean writableStackTrace)
	{
		super(message, cause, enableSuppression, writableStackTrace);
	}
	
	public AuthenticationFailedException(final String message, final Throwable cause)
	{
		super(message, cause);
	}
	
	public AuthenticationFailedException(final String message)
	{
		super(message);
	}
	
	public AuthenticationFailedException(final Throwable cause)
	{
		super(cause);
	}
	
	///////////////////////////////////////////////////////////////////////////
	// declared methods //
	/////////////////////
	
	// methods resolving the conflict between querying the message and assembling the message
	public final String message()
	{
		return super.getMessage();
	}
	
	public String assembleDetailString()
	{
		return "Authentication failed.";
	}
	
	protected String assembleExplicitMessageAddon()
	{
		final String explicitMessage = super.getMessage();
		return explicitMessage == null ? "" : " (" + explicitMessage + ")";
	}
	
	public String assembleOutputString()
	{
		return this.assembleDetailString() + this.assembleExplicitMessageAddon();
	}
	
	///////////////////////////////////////////////////////////////////////////
	// override methods //
	/////////////////////
	
	/**
	 * Returns an assembled output String due to bad method design in {@link Throwable}.
	 * For the actual message getter, see {@link #message()}.
	 *
	 * @return this exception type's generic string plus an explicit message if present.
	 */
	@Override
	public String getMessage() // intentionally not final to enable subclasses to change the behaviour again
	{
		return this.assembleOutputString();
	}
	
}
