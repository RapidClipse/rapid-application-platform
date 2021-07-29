/*
 * Copyright (C) 2013-2021 by XDEV Software, All Rights Reserved.
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
 *     XDEV Software Corp. - initial API and implementation
 */
package com.rapidclipse.framework.server.navigation;

/**
 * @author XDEV Software
 *
 */
public class NavigationException extends RuntimeException
{
	/**
	 *
	 */
	public NavigationException()
	{
		super();
	}
	
	/**
	 * @param message
	 * @param cause
	 * @param enableSuppression
	 * @param writableStackTrace
	 */
	public NavigationException(
		final String message,
		final Throwable cause,
		final boolean enableSuppression,
		final boolean writableStackTrace)
	{
		super(message, cause, enableSuppression, writableStackTrace);
	}
	
	/**
	 * @param message
	 * @param cause
	 */
	public NavigationException(final String message, final Throwable cause)
	{
		super(message, cause);
	}
	
	/**
	 * @param message
	 */
	public NavigationException(final String message)
	{
		super(message);
	}
	
	/**
	 * @param cause
	 */
	public NavigationException(final Throwable cause)
	{
		super(cause);
	}
}
