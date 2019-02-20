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

package com.rapidclipse.framework.server.mobilekit;

/**
 * @author XDEV Software
 *
 */
public class MobileServiceError
{
	private final MobileService source;
	private final String        message;
	
	public MobileServiceError(final MobileService source, final String message)
	{
		this.source  = source;
		this.message = message;
	}
	
	public MobileService getSource()
	{
		return this.source;
	}
	
	public String getMessage()
	{
		return this.message;
	}
	
	@Override
	public String toString()
	{
		return getClass().getName() + " [source=" + this.source + ", message=" + this.message + "]";
	}
}
