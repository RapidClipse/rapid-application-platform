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
package com.rapidclipse.framework.server.mobilekit.contacts;

import com.rapidclipse.framework.server.mobilekit.MobileServiceError;


/**
 * @author XDEV Software
 *
 */
public class ContactsServiceError extends MobileServiceError
{
	public static enum Reason
	{
		UNKNOWN_ERROR(0),
		INVALID_ARGUMENT_ERROR(1),
		TIMEOUT_ERROR(2),
		PENDING_OPERATION_ERROR(3),
		IO_ERROR(4),
		NOT_SUPPORTED_ERROR(5),
		OPERATION_CANCELLED_ERROR(6),
		PERMISSION_DENIED_ERROR(20);
		
		private final int code;
		
		private Reason(final int code)
		{
			this.code = code;
		}
		
		public static Reason getByCode(final int code)
		{
			for(final Reason reason : values())
			{
				if(reason.code == code)
				{
					return reason;
				}
			}
			return null;
		}
	}
	
	private final Reason reason;
	
	public ContactsServiceError(
		final ContactsService source,
		final String message,
		final Reason reason)
	{
		super(source, message);
		
		this.reason = reason;
	}
	
	@Override
	public ContactsService getSource()
	{
		return (ContactsService)super.getSource();
	}
	
	public Reason getReason()
	{
		return this.reason;
	}
}
