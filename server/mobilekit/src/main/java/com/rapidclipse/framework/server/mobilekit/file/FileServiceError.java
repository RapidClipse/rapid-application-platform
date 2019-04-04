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
package com.rapidclipse.framework.server.mobilekit.file;

import com.rapidclipse.framework.server.mobilekit.MobileServiceError;


/**
 * @author XDEV Software
 *
 */
public class FileServiceError extends MobileServiceError
{
	public static enum Reason
	{
		NOT_FOUND_ERR(1),
		SECURITY_ERR(2),
		ABORT_ERR(3),
		NOT_READABLE_ERR(4),
		ENCODING_ERR(5),
		NO_MODIFICATION_ALLOWED_ERR(6),
		INVALID_STATE_ERR(7),
		SYNTAX_ERR(8),
		INVALID_MODIFICATION_ERR(9),
		QUOTA_EXCEEDED_ERR(10),
		TYPE_MISMATCH_ERR(11),
		PATH_EXISTS_ERR(12);
		
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
	
	public FileServiceError(final FileService source, final String message, final Reason reason)
	{
		super(source, message);
		
		this.reason = reason;
	}
	
	@Override
	public FileService getSource()
	{
		return (FileService)super.getSource();
	}
	
	public Reason getReason()
	{
		return this.reason;
	}
}
