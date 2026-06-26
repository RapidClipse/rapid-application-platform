/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
 */
package com.rapidclipse.framework.server.data.filter;

/**
 * @author XDEV Software
 *
 */
public interface IsNull extends Filter
{
	public Object identifier();
	
	public static IsNull New(final Object identifier)
	{
		return new Default(identifier);
	}

	public static class Default implements IsNull
	{
		private final Object identifier;

		protected Default(final Object identifier)
		{
			super();
			this.identifier = identifier;
		}

		@Override
		public Object identifier()
		{
			return this.identifier;
		}
	}
}
