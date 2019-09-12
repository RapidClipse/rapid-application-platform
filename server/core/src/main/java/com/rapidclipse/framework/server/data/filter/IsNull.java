/*
 * Copyright (C) 2013-2019 by XDEV Software, All Rights Reserved.
 *
 * This file is part of the RapidClipse Application Platform (RAP).
 *
 * RAP is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * RAP is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with RAP. If not, see <http://www.gnu.org/licenses/>.
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *
 * Contributors:
 *     XDEV Software Corp. - initial API and implementation
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
