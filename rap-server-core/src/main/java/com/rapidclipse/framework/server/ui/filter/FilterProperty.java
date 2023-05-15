/*
 * Copyright (C) 2013-2022 by XDEV Software, All Rights Reserved.
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
 *     XDEV Software - initial API and implementation
 */
package com.rapidclipse.framework.server.ui.filter;

import static java.util.Objects.requireNonNull;

import java.io.Serializable;


public interface FilterProperty<T> extends Serializable
{
	public Object identifier();
	
	public Class<T> type();
	
	public String caption();
	
	public static <T> FilterProperty<T> New(
		final Object identifier,
		final Class<T> type,
		final String caption)
	{
		return new Default<>(identifier, type, caption);
	}
	
	public static class Default<T> implements FilterProperty<T>
	{
		private final Object   identifier;
		private final Class<T> type;
		private final String   caption;
		
		protected Default(final Object identifier, final Class<T> type, final String caption)
		{
			super();
			
			this.identifier = requireNonNull(identifier);
			this.type       = requireNonNull(type);
			this.caption    = requireNonNull(caption);
		}
		
		@Override
		public Object identifier()
		{
			return this.identifier;
		}
		
		@Override
		public Class<T> type()
		{
			return this.type;
		}
		
		@Override
		public String caption()
		{
			return this.caption;
		}
	}
}
