/*
 * Copyright (C) 2013-2019 by XDEV Software, All Rights Reserved.
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
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *
 * Contributors:
 *     XDEV Software Corp. - initial API and implementation
 */
package com.rapidclipse.framework.server.charts.annotation;

import java.io.Serializable;

import com.rapidclipse.framework.server.charts.JavaScriptable;

import elemental.json.Json;


/**
 * @author XDEV Software
 * @since 10.02.00
 *
 */
public interface Table extends Serializable, JavaScriptable
{
	public static enum Column implements JavaScriptable
	{
		LABEL(0),
		TEXT(1);
		
		private final String js;
		
		private Column(final Integer index)
		{
			this.js = Json.create(index).toJson();
		}
		
		@Override
		public String js()
		{
			return this.js;
		}
	}
	
	public Boolean sortAscending();
	
	public Column sortColumn();
	
	public static Table New(final Boolean sortAscending, final Column sortColumn)
	{
		return new Default(sortAscending, sortColumn);
	}
	
	public static class Default implements Table
	{
		private final Boolean sortAscending;
		private final Column  sortColumn;
		
		Default(final Boolean sortAscending, final Column sortColumn)
		{
			super();
			
			this.sortAscending = sortAscending;
			this.sortColumn    = sortColumn;
		}
		
		@Override
		public Boolean sortAscending()
		{
			return this.sortAscending;
		}
		
		@Override
		public Column sortColumn()
		{
			return this.sortColumn;
		}
		
		@Override
		public String js()
		{
			final ObjectHelper obj = new ObjectHelper();
			obj.putIfNotNull("sortAscending", this.sortAscending);
			obj.putIfNotNull("sortColumn", this.sortColumn);
			return obj.js();
		}
	}
}
