/*
 * Copyright (C) 2013-2023 by XDEV Software, All Rights Reserved.
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
package com.rapidclipse.framework.server.charts;

import java.io.Serializable;


/**
 * @author XDEV Software
 * @since 10.02.00
 *
 */
public interface GridLines extends Serializable, JavaScriptable
{
	public String color();
	
	public Integer count();
	
	public static GridLines New(final String color)
	{
		return new Default(color, null);
	}
	
	public static GridLines New(final Integer count)
	{
		return new Default(null, count);
	}
	
	public static GridLines New(final String color, final Integer count)
	{
		return new Default(color, count);
	}
	
	public static class Default implements GridLines
	{
		private final String  color;
		private final Integer count;
		
		Default(final String color, final Integer count)
		{
			super();
			
			this.color = color;
			this.count = count;
		}
		
		@Override
		public String color()
		{
			return this.color;
		}
		
		@Override
		public Integer count()
		{
			return this.count;
		}
		
		@Override
		public String js()
		{
			final ObjectHelper obj = new ObjectHelper();
			obj.putIfNotNull("color", this.color);
			obj.putIfNotNull("count", this.count);
			return obj.js();
		}
		
	}
	
}
