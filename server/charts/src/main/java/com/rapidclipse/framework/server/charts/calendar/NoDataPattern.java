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
package com.rapidclipse.framework.server.charts.calendar;

import java.io.Serializable;

import com.rapidclipse.framework.server.util.JavaScriptable;


/**
 * @author XDEV Software
 * @since 10.02.00
 *
 */
public interface NoDataPattern extends Serializable, JavaScriptable
{
	public String backgroundColor();
	
	public String color();
	
	public static NoDataPattern New(final String backgroundColor, final String color)
	{
		return new Default(backgroundColor, color);
	}
	
	public static class Default implements NoDataPattern
	{
		private final String backgroundColor;
		private final String color;
		
		Default(final String backgroundColor, final String color)
		{
			super();
			
			this.backgroundColor = backgroundColor;
			this.color           = color;
		}
		
		@Override
		public String backgroundColor()
		{
			return this.backgroundColor;
		}
		
		@Override
		public String color()
		{
			return this.color;
		}
		
		@Override
		public String js()
		{
			final ObjectHelper obj = new ObjectHelper();
			obj.putIfNotNull("backgroundColor", this.backgroundColor);
			obj.putIfNotNull("color", this.color);
			return obj.js();
		}
		
	}
	
}
