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
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *
 * Contributors:
 *     XDEV Software Corp. - initial API and implementation
 */
package com.rapidclipse.framework.server.charts;

import java.io.Serializable;


/**
 * @author XDEV Software
 * @since 10.02.00
 *
 */
public interface Area extends Serializable, JavaScriptable
{
	public String left();
	
	public String top();
	
	public String width();
	
	public String height();
	
	public String backgroundColor();
	
	public static Area
		New(final String left, final String top, final String width, final String height)
	{
		return new Default(left, top, width, height, null);
	}
	
	public static Area
		New(final String backgroundColor)
	{
		return new Default(null, null, null, null, backgroundColor);
	}
	
	public static Area
		New(final String left, final String top, final String width, final String height, final String backgroundColor)
	{
		return new Default(left, top, width, height, backgroundColor);
	}
	
	public static class Default implements Area
	{
		private final String left;
		private final String top;
		private final String width;
		private final String height;
		private final String backgroundColor;
		
		Default(
			final String left,
			final String top,
			final String width,
			final String height,
			final String backgroundColor)
		{
			super();
			
			this.left            = left;
			this.top             = top;
			this.width           = width;
			this.height          = height;
			this.backgroundColor = backgroundColor;
		}
		
		@Override
		public String left()
		{
			return this.left;
		}
		
		@Override
		public String top()
		{
			return this.top;
		}
		
		@Override
		public String width()
		{
			return this.width;
		}
		
		@Override
		public String height()
		{
			return this.height;
		}
		
		@Override
		public String backgroundColor()
		{
			return this.backgroundColor;
		}
		
		@Override
		public String js()
		{
			final ObjectHelper obj = new ObjectHelper();
			obj.putIfNotNull("left", this.left);
			obj.putIfNotNull("top", this.top);
			obj.putIfNotNull("width", this.width);
			obj.putIfNotNull("height", this.height);
			obj.putIfNotNull("backgroundColor", this.backgroundColor);
			return obj.js();
		}
		
	}
	
}
