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
public interface Stem extends Serializable, JavaScriptable
{
	public String color();
	
	public Number length();
	
	public static Stem New(final String color)
	{
		return new Default(color, null);
	}

	public static Stem New(final Number length)
	{
		return new Default(null, length);
	}

	public static Stem New(final String color, final Number length)
	{
		return new Default(color, length);
	}
	
	public static class Default implements Stem
	{
		private final String color;
		private final Number length;
		
		Default(final String stemColor, final Number stemLength)
		{
			super();
			
			this.color  = stemColor;
			this.length = stemLength;
		}
		
		@Override
		public String color()
		{
			return this.color;
		}
		
		@Override
		public Number length()
		{
			return this.length;
		}
		
		@Override
		public String js()
		{
			final ObjectHelper obj = new ObjectHelper();
			obj.putIfNotNull("color", this.color);
			obj.putIfNotNull("length", this.length);
			return obj.js();
		}
	}
}
