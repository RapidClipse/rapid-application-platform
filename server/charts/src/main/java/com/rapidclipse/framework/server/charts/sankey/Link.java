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

package com.rapidclipse.framework.server.charts.sankey;

import java.io.Serializable;
import java.util.List;

import com.rapidclipse.framework.server.util.JavaScriptable;


/**
 * @author XDEV Software
 * @since 10.02.00
 *
 */
public interface Link extends Serializable, JavaScriptable
{
	public LinkColor color();
	
	public List<String> colors();

	public ColorMode colorMode();
	
	public static Link New(final LinkColor color, final List<String> colors, final ColorMode colorMode)
	{
		return new Default(color, colors, colorMode);
	}
	
	public static class Default implements Link
	{
		private final LinkColor    color;
		private final List<String> colors;
		private final ColorMode    colorMode;
		
		Default(final LinkColor color, final List<String> colors, final ColorMode colorMode)
		{
			super();

			this.color     = color;
			this.colors    = colors;
			this.colorMode = colorMode;
		}

		@Override
		public LinkColor color()
		{
			return this.color;
		}
		
		@Override
		public List<String> colors()
		{
			return this.colors;
		}

		@Override
		public ColorMode colorMode()
		{
			return this.colorMode;
		}

		@Override
		public String js()
		{
			final ObjectHelper obj = new ObjectHelper();
			obj.putIfNotNull("color", this.color);
			obj.putIfNotNull("colors", new ArrayHelper().addAllStrings(this.colors));
			obj.putIfNotNull("colorMode", this.colorMode);
			return obj.js();
		}
	}
}
