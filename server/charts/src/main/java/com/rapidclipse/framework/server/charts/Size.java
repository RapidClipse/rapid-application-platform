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

package com.rapidclipse.framework.server.charts;

import java.io.Serializable;

import com.rapidclipse.framework.server.util.JavaScriptable;

import elemental.json.Json;


/**
 * @author XDEV Software
 * @since 10.02.00
 *
 */
public interface Size extends Serializable, JavaScriptable
{
	public static Size Pixels(final int value)
	{
		return new Pixels(value);
	}
	
	public static Size Percent(final int value)
	{
		return new Percent(value);
	}
	
	public static class Pixels implements Size
	{
		private final Integer value;
		
		Pixels(final int value)
		{
			super();
			
			this.value = value;
		}
		
		@Override
		public String js()
		{
			return Json.create(this.value).toJson();
		}
	}
	
	public static class Percent implements Size
	{
		private final String value;
		
		Percent(final int value)
		{
			super();
			
			this.value = Integer.toString(value).concat("%");
		}
		
		@Override
		public String js()
		{
			return Json.create(this.value).toJson();
		}
	}
}
