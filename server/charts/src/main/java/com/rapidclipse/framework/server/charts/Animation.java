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
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *
 * Contributors:
 *     XDEV Software Corp. - initial API and implementation
 */
package com.rapidclipse.framework.server.charts;

import java.io.Serializable;

import elemental.json.Json;


/**
 * @author XDEV Software
 * @since 10.02.00
 *
 */
public interface Animation extends Serializable, JavaScriptable
{
	public static enum Easing implements JavaScriptable
	{
		LINEAR("linear"),
		IN("in"),
		OUT("out"),
		IN_AND_OUT("inAndOut");
		
		private final String js;
		
		private Easing(final String js)
		{
			this.js = Json.create(js).toJson();
		}
		
		@Override
		public String js()
		{
			return this.js;
		}
	}
	
	public Number duration();
	
	public Easing easing();
	
	public Boolean startup();
	
	public static Animation New(final Number duration)
	{
		return new Default(duration, null, null);
	}
	
	public static Animation New(final Easing easing)
	{
		return new Default(null, easing, null);
	}
	
	public static Animation New(final Number duration, final Easing easing)
	{
		return new Default(duration, easing, null);
	}
	
	public static Animation New(final Number duration, final Easing easing, final Boolean startup)
	{
		return new Default(duration, easing, startup);
	}
	
	public static class Default implements Animation
	{
		private final Number  duration;
		private final Easing  easing;
		private final Boolean startup;
		
		Default(final Number duration, final Easing easing, final Boolean startup)
		{
			super();
			
			this.duration = duration;
			this.easing   = easing;
			this.startup  = startup;
		}
		
		@Override
		public Number duration()
		{
			return this.duration;
		}
		
		@Override
		public Easing easing()
		{
			return this.easing;
		}
		
		@Override
		public Boolean startup()
		{
			return this.startup;
		}
		
		@Override
		public String js()
		{
			final ObjectHelper obj = new ObjectHelper();
			obj.putIfNotNull("duration", this.duration);
			obj.putIfNotNull("easing", this.easing);
			obj.putIfNotNull("startup", this.startup);
			return obj.js();
		}
	}
}
