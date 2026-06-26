/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
 */
package com.rapidclipse.framework.server.charts;

import java.io.Serializable;

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
