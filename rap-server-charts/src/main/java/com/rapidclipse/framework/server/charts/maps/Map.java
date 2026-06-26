/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
 */
package com.rapidclipse.framework.server.charts.maps;

import java.io.Serializable;

import com.rapidclipse.framework.server.charts.JavaScriptable;
import com.rapidclipse.framework.server.charts.Styles;


/**
 * @author XDEV Software
 * @since 10.02.00
 *
 */
public interface Map extends Serializable, JavaScriptable
{
	public String name();
	
	public Styles styles();

	public static Map New(final String name, final Styles styles)
	{
		return new Default(name, styles);
	}
	
	public static class Default implements Map
	{
		private final String name;
		private final Styles styles;
		
		Default(final String name, final Styles styles)
		{
			super();
			
			this.name   = name;
			this.styles = styles;
		}
		
		@Override
		public String name()
		{
			return this.name;
		}
		
		@Override
		public Styles styles()
		{
			return this.styles;
		}

		@Override
		public String js()
		{
			final ObjectHelper obj = new ObjectHelper();
			obj.putIfNotNull("name", this.name);
			obj.putIfNotNull("styles", this.styles);
			return obj.js();
		}
	}
	
}
