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


/**
 * @author XDEV Software
 * @since 10.02.00
 *
 */
public interface Icon extends Serializable, JavaScriptable
{
	public String normal();

	public String selected();

	public static Icon New(final String normal, final String selected)
	{
		return new Default(normal, selected);
	}
	
	public static class Default implements Icon
	{
		private final String normal;
		private final String selected;

		Default(final String normal, final String selected)
		{
			super();

			this.normal   = normal;
			this.selected = selected;
		}

		@Override
		public String normal()
		{
			return this.normal;
		}

		@Override
		public String selected()
		{
			return this.selected;
		}
		
		@Override
		public String js()
		{
			final ObjectHelper obj = new ObjectHelper();
			obj.putIfNotNull("normal", this.normal);
			obj.putIfNotNull("selected", this.selected);
			return obj.js();
		}
	}
}
