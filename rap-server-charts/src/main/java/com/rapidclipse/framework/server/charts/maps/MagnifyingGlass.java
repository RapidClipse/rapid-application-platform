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
public interface MagnifyingGlass extends Serializable, JavaScriptable
{
	public Boolean enable();

	public Number zoomFactor();

	public static MagnifyingGlass New(final Boolean enable, final Number zoomFactor)
	{
		return new Default(enable, zoomFactor);
	}

	public static class Default implements MagnifyingGlass
	{
		private final Boolean enable;
		private final Number  zoomFactor;

		Default(final Boolean enable, final Number zoomFactor)
		{
			super();

			this.enable     = enable;
			this.zoomFactor = zoomFactor;
		}

		@Override
		public Boolean enable()
		{
			return this.enable;
		}

		@Override
		public Number zoomFactor()
		{
			return this.zoomFactor;
		}

		@Override
		public String js()
		{
			final ObjectHelper obj = new ObjectHelper();
			obj.putIfNotNull("enable", this.enable);
			obj.putIfNotNull("zoomFactor", this.zoomFactor);
			return obj.js();
		}

	}

}
