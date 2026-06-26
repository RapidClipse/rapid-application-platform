/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
 */
package com.rapidclipse.framework.server.charts;

import static java.util.Objects.requireNonNull;

import java.io.Serializable;


/**
 * @author XDEV Software
 * @since 10.02.00
 *
 */
public interface Gradient extends Serializable, JavaScriptable
{
	public String color1();
	
	public String color2();
	
	public String x1();
	
	public String y1();
	
	public String x2();
	
	public String y2();
	
	public Boolean useObjectBoundingBoxUnits();
	
	public static Gradient New(
		final String color1,
		final String color2,
		final String x1,
		final String y1,
		final String x2,
		final String y2)
	{
		return new Default(color1, color2, x1, y1, x2, y2, null);
	}
	
	public static Gradient New(
		final String color1,
		final String color2,
		final String x1,
		final String y1,
		final String x2,
		final String y2,
		final Boolean useObjectBoundingBoxUnits)
	{
		return new Default(color1, color2, x1, y1, x2, y2, useObjectBoundingBoxUnits);
	}
	
	public static class Default implements Gradient
	{
		private final String  color1;
		private final String  color2;
		private final String  x1;
		private final String  y1;
		private final String  x2;
		private final String  y2;
		private final Boolean useObjectBoundingBoxUnits;
		
		Default(
			final String color1,
			final String color2,
			final String x1,
			final String y1,
			final String x2,
			final String y2,
			final Boolean useObjectBoundingBoxUnits)
		{
			super();
			
			this.color1                    = requireNonNull(color1);
			this.color2                    = requireNonNull(color2);
			this.x1                        = requireNonNull(x1);
			this.y1                        = requireNonNull(y1);
			this.x2                        = requireNonNull(x2);
			this.y2                        = requireNonNull(y2);
			this.useObjectBoundingBoxUnits = useObjectBoundingBoxUnits;
		}
		
		@Override
		public String color1()
		{
			return this.color1;
		}
		
		@Override
		public String color2()
		{
			return this.color2;
		}
		
		@Override
		public String x1()
		{
			return this.x1;
		}
		
		@Override
		public String y1()
		{
			return this.y1;
		}
		
		@Override
		public String x2()
		{
			return this.x2;
		}
		
		@Override
		public String y2()
		{
			return this.y2;
		}
		
		@Override
		public Boolean useObjectBoundingBoxUnits()
		{
			return this.useObjectBoundingBoxUnits;
		}
		
		@Override
		public String js()
		{
			final ObjectHelper obj = new ObjectHelper();
			obj.putIfNotNull("color1", this.color1);
			obj.putIfNotNull("color2", this.color2);
			obj.putIfNotNull("x1", this.x1);
			obj.putIfNotNull("y1", this.y1);
			obj.putIfNotNull("x2", this.x2);
			obj.putIfNotNull("y2", this.y2);
			obj.putIfNotNull("useObjectBoundingBoxUnits", this.useObjectBoundingBoxUnits);
			return obj.js();
		}
	}
}
