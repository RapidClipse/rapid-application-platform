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
import java.util.Objects;

import com.rapidclipse.framework.server.util.JavaScriptable;

import elemental.json.Json;


/**
 *
 * @author XDEV Software
 * @since 10.02.00
 */
public interface PointShape extends Serializable, JavaScriptable
{
	public static enum Type implements JavaScriptable
	{
		CIRCLE("circle"),
		TRIANGLE("triangle"),
		SQUARE("square"),
		DIAMOND("diamond"),
		STAR("star"),
		POLYGON("polygon");
		
		private final String js;
		
		private Type(final String js)
		{
			this.js = Json.create(js).toJson();
		}
		
		@Override
		public String js()
		{
			return this.js;
		}
	}
	
	public Type type();
	
	public Integer sides();
	
	public Double dent();
	
	public Number rotation();
	
	public static Builder Builder()
	{
		return new Builder.Default();
	}
	
	public static interface Builder
	{
		public Builder type(Type type);
		
		public Builder sides(Integer sides);
		
		public Builder dent(Double dent);
		
		public Builder rotation(Number rotation);
		
		public PointShape build();
		
		public static class Default implements Builder
		{
			private Type    type;
			private Integer sides;
			private Double  dent;
			private Number  rotation;
			
			Default()
			{
				super();
			}
			
			@Override
			public Builder type(final Type type)
			{
				this.type = type;
				return this;
			}
			
			@Override
			public Builder sides(final Integer sides)
			{
				this.sides = sides;
				return this;
			}
			
			@Override
			public Builder dent(final Double dent)
			{
				this.dent = dent;
				return this;
			}
			
			@Override
			public Builder rotation(final Number rotation)
			{
				this.rotation = rotation;
				return this;
			}
			
			@Override
			public PointShape build()
			{
				return PointShape.New(this.type, this.sides, this.dent, this.rotation);
			}
			
		}
		
	}
	
	public static PointShape New(final Type type)
	{
		return new Default(type, null, null, null);
	}

	public static PointShape New(final Type type, final Integer sides, final Double dent, final Number rotation)
	{
		return new Default(type, sides, dent, rotation);
	}
	
	public static class Default implements PointShape
	{
		private final Type    type;
		private final Integer sides;
		private final Double  dent;
		private final Number  rotation;
		
		Default(final Type type, final Integer sides, final Double dent, final Number rotation)
		{
			super();
			
			this.type     = Objects.requireNonNull(type);
			this.sides    = sides;
			this.dent     = dent;
			this.rotation = rotation;
		}
		
		@Override
		public Type type()
		{
			return this.type;
		}
		
		@Override
		public Integer sides()
		{
			return this.sides;
		}
		
		@Override
		public Double dent()
		{
			return this.dent;
		}
		
		@Override
		public Number rotation()
		{
			return this.rotation;
		}
		
		@Override
		public String js()
		{
			final ObjectHelper obj = new ObjectHelper();
			obj.put("type", this.type);
			obj.putIfNotNull("sides", this.sides);
			obj.putIfNotNull("dent", this.dent);
			obj.putIfNotNull("rotation", this.rotation);
			return obj.js();
		}
		
	}
	
}
