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
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
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
public interface Annotations extends Serializable, JavaScriptable
{
	public static enum Style implements JavaScriptable
	{
		LINE("line"),
		POINT("point");
		
		private final String js;
		
		private Style(final String js)
		{
			this.js = Json.create(js).toJson();
		}
		
		@Override
		public String js()
		{
			return this.js;
		}
	}
	
	public Boolean alwaysOutside();
	
	public BoxStyle boxStyle();
	
	public AnnotationStyle datum();
	
	public AnnotationStyle domain();
	
	public Boolean highContrast();
	
	public Stem stem();
	
	public Style style();
	
	public TextStyle textStyle();
	
	public static Builder Builder()
	{
		return new Builder.Default();
	}
	
	public static interface Builder
	{
		public Builder alwaysOutside(Boolean alwaysOutside);
		
		public Builder boxStyle(BoxStyle boxStyle);
		
		public Builder datum(AnnotationStyle datum);
		
		public Builder domain(AnnotationStyle domain);
		
		public Builder highContrast(Boolean highContrast);
		
		public Builder stem(Stem stem);
		
		public Builder style(Style style);
		
		public Builder textStyle(TextStyle textStyle);
		
		public Annotations build();
		
		public static class Default implements Builder
		{
			private Boolean         alwaysOutside;
			private BoxStyle        boxStyle;
			private AnnotationStyle datum;
			private AnnotationStyle domain;
			private Boolean         highContrast;
			private Stem            stem;
			private Style           style;
			private TextStyle       textStyle;
			
			Default()
			{
				super();
			}
			
			@Override
			public Builder alwaysOutside(final Boolean alwaysOutside)
			{
				this.alwaysOutside = alwaysOutside;
				return this;
			}
			
			@Override
			public Builder boxStyle(final BoxStyle boxStyle)
			{
				this.boxStyle = boxStyle;
				return this;
			}
			
			@Override
			public Builder datum(final AnnotationStyle datum)
			{
				this.datum = datum;
				return this;
			}
			
			@Override
			public Builder domain(final AnnotationStyle domain)
			{
				this.domain = domain;
				return this;
			}
			
			@Override
			public Builder highContrast(final Boolean highContrast)
			{
				this.highContrast = highContrast;
				return this;
			}
			
			@Override
			public Builder stem(final Stem stem)
			{
				this.stem = stem;
				return this;
			}
			
			@Override
			public Builder style(final Style style)
			{
				this.style = style;
				return this;
			}
			
			@Override
			public Builder textStyle(final TextStyle textStyle)
			{
				this.textStyle = textStyle;
				return this;
			}
			
			@Override
			public Annotations build()
			{
				return new Annotations.Default(this.alwaysOutside, this.boxStyle, this.datum, this.domain,
					this.highContrast, this.stem, this.style, this.textStyle);
			}
		}
	}
	
	public static class Default implements Annotations
	{
		private final Boolean         alwaysOutside;
		private final BoxStyle        boxStyle;
		private final AnnotationStyle datum;
		private final AnnotationStyle domain;
		private final Boolean         highContrast;
		private final Stem            stem;
		private final Style           style;
		private final TextStyle       textStyle;
		
		Default(
			final Boolean alwaysOutside,
			final BoxStyle boxStyle,
			final AnnotationStyle datum,
			final AnnotationStyle domain,
			final Boolean highContrast,
			final Stem stem,
			final Style style,
			final TextStyle textStyle)
		{
			super();
			
			this.alwaysOutside = alwaysOutside;
			this.boxStyle      = boxStyle;
			this.datum         = datum;
			this.domain        = domain;
			this.highContrast  = highContrast;
			this.stem          = stem;
			this.style         = style;
			this.textStyle     = textStyle;
		}
		
		@Override
		public Boolean alwaysOutside()
		{
			return this.alwaysOutside;
		}
		
		@Override
		public BoxStyle boxStyle()
		{
			return this.boxStyle;
		}
		
		@Override
		public AnnotationStyle datum()
		{
			return this.datum;
		}
		
		@Override
		public AnnotationStyle domain()
		{
			return this.domain;
		}
		
		@Override
		public Boolean highContrast()
		{
			return this.highContrast;
		}
		
		@Override
		public Stem stem()
		{
			return this.stem;
		}
		
		@Override
		public Style style()
		{
			return this.style;
		}
		
		@Override
		public TextStyle textStyle()
		{
			return this.textStyle;
		}
		
		@Override
		public String js()
		{
			final ObjectHelper obj = new ObjectHelper();
			obj.putIfNotNull("alwaysOutside", this.alwaysOutside);
			obj.putIfNotNull("boxStyle", this.boxStyle);
			obj.putIfNotNull("datum", this.datum);
			obj.putIfNotNull("domain", this.domain);
			obj.putIfNotNull("highContrast", this.highContrast);
			obj.putIfNotNull("stem", this.stem);
			obj.putIfNotNull("style", this.style);
			obj.putIfNotNull("textStyle", this.textStyle);
			return obj.js();
		}
	}
}
