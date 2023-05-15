/*
 * Copyright (C) 2013-2023 by XDEV Software, All Rights Reserved.
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
 *     XDEV Software - initial API and implementation
 */
package com.rapidclipse.framework.server.charts;

import java.io.Serializable;

import elemental.json.Json;


/**
 *
 * @author XDEV Software
 * @since 10.02.00
 */
public interface Tooltip extends Serializable, JavaScriptable
{
	public static enum Text implements JavaScriptable
	{
		BOTH("both"),
		VALUE("value"),
		PERCENTAGE("percentage");
		
		private final String js;
		
		private Text(final String js)
		{
			this.js = Json.create(js).toJson();
		}
		
		@Override
		public String js()
		{
			return this.js;
		}
	}
	
	public static enum Trigger implements JavaScriptable
	{
		FOCUS("focus"),
		NONE("none"),
		SELECTION("certainty");
		
		private final String js;
		
		private Trigger(final String js)
		{
			this.js = js;
		}
		
		@Override
		public String js()
		{
			return this.js;
		}
	}
	
	public Boolean ignoreBounds();
	
	public Boolean isHtml();
	
	public Boolean showColorCode();
	
	public Text text();
	
	public TextStyle textStyle();
	
	public Trigger trigger();
	
	public static Builder Builder()
	{
		return new Builder.Default();
	}
	
	public static interface Builder
	{
		public Builder ignoreBounds(Boolean ignoreBounds);
		
		public Builder isHtml(Boolean isHtml);
		
		public Builder showColorCode(Boolean showColorCode);
		
		public Builder text(Text text);
		
		public Builder textStyle(TextStyle textStyle);
		
		public Builder trigger(Trigger trigger);
		
		public Tooltip build();
		
		public static class Default implements Builder
		{
			private Boolean   ignoreBounds;
			private Boolean   isHtml;
			private Boolean   showColorCode;
			private Text      text;
			private TextStyle textStyle;
			private Trigger   trigger;
			
			Default()
			{
				super();
			}
			
			@Override
			public Builder ignoreBounds(final Boolean ignoreBounds)
			{
				this.ignoreBounds = ignoreBounds;
				return this;
			}
			
			@Override
			public Builder isHtml(final Boolean isHtml)
			{
				this.isHtml = isHtml;
				return this;
			}
			
			@Override
			public Builder showColorCode(final Boolean showColorCode)
			{
				this.showColorCode = showColorCode;
				return this;
			}
			
			@Override
			public Builder text(final Text text)
			{
				this.text = text;
				return this;
			}
			
			@Override
			public Builder textStyle(final TextStyle textStyle)
			{
				this.textStyle = textStyle;
				return this;
			}
			
			@Override
			public Builder trigger(final Trigger trigger)
			{
				this.trigger = trigger;
				return this;
			}
			
			@Override
			public Tooltip build()
			{
				return Tooltip.New(this.ignoreBounds, this.isHtml, this.showColorCode, this.text, this.textStyle,
					this.trigger);
			}
			
		}
		
	}
	
	public static Tooltip New(
		final Boolean ignoreBounds,
		final Boolean isHtml,
		final Boolean showColorCode,
		final Text text,
		final TextStyle textStyle,
		final Trigger trigger)
	{
		return new Default(ignoreBounds, isHtml, showColorCode, text, textStyle, trigger);
	}
	
	public static class Default implements Tooltip
	{
		private final Boolean   ignoreBounds;
		private final Boolean   isHtml;
		private final Boolean   showColorCode;
		private final Text      text;
		private final TextStyle textStyle;
		private final Trigger   trigger;

		Default(
			final Boolean ignoreBounds,
			final Boolean isHtml,
			final Boolean showColorCode,
			final Text text,
			final TextStyle textStyle,
			final Trigger trigger)
		{
			super();
			
			this.ignoreBounds  = ignoreBounds;
			this.isHtml        = isHtml;
			this.showColorCode = showColorCode;
			this.text          = text;
			this.textStyle     = textStyle;
			this.trigger       = trigger;
		}

		@Override
		public Boolean ignoreBounds()
		{
			return this.ignoreBounds;
		}
		
		@Override
		public Boolean isHtml()
		{
			return this.isHtml;
		}
		
		@Override
		public Boolean showColorCode()
		{
			return this.showColorCode;
		}
		
		@Override
		public Text text()
		{
			return this.text;
		}
		
		@Override
		public TextStyle textStyle()
		{
			return this.textStyle;
		}
		
		@Override
		public Trigger trigger()
		{
			return this.trigger;
		}
		
		@Override
		public String js()
		{
			final ObjectHelper obj = new ObjectHelper();
			obj.putIfNotNull("ignoreBounds", this.ignoreBounds);
			obj.putIfNotNull("isHtml", this.isHtml);
			obj.putIfNotNull("text", this.text);
			obj.putIfNotNull("textStyle", this.textStyle);
			obj.putIfNotNull("trigger", this.trigger);
			return obj.js();
		}

	}

}
