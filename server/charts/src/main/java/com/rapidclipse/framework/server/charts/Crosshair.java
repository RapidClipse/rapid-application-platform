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
public interface Crosshair extends Serializable, JavaScriptable
{
	public static enum Trigger implements JavaScriptable
	{
		BOTH("both"),
		FOCUS("focus"),
		SELECTION("selection");

		private final String js;

		private Trigger(final String js)
		{
			this.js = Json.create(js).toJson();
		}

		@Override
		public String js()
		{
			return this.js;
		}
	}

	public static enum Orientation implements JavaScriptable
	{
		BOTH("both"),
		HORIZONTAL("horizontal"),
		VERTICAL("vertical");

		private final String js;

		private Orientation(final String js)
		{
			this.js = Json.create(js).toJson();
		}

		@Override
		public String js()
		{
			return this.js;
		}
	}

	public Trigger trigger();

	public Orientation orientation();

	public String color();

	public Number opacity();

	public String focusedColor();

	public Number focusedOpacity();

	public String selectedColor();

	public Number selectedOpacity();

	public static Builder Builder()
	{
		return new Builder.Default();
	}

	public static interface Builder
	{
		public Builder trigger(Trigger trigger);

		public Builder orientation(Orientation orientation);

		public Builder color(String color);

		public Builder opacity(Number opacity);

		public Builder focusedColor(String focusedColor);

		public Builder focusedOpacity(Number focusedOpacity);

		public Builder selectedColor(String selectedColor);

		public Builder selectedOpacity(Number selectedOpacity);

		public Crosshair build();

		public static class Default implements Builder
		{
			private Trigger     trigger;
			private Orientation orientation;
			private String      color;
			private Number      opacity;
			private String      focusedColor;
			private Number      focusedOpacity;
			private String      selectedColor;
			private Number      selectedOpacity;

			Default()
			{
				super();
			}

			@Override
			public Builder trigger(final Trigger trigger)
			{
				this.trigger = trigger;
				return this;
			}

			@Override
			public Builder orientation(final Orientation orientation)
			{
				this.orientation = orientation;
				return this;
			}

			@Override
			public Builder color(final String color)
			{
				this.color = color;
				return this;
			}

			@Override
			public Builder opacity(final Number opacity)
			{
				this.opacity = opacity;
				return this;
			}

			@Override
			public Builder focusedColor(final String focusedColor)
			{
				this.focusedColor = focusedColor;
				return this;
			}

			@Override
			public Builder focusedOpacity(final Number focusedOpacity)
			{
				this.focusedOpacity = focusedOpacity;
				return this;
			}

			@Override
			public Builder selectedColor(final String selectedColor)
			{
				this.selectedColor = selectedColor;
				return this;
			}

			@Override
			public Builder selectedOpacity(final Number selectedOpacity)
			{
				this.selectedOpacity = selectedOpacity;
				return this;
			}

			@Override
			public Crosshair build()
			{
				return new Crosshair.Default(this.trigger, this.orientation, this.color, this.opacity,
					this.focusedColor, this.focusedOpacity, this.selectedColor, this.selectedOpacity);
			}

		}

	}

	public static class Default implements Crosshair
	{
		private final Trigger     trigger;
		private final Orientation orientation;
		private final String      color;
		private final Number      opacity;
		private final String      focusedColor;
		private final Number      focusedOpacity;
		private final String      selectedColor;
		private final Number      selectedOpacity;

		Default(
			final Trigger trigger,
			final Orientation orientation,
			final String color,
			final Number opacity,
			final String focusedColor,
			final Number focusedOpacity,
			final String selectedColor,
			final Number selectedOpacity)
		{
			super();

			this.trigger         = trigger;
			this.orientation     = orientation;
			this.color           = color;
			this.opacity         = opacity;
			this.focusedColor    = focusedColor;
			this.focusedOpacity  = focusedOpacity;
			this.selectedColor   = selectedColor;
			this.selectedOpacity = selectedOpacity;
		}

		@Override
		public Trigger trigger()
		{
			return this.trigger;
		}

		@Override
		public Orientation orientation()
		{
			return this.orientation;
		}

		@Override
		public String color()
		{
			return this.color;
		}

		@Override
		public Number opacity()
		{
			return this.opacity;
		}

		@Override
		public String focusedColor()
		{
			return this.focusedColor;
		}

		@Override
		public Number focusedOpacity()
		{
			return this.focusedOpacity;
		}

		@Override
		public String selectedColor()
		{
			return this.selectedColor;
		}

		@Override
		public Number selectedOpacity()
		{
			return this.selectedOpacity;
		}

		@Override
		public String js()
		{
			final ObjectHelper obj = new ObjectHelper();
			obj.putIfNotNull("trigger", this.trigger);
			obj.putIfNotNull("orientation", this.orientation);
			obj.putIfNotNull("color", this.color);
			obj.putIfNotNull("opacity", this.opacity);
			obj.putIfNotNull("focused", new ObjectHelper()
				.putIfNotNull("color", this.focusedColor)
				.putIfNotNull("opacity", this.focusedOpacity));
			obj.putIfNotNull("selected", new ObjectHelper()
				.putIfNotNull("color", this.selectedColor)
				.putIfNotNull("opacity", this.selectedOpacity));
			return obj.js();
		}

	}

}
