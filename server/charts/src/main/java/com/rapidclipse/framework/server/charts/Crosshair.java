
package com.rapidclipse.framework.server.charts;

import java.io.Serializable;

import com.rapidclipse.framework.server.util.JavaScriptable;

import elemental.json.Json;


/**
 * @author XDEV Software
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

	public Double opacity();

	public String focusedColor();

	public Double focusedOpacity();

	public String selectedColor();

	public Double selectedOpacity();

	public static Builder Builder()
	{
		return new Builder.Default();
	}

	public static interface Builder
	{
		public Builder trigger(Trigger trigger);

		public Builder orientation(Orientation orientation);

		public Builder color(String color);

		public Builder opacity(Double opacity);

		public Builder focusedColor(String focusedColor);

		public Builder focusedOpacity(Double focusedOpacity);

		public Builder selectedColor(String selectedColor);

		public Builder selectedOpacity(Double selectedOpacity);

		public Crosshair build();

		public static class Default implements Builder
		{
			private Trigger     trigger;
			private Orientation orientation;
			private String      color;
			private Double      opacity;
			private String      focusedColor;
			private Double      focusedOpacity;
			private String      selectedColor;
			private Double      selectedOpacity;

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
			public Builder opacity(final Double opacity)
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
			public Builder focusedOpacity(final Double focusedOpacity)
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
			public Builder selectedOpacity(final Double selectedOpacity)
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
		private final Double      opacity;
		private final String      focusedColor;
		private final Double      focusedOpacity;
		private final String      selectedColor;
		private final Double      selectedOpacity;

		Default(
			final Trigger trigger,
			final Orientation orientation,
			final String color,
			final Double opacity,
			final String focusedColor,
			final Double focusedOpacity,
			final String selectedColor,
			final Double selectedOpacity)
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
		public Double opacity()
		{
			return this.opacity;
		}

		@Override
		public String focusedColor()
		{
			return this.focusedColor;
		}

		@Override
		public Double focusedOpacity()
		{
			return this.focusedOpacity;
		}

		@Override
		public String selectedColor()
		{
			return this.selectedColor;
		}

		@Override
		public Double selectedOpacity()
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
