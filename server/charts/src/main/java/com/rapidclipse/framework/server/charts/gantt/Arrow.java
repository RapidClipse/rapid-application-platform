
package com.rapidclipse.framework.server.charts.gantt;

import java.io.Serializable;

import com.rapidclipse.framework.server.util.JavaScriptable;


/**
 * @author XDEV Software
 * @since 10.02.00
 *
 */
public interface Arrow extends Serializable, JavaScriptable
{
	public Number angle();

	public String color();

	public Number length();

	public Number radius();

	public Number spaceAfter();

	public Number width();

	public static Builder Builder()
	{
		return new Builder.Default();
	}

	public static interface Builder
	{
		public Builder angle(Number angle);

		public Builder color(String color);

		public Builder length(Number length);

		public Builder radius(Number radius);

		public Builder spaceAfter(Number spaceAfter);

		public Builder width(Number width);

		public Arrow build();
		
		public static class Default implements Builder
		{
			private Number angle;
			private String color;
			private Number length;
			private Number radius;
			private Number spaceAfter;
			private Number width;
			
			Default()
			{
				super();
			}
			
			@Override
			public Builder angle(final Number angle)
			{
				this.angle = angle;
				return this;
			}

			@Override
			public Builder color(final String color)
			{
				this.color = color;
				return this;
			}

			@Override
			public Builder length(final Number length)
			{
				this.length = length;
				return this;
			}

			@Override
			public Builder radius(final Number radius)
			{
				this.radius = radius;
				return this;
			}

			@Override
			public Builder spaceAfter(final Number spaceAfter)
			{
				this.spaceAfter = spaceAfter;
				return this;
			}

			@Override
			public Builder width(final Number width)
			{
				this.width = width;
				return this;
			}

			@Override
			public Arrow build()
			{
				return new Arrow.Default(this.angle, this.color, this.length, this.radius, this.spaceAfter, this.width);
			}
		}
	}

	public static class Default implements Arrow
	{
		private final Number angle;
		private final String color;
		private final Number length;
		private final Number radius;
		private final Number spaceAfter;
		private final Number width;

		Default(
			final Number angle,
			final String color,
			final Number length,
			final Number radius,
			final Number spaceAfter,
			final Number width)
		{
			super();
			
			this.angle      = angle;
			this.color      = color;
			this.length     = length;
			this.radius     = radius;
			this.spaceAfter = spaceAfter;
			this.width      = width;
		}

		@Override
		public Number angle()
		{
			return this.angle;
		}

		@Override
		public String color()
		{
			return this.color;
		}

		@Override
		public Number length()
		{
			return this.length;
		}

		@Override
		public Number radius()
		{
			return this.radius;
		}

		@Override
		public Number spaceAfter()
		{
			return this.spaceAfter;
		}

		@Override
		public Number width()
		{
			return this.width;
		}

		@Override
		public String js()
		{
			final ObjectHelper obj = new ObjectHelper();
			obj.putIfNotNull("angle", this.angle);
			obj.putIfNotNull("color", this.color);
			obj.putIfNotNull("length", this.length);
			obj.putIfNotNull("radius", this.radius);
			obj.putIfNotNull("spaceAfter", this.spaceAfter);
			obj.putIfNotNull("width", this.width);
			return obj.js();
		}
	}
}
