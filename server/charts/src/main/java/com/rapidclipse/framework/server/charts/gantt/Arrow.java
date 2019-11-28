
package com.rapidclipse.framework.server.charts.gantt;

import java.io.Serializable;

import com.rapidclipse.framework.server.util.JavaScriptable;


/**
 * @author XDEV Software
 *
 */
public interface Arrow extends Serializable, JavaScriptable
{
	public Double angle();

	public String color();

	public Double length();

	public Double radius();

	public Double spaceAfter();

	public Double width();

	public static Builder Builder()
	{
		return new Builder.Default();
	}

	public static interface Builder
	{
		public Builder angle(Double angle);

		public Builder color(String color);

		public Builder length(Double length);

		public Builder radius(Double radius);

		public Builder spaceAfter(Double spaceAfter);

		public Builder width(Double width);

		public Arrow build();
		
		public static class Default implements Builder
		{
			private Double angle;
			private String color;
			private Double length;
			private Double radius;
			private Double spaceAfter;
			private Double width;
			
			Default()
			{
				super();
			}
			
			@Override
			public Builder angle(final Double angle)
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
			public Builder length(final Double length)
			{
				this.length = length;
				return this;
			}

			@Override
			public Builder radius(final Double radius)
			{
				this.radius = radius;
				return this;
			}

			@Override
			public Builder spaceAfter(final Double spaceAfter)
			{
				this.spaceAfter = spaceAfter;
				return this;
			}

			@Override
			public Builder width(final Double width)
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
		private final Double angle;
		private final String color;
		private final Double length;
		private final Double radius;
		private final Double spaceAfter;
		private final Double width;

		Default(
			final Double angle,
			final String color,
			final Double length,
			final Double radius,
			final Double spaceAfter,
			final Double width)
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
		public Double angle()
		{
			return this.angle;
		}

		@Override
		public String color()
		{
			return this.color;
		}

		@Override
		public Double length()
		{
			return this.length;
		}

		@Override
		public Double radius()
		{
			return this.radius;
		}

		@Override
		public Double spaceAfter()
		{
			return this.spaceAfter;
		}

		@Override
		public Double width()
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
