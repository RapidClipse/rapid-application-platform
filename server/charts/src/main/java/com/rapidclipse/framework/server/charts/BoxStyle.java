
package com.rapidclipse.framework.server.charts;

import java.io.Serializable;

import com.rapidclipse.framework.server.util.JavaScriptable;


/**
 * @author XDEV Software
 *
 */
public interface BoxStyle extends Serializable, JavaScriptable
{
	public String stroke();

	public Integer strokeWidth();

	public Integer radiusX();

	public Integer radiusY();

	public Gradient gradient();

	public static Builder Builder()
	{
		return new Builder.Default();
	}

	public static interface Builder
	{
		public Builder stroke(String stroke);

		public Builder strokeWidth(Integer strokeWidth);

		public Builder radiusX(Integer radiusX);

		public Builder radiusY(Integer radiusY);

		public Builder gradient(Gradient gradient);

		public BoxStyle build();

		public static class Default implements Builder
		{
			private String   stroke;
			private Integer  strokeWidth;
			private Integer  radiusX;
			private Integer  radiusY;
			private Gradient gradient;

			Default()
			{
				super();
			}

			@Override
			public Builder stroke(final String stroke)
			{
				this.stroke = stroke;
				return this;
			}

			@Override
			public Builder strokeWidth(final Integer strokeWidth)
			{
				this.strokeWidth = strokeWidth;
				return this;
			}

			@Override
			public Builder radiusX(final Integer radiusX)
			{
				this.radiusX = radiusX;
				return this;
			}

			@Override
			public Builder radiusY(final Integer radiusY)
			{
				this.radiusY = radiusY;
				return this;
			}

			@Override
			public Builder gradient(final Gradient gradient)
			{
				this.gradient = gradient;
				return this;
			}

			@Override
			public BoxStyle build()
			{
				return new BoxStyle.Default(this.stroke, this.strokeWidth, this.radiusX, this.radiusY, this.gradient);
			}
		}
	}

	public static BoxStyle New(
		final String stroke,
		final Integer strokeWidth)
	{
		return new Default(stroke, strokeWidth, null, null, null);
	}

	public static BoxStyle New(
		final String stroke,
		final Integer strokeWidth,
		final Integer radiusX,
		final Integer radiusY)
	{
		return new Default(stroke, strokeWidth, radiusX, radiusY, null);
	}

	public static BoxStyle New(
		final String stroke,
		final Integer strokeWidth,
		final Integer radiusX,
		final Integer radiusY,
		final Gradient gradient)
	{
		return new Default(stroke, strokeWidth, radiusX, radiusY, gradient);
	}

	public static class Default implements BoxStyle
	{
		private final String   stroke;
		private final Integer  strokeWidth;
		private final Integer  radiusX;
		private final Integer  radiusY;
		private final Gradient gradient;

		Default(
			final String stroke,
			final Integer strokeWidth,
			final Integer radiusX,
			final Integer radiusY,
			final Gradient gradient)
		{
			super();

			this.stroke      = stroke;
			this.strokeWidth = strokeWidth;
			this.radiusX     = radiusX;
			this.radiusY     = radiusY;
			this.gradient    = gradient;
		}

		@Override
		public String stroke()
		{
			return this.stroke;
		}

		@Override
		public Integer strokeWidth()
		{
			return this.strokeWidth;
		}

		@Override
		public Integer radiusX()
		{
			return this.radiusX;
		}

		@Override
		public Integer radiusY()
		{
			return this.radiusY;
		}

		@Override
		public Gradient gradient()
		{
			return this.gradient;
		}

		@Override
		public String js()
		{
			final ObjectHelper obj = new ObjectHelper();
			obj.putIfNotNull("stroke", this.stroke);
			obj.putIfNotNull("strokeWidth", this.strokeWidth);
			obj.putIfNotNull("rx", this.radiusX);
			obj.putIfNotNull("ry", this.radiusY);
			obj.putIfNotNull("gradient", this.gradient);
			return obj.js();
		}
	}
}
