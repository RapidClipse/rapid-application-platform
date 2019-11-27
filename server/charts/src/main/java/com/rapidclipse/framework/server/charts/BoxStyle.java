
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

	public Double strokeWidth();

	public Double radiusX();

	public Double radiusY();

	public Gradient gradient();

	public static Builder Builder()
	{
		return new Builder.Default();
	}

	public static interface Builder
	{
		public Builder stroke(String stroke);

		public Builder strokeWidth(Double strokeWidth);

		public Builder radiusX(Double radiusX);

		public Builder radiusY(Double radiusY);

		public Builder gradient(Gradient gradient);

		public BoxStyle build();

		public static class Default implements Builder
		{
			private String   stroke;
			private Double   strokeWidth;
			private Double   radiusX;
			private Double   radiusY;
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
			public Builder strokeWidth(final Double strokeWidth)
			{
				this.strokeWidth = strokeWidth;
				return this;
			}

			@Override
			public Builder radiusX(final Double radiusX)
			{
				this.radiusX = radiusX;
				return this;
			}

			@Override
			public Builder radiusY(final Double radiusY)
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
		final Double strokeWidth)
	{
		return new Default(stroke, strokeWidth, null, null, null);
	}

	public static BoxStyle New(
		final String stroke,
		final Double strokeWidth,
		final Double radiusX,
		final Double radiusY)
	{
		return new Default(stroke, strokeWidth, radiusX, radiusY, null);
	}

	public static BoxStyle New(
		final String stroke,
		final Double strokeWidth,
		final Double radiusX,
		final Double radiusY,
		final Gradient gradient)
	{
		return new Default(stroke, strokeWidth, radiusX, radiusY, gradient);
	}

	public static class Default implements BoxStyle
	{
		private final String   stroke;
		private final Double   strokeWidth;
		private final Double   radiusX;
		private final Double   radiusY;
		private final Gradient gradient;

		Default(
			final String stroke,
			final Double strokeWidth,
			final Double radiusX,
			final Double radiusY,
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
		public Double strokeWidth()
		{
			return this.strokeWidth;
		}

		@Override
		public Double radiusX()
		{
			return this.radiusX;
		}

		@Override
		public Double radiusY()
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
