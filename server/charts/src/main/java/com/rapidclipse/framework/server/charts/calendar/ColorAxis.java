
package com.rapidclipse.framework.server.charts.calendar;

import java.io.Serializable;
import java.util.List;

import com.rapidclipse.framework.server.util.JavaScriptable;


/**
 * @author XDEV Software
 *
 */
public interface ColorAxis extends Serializable, JavaScriptable
{
	public Double minValue();

	public Double maxValue();

	public List<Double> values();

	public List<String> colors();

	public static Builder Builder()
	{
		return new Builder.Default();
	}

	public static interface Builder
	{
		public Builder minValue(Double minValue);

		public Builder maxValue(Double maxValue);

		public Builder values(List<Double> values);

		public Builder colors(List<String> colors);

		public ColorAxis build();

		public static class Default implements Builder
		{
			private Double       minValue;
			private Double       maxValue;
			private List<Double> values;
			private List<String> colors;

			Default()
			{
				super();
			}

			@Override
			public Builder minValue(final Double minValue)
			{
				this.minValue = minValue;
				return this;
			}

			@Override
			public Builder maxValue(final Double maxValue)
			{
				this.maxValue = maxValue;
				return this;
			}

			@Override
			public Builder values(final List<Double> values)
			{
				this.values = values;
				return this;
			}

			@Override
			public Builder colors(final List<String> colors)
			{
				this.colors = colors;
				return this;
			}

			@Override
			public ColorAxis build()
			{
				return new ColorAxis.Default(this.minValue, this.maxValue, this.values, this.colors);
			}
			
		}
		
	}

	public static class Default implements ColorAxis
	{
		private final Double       minValue;
		private final Double       maxValue;
		private final List<Double> values;
		private final List<String> colors;

		Default(
			final Double minValue,
			final Double maxValue,
			final List<Double> values,
			final List<String> colors)
		{
			super();

			this.minValue = minValue;
			this.maxValue = maxValue;
			this.values   = values;
			this.colors   = colors;
		}

		@Override
		public Double minValue()
		{
			return this.minValue;
		}

		@Override
		public Double maxValue()
		{
			return this.maxValue;
		}

		@Override
		public List<Double> values()
		{
			return this.values;
		}

		@Override
		public List<String> colors()
		{
			return this.colors;
		}

		@Override
		public String js()
		{
			final ObjectHelper obj = new ObjectHelper();
			obj.putIfNotNull("minValue", this.minValue);
			obj.putIfNotNull("maxValue", this.maxValue);
			obj.putIfNotNull("values", new ArrayHelper().addAllNumbers(this.values));
			obj.putIfNotNull("colors", new ArrayHelper().addAllStrings(this.colors));
			return obj.js();
		}

	}

}
