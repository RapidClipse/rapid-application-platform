
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
	public Number minValue();

	public Number maxValue();

	public List<Number> values();

	public List<String> colors();

	public static Builder Builder()
	{
		return new Builder.Default();
	}

	public static interface Builder
	{
		public Builder minValue(Number minValue);

		public Builder maxValue(Number maxValue);

		public Builder values(List<Number> values);

		public Builder colors(List<String> colors);

		public ColorAxis build();

		public static class Default implements Builder
		{
			private Number       minValue;
			private Number       maxValue;
			private List<Number> values;
			private List<String> colors;

			Default()
			{
				super();
			}

			@Override
			public Builder minValue(final Number minValue)
			{
				this.minValue = minValue;
				return this;
			}

			@Override
			public Builder maxValue(final Number maxValue)
			{
				this.maxValue = maxValue;
				return this;
			}

			@Override
			public Builder values(final List<Number> values)
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
		private final Number       minValue;
		private final Number       maxValue;
		private final List<Number> values;
		private final List<String> colors;

		Default(
			final Number minValue,
			final Number maxValue,
			final List<Number> values,
			final List<String> colors)
		{
			super();

			this.minValue = minValue;
			this.maxValue = maxValue;
			this.values   = values;
			this.colors   = colors;
		}

		@Override
		public Number minValue()
		{
			return this.minValue;
		}

		@Override
		public Number maxValue()
		{
			return this.maxValue;
		}

		@Override
		public List<Number> values()
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
