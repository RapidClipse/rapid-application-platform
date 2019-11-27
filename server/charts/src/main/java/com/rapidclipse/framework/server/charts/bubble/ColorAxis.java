
package com.rapidclipse.framework.server.charts.bubble;

import java.io.Serializable;
import java.util.List;

import com.rapidclipse.framework.server.charts.TextStyle;
import com.rapidclipse.framework.server.util.JavaScriptable;

import elemental.json.Json;


/**
 * @author XDEV Software
 *
 */
public interface ColorAxis extends Serializable, JavaScriptable
{
	public static enum LegendPosition implements JavaScriptable
	{
		TOP("top"),
		BOTTOM("bottom"),
		IN("in"),
		NONE("none");

		private final String js;

		private LegendPosition(final String js)
		{
			this.js = Json.create(js).toJson();
		}

		@Override
		public String js()
		{
			return this.js;
		}
	}

	public Double minValue();

	public Double maxValue();

	public List<Double> values();

	public List<String> colors();

	public LegendPosition legendPosition();

	public TextStyle legendTextStyle();

	public String legendNumberFormat();

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

		public Builder legendPosition(LegendPosition legendPosition);

		public Builder legendTextStyle(TextStyle legendTextStyle);

		public Builder legendNumberFormat(String legendNumberFormat);

		public ColorAxis build();

		public static class Default implements Builder
		{
			private Double         minValue;
			private Double         maxValue;
			private List<Double>   values;
			private List<String>   colors;
			private LegendPosition legendPosition;
			private TextStyle      legendTextStyle;
			private String         legendNumberFormat;

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
			public Builder legendPosition(final LegendPosition legendPosition)
			{
				this.legendPosition = legendPosition;
				return this;
			}

			@Override
			public Builder legendTextStyle(final TextStyle legendTextStyle)
			{
				this.legendTextStyle = legendTextStyle;
				return this;
			}

			@Override
			public Builder legendNumberFormat(final String legendNumberFormat)
			{
				this.legendNumberFormat = legendNumberFormat;
				return this;
			}

			@Override
			public ColorAxis build()
			{
				return new ColorAxis.Default(this.minValue, this.maxValue, this.values, this.colors,
					this.legendPosition, this.legendTextStyle, this.legendNumberFormat);
			}
			
		}
		
	}

	public static class Default implements ColorAxis
	{
		private final Double         minValue;
		private final Double         maxValue;
		private final List<Double>   values;
		private final List<String>   colors;
		private final LegendPosition legendPosition;
		private final TextStyle      legendTextStyle;
		private final String         legendNumberFormat;

		Default(
			final Double minValue,
			final Double maxValue,
			final List<Double> values,
			final List<String> colors,
			final LegendPosition legendPosition,
			final TextStyle legendTextStyle,
			final String legendNumberFormat)
		{
			super();

			this.minValue           = minValue;
			this.maxValue           = maxValue;
			this.values             = values;
			this.colors             = colors;
			this.legendPosition     = legendPosition;
			this.legendTextStyle    = legendTextStyle;
			this.legendNumberFormat = legendNumberFormat;
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
		public LegendPosition legendPosition()
		{
			return this.legendPosition;
		}

		@Override
		public TextStyle legendTextStyle()
		{
			return this.legendTextStyle;
		}

		@Override
		public String legendNumberFormat()
		{
			return this.legendNumberFormat;
		}

		@Override
		public String js()
		{
			final ObjectHelper obj = new ObjectHelper();
			obj.putIfNotNull("minValue", this.minValue);
			obj.putIfNotNull("maxValue", this.maxValue);
			obj.putIfNotNull("values", new ArrayHelper().addAllNumbers(this.values));
			obj.putIfNotNull("colors", new ArrayHelper().addAllStrings(this.colors));
			obj.putIfNotNull("legend", new ObjectHelper()
				.putIfNotNull("position", this.legendPosition)
				.putIfNotNull("textStyle", this.legendTextStyle)
				.putIfNotNull("numberFormat", this.legendNumberFormat));
			return obj.js();
		}

	}

}
