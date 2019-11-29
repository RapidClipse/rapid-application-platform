
package com.rapidclipse.framework.server.charts;

import java.io.Serializable;

import com.rapidclipse.framework.server.util.JavaScriptable;


/**
 * @author XDEV Software
 *
 */
public interface SizeAxis extends Serializable, JavaScriptable
{
	public Number minValue();

	public Number maxValue();

	public Number minSize();

	public Number maxSize();

	public static Builder Builder()
	{
		return new Builder.Default();
	}

	public static interface Builder
	{
		public Builder minValue(Number minValue);

		public Builder maxValue(Number maxValue);

		public Builder minSize(Number minSize);

		public Builder maxSize(Number maxSize);

		public SizeAxis build();

		public static class Default implements Builder
		{
			private Number minValue;
			private Number maxValue;
			private Number minSize;
			private Number maxSize;

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
			public Builder minSize(final Number minSize)
			{
				this.minSize = minSize;
				return this;
			}

			@Override
			public Builder maxSize(final Number maxSize)
			{
				this.maxSize = maxSize;
				return this;
			}

			@Override
			public SizeAxis build()
			{
				return new SizeAxis.Default(this.minValue, this.maxValue, this.minSize, this.maxSize);
			}

		}

	}

	public static class Default implements SizeAxis
	{
		private final Number minValue;
		private final Number maxValue;
		private final Number minSize;
		private final Number maxSize;

		Default(final Number minValue, final Number maxValue, final Number minSize, final Number maxSize)
		{
			super();

			this.minValue = minValue;
			this.maxValue = maxValue;
			this.minSize  = minSize;
			this.maxSize  = maxSize;
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
		public Number minSize()
		{
			return this.minSize;
		}

		@Override
		public Number maxSize()
		{
			return this.maxSize;
		}

		@Override
		public String js()
		{
			final ObjectHelper obj = new ObjectHelper();
			obj.putIfNotNull("minValue", this.minValue);
			obj.putIfNotNull("maxValue", this.maxValue);
			obj.putIfNotNull("minSize", this.minSize);
			obj.putIfNotNull("maxSize", this.maxSize);
			return obj.js();
		}

	}

}
