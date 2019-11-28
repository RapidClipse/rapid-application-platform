
package com.rapidclipse.framework.server.charts;

import java.io.Serializable;

import com.rapidclipse.framework.server.util.JavaScriptable;


/**
 * @author XDEV Software
 *
 */
public interface SizeAxis extends Serializable, JavaScriptable
{
	public Double minValue();
	
	public Double maxValue();
	
	public Double minSize();
	
	public Double maxSize();
	
	public static Builder Builder()
	{
		return new Builder.Default();
	}
	
	public static interface Builder
	{
		public Builder minValue(Double minValue);
		
		public Builder maxValue(Double maxValue);
		
		public Builder minSize(Double minSize);
		
		public Builder maxSize(Double maxSize);
		
		public SizeAxis build();
		
		public static class Default implements Builder
		{
			private Double minValue;
			private Double maxValue;
			private Double minSize;
			private Double maxSize;
			
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
			public Builder minSize(final Double minSize)
			{
				this.minSize = minSize;
				return this;
			}
			
			@Override
			public Builder maxSize(final Double maxSize)
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
		private final Double minValue;
		private final Double maxValue;
		private final Double minSize;
		private final Double maxSize;
		
		Default(final Double minValue, final Double maxValue, final Double minSize, final Double maxSize)
		{
			super();
			
			this.minValue = minValue;
			this.maxValue = maxValue;
			this.minSize  = minSize;
			this.maxSize  = maxSize;
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
		public Double minSize()
		{
			return this.minSize;
		}
		
		@Override
		public Double maxSize()
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
