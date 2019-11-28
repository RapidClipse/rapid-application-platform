
package com.rapidclipse.framework.server.charts.histogram;

import java.io.Serializable;

import com.rapidclipse.framework.server.util.JavaScriptable;


/**
 * @author XDEV Software
 *
 */
public interface Histogram extends Serializable, JavaScriptable
{
	public Double bucketSize();
	
	public Boolean hideBucketItems();
	
	public Double lastBucketPercentile();
	
	public Double minValue();
	
	public Double maxValue();
	
	public static Builder Builder()
	{
		return new Builder.Default();
	}
	
	public static interface Builder
	{
		public Builder bucketSize(Double bucketSize);
		
		public Builder hideBucketItems(Boolean hideBucketItems);
		
		public Builder lastBucketPercentile(Double lastBucketPercentile);
		
		public Builder minValue(Double minValue);
		
		public Builder maxValue(Double maxValue);
		
		public Histogram build();
		
		public static class Default implements Builder
		{
			private Double  bucketSize;
			private Boolean hideBucketItems;
			private Double  lastBucketPercentile;
			private Double  minValue;
			private Double  maxValue;
			
			Default()
			{
				super();
			}
			
			@Override
			public Builder bucketSize(final Double bucketSize)
			{
				this.bucketSize = bucketSize;
				return this;
			}
			
			@Override
			public Builder hideBucketItems(final Boolean hideBucketItems)
			{
				this.hideBucketItems = hideBucketItems;
				return this;
			}
			
			@Override
			public Builder lastBucketPercentile(final Double lastBucketPercentile)
			{
				this.lastBucketPercentile = lastBucketPercentile;
				return this;
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
			public Histogram build()
			{
				return new Histogram.Default(this.bucketSize, this.hideBucketItems, this.lastBucketPercentile,
					this.minValue, this.maxValue);
			}
			
		}
		
	}
	
	public static class Default implements Histogram
	{
		private final Double  bucketSize;
		private final Boolean hideBucketItems;
		private final Double  lastBucketPercentile;
		private final Double  minValue;
		private final Double  maxValue;
		
		Default(
			final Double bucketSize,
			final Boolean hideBucketItems,
			final Double lastBucketPercentile,
			final Double minValue,
			final Double maxValue)
		{
			super();
			
			this.bucketSize           = bucketSize;
			this.hideBucketItems      = hideBucketItems;
			this.lastBucketPercentile = lastBucketPercentile;
			this.minValue             = minValue;
			this.maxValue             = maxValue;
		}
		
		@Override
		public Double bucketSize()
		{
			return this.bucketSize;
		}
		
		@Override
		public Boolean hideBucketItems()
		{
			return this.hideBucketItems;
		}
		
		@Override
		public Double lastBucketPercentile()
		{
			return this.lastBucketPercentile;
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
		public String js()
		{
			final ObjectHelper obj = new ObjectHelper();
			obj.putIfNotNull("bucketSize", this.bucketSize);
			obj.putIfNotNull("hideBucketItems", this.hideBucketItems);
			obj.putIfNotNull("lastBucketPercentile", this.lastBucketPercentile);
			obj.putIfNotNull("minValue", this.minValue);
			obj.putIfNotNull("maxValue", this.maxValue);
			return obj.js();
		}
		
	}
	
}
