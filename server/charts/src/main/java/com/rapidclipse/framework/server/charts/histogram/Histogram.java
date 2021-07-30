/*
 * Copyright (C) 2013-2021 by XDEV Software, All Rights Reserved.
 *
 * This file is part of the RapidClipse Application Platform (RAP).
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *
 * Contributors:
 *     XDEV Software Corp. - initial API and implementation
 */
package com.rapidclipse.framework.server.charts.histogram;

import java.io.Serializable;

import com.rapidclipse.framework.server.charts.JavaScriptable;


/**
 * @author XDEV Software
 * @since 10.02.00
 *
 */
public interface Histogram extends Serializable, JavaScriptable
{
	public Number bucketSize();
	
	public Boolean hideBucketItems();
	
	public Number lastBucketPercentile();
	
	public Number minValue();
	
	public Number maxValue();
	
	public static Builder Builder()
	{
		return new Builder.Default();
	}
	
	public static interface Builder
	{
		public Builder bucketSize(Number bucketSize);
		
		public Builder hideBucketItems(Boolean hideBucketItems);
		
		public Builder lastBucketPercentile(Number lastBucketPercentile);
		
		public Builder minValue(Number minValue);
		
		public Builder maxValue(Number maxValue);
		
		public Histogram build();
		
		public static class Default implements Builder
		{
			private Number  bucketSize;
			private Boolean hideBucketItems;
			private Number  lastBucketPercentile;
			private Number  minValue;
			private Number  maxValue;
			
			Default()
			{
				super();
			}
			
			@Override
			public Builder bucketSize(final Number bucketSize)
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
			public Builder lastBucketPercentile(final Number lastBucketPercentile)
			{
				this.lastBucketPercentile = lastBucketPercentile;
				return this;
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
			public Histogram build()
			{
				return new Histogram.Default(this.bucketSize, this.hideBucketItems, this.lastBucketPercentile,
					this.minValue, this.maxValue);
			}
			
		}
		
	}
	
	public static class Default implements Histogram
	{
		private final Number  bucketSize;
		private final Boolean hideBucketItems;
		private final Number  lastBucketPercentile;
		private final Number  minValue;
		private final Number  maxValue;
		
		Default(
			final Number bucketSize,
			final Boolean hideBucketItems,
			final Number lastBucketPercentile,
			final Number minValue,
			final Number maxValue)
		{
			super();
			
			this.bucketSize           = bucketSize;
			this.hideBucketItems      = hideBucketItems;
			this.lastBucketPercentile = lastBucketPercentile;
			this.minValue             = minValue;
			this.maxValue             = maxValue;
		}
		
		@Override
		public Number bucketSize()
		{
			return this.bucketSize;
		}
		
		@Override
		public Boolean hideBucketItems()
		{
			return this.hideBucketItems;
		}
		
		@Override
		public Number lastBucketPercentile()
		{
			return this.lastBucketPercentile;
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
