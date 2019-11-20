/*
 * Copyright (C) 2013-2019 by XDEV Software, All Rights Reserved.
 *
 * This file is part of the RapidClipse Application Platform (RAP).
 *
 * RAP is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * RAP is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with RAP. If not, see <http://www.gnu.org/licenses/>.
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *
 * Contributors:
 *     XDEV Software Corp. - initial API and implementation
 */

package com.rapidclipse.framework.server.charts.histogram;

/**
 *
 * @author XDEV Software
 * @since 10.02.00
 */
public class Histogram
{
	private int     bucketSize;
	private boolean hideBucketItems;
	private int     lastBucketPercentile;
	private int     minValue;
	private int     maxValue;
	
	public int getBucketSize()
	{
		return this.bucketSize;
	}
	
	/**
	 * Hardcode the size of each histogram bar, rather than letting it be determined algorithmically.
	 *
	 * @param bucketSize
	 */
	public void setBucketSize(final int bucketSize)
	{
		this.bucketSize = bucketSize;
	}
	
	public boolean isHideBucketItems()
	{
		return this.hideBucketItems;
	}

	/**
	 * Omit the thin divisions between the blocks of the histogram, making it into a series of solid bars.
	 *
	 * @param hideBucketItems
	 */
	public void setHideBucketItems(final boolean hideBucketItems)
	{
		this.hideBucketItems = hideBucketItems;
	}
	
	public int getLastBucketPercentile()
	{
		return this.lastBucketPercentile;
	}
	
	/**
	 * When calculating the histogram's bucket size, ignore the top and bottom lastBucketPercentile percent. The values
	 * are still included in the histogram, but do not affect bucketing.
	 *
	 * @param lastBucketPercentile
	 */
	public void setLastBucketPercentile(final int lastBucketPercentile)
	{
		this.lastBucketPercentile = lastBucketPercentile;
	}
	
	public int getMinValue()
	{
		return this.minValue;
	}

	/**
	 *
	 * Expand the range of buckets to include this value.
	 *
	 * @param minValue
	 */
	public void setMinValue(final int minValue)
	{
		this.minValue = minValue;
	}
	
	public int getMaxValue()
	{
		return this.maxValue;
	}
	
	/**
	 *
	 * Expand the range of buckets to include this value.
	 *
	 * @param maxValue
	 */
	public void setMaxValue(final int maxValue)
	{
		this.maxValue = maxValue;
	}
	
	@Override
	public String toString()
	{
		final StringBuilder str = new StringBuilder();
		str.append("{");
		str.append("bucketSize: " + this.bucketSize + ", ");
		str.append("lastBucketPercentile: " + this.lastBucketPercentile + ", ");
		str.append("minValue: " + this.minValue + ", ");
		str.append("maxValue: " + this.maxValue + ", ");
		str.append("hideBucketItems: " + this.hideBucketItems + " ");
		str.append("}");

		return str.toString();
	}
}
