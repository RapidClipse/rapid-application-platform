/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
 */
package com.rapidclipse.framework.server.charts;

/**
 * @author XDEV Software
 * @since 10.02.00
 *
 */
public interface HasIntervals extends Chart
{
	public default void addInterval(final String column, final Interval interval)
	{
		properties().putIndexed("interval", column, interval);
	}
	
	public default Interval removeInterval(final String column)
	{
		return properties().removeIndexed("interval", column);
	}
	
	public default void removeAllIntervals()
	{
		properties().removeAllIndexed("interval");
	}
	
	public default void setInterval(final Interval interval)
	{
		properties().put("intervals", interval);
	}

	public default Interval getInterval()
	{
		return properties().get("intervals", null);
	}
}
