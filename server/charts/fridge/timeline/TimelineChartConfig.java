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
package com.rapidclipse.framework.server.charts.timeline;

import java.io.Serializable;
import java.util.HashMap;


/**
 *
 * @author XDEV Software
 * @since 10.02.00
 */
public class TimelineChartConfig implements Serializable
{
	private TimelineOptions timeline                  = new TimelineOptions();
	private boolean         avoidOverlappingGridLines = true;

	public HashMap<String, Object> getOptions()
	{
		final HashMap<String, Object> options = new HashMap<>();
		options.put("timeline", this.timeline);
		options.put("avoidOverlappingGridLines", this.avoidOverlappingGridLines);
		return options;
	}
	
	public TimelineOptions getTimeline()
	{
		return this.timeline;
	}
	
	/**
	 * An 'TimelineOptions' object that specifies the default Timeline-Chart style.
	 * <br>
	 *
	 * @param timeline
	 */
	public void setTimeline(final TimelineOptions timeline)
	{
		this.timeline = timeline;
	}
	
	/**
	 * @return the avoidOverlappingGridLines
	 */
	public boolean isAvoidOverlappingGridLines()
	{
		return this.avoidOverlappingGridLines;
	}
	
	/**
	 * Whether display elements (e.g., the bars in a timeline) should obscure grid
	 * lines. If false, grid lines may be covered completely by display elements. If
	 * true, display elements may be altered to keep grid lines visible. <br>
	 *
	 * @param avoidOverlappingGridLines
	 */
	public void setAvoidOverlappingGridLines(final boolean avoidOverlappingGridLines)
	{
		this.avoidOverlappingGridLines = avoidOverlappingGridLines;
	}

}
