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

import com.rapidclipse.framework.server.charts.AllowsIFrame;
import com.rapidclipse.framework.server.charts.ChartBase;
import com.rapidclipse.framework.server.charts.ChartModel;
import com.rapidclipse.framework.server.charts.Column;
import com.rapidclipse.framework.server.charts.HasChartSize;
import com.rapidclipse.framework.server.charts.HasColors;
import com.rapidclipse.framework.server.charts.HasFont;
import com.rapidclipse.framework.server.charts.HasInteractivity;
import com.rapidclipse.framework.server.charts.HasTooltip;
import com.vaadin.flow.component.Tag;


/**
 *
 * @author XDEV Software
 * @since 10.02.00
 */
@Tag("timeline-chart")
public class TimelineChart extends ChartBase
	implements HasColors, HasInteractivity, HasFont, AllowsIFrame, HasChartSize, HasTooltip
{
	public TimelineChart()
	{
		super("Timeline", "timeline");
	}
	
	public ChartModel
		initDefaultColumns(
			final String rowLabelColumn,
			final String barLabelColumn,
			final String toolTipColumn,
			final String startColumn,
			final Column.Type startColumnType,
			final String endColumn,
			final Column.Type endColumnType)
	{
		return getModel().removeAll()
			.addColumn(Column.New(Column.Type.STRING, rowLabelColumn))
			.addColumn(Column.New(Column.Type.STRING, barLabelColumn))
			.addColumn(Column.New(Column.Type.STRING, toolTipColumn))
			.addColumn(Column.New(startColumnType, startColumn))
			.addColumn(Column.New(endColumnType, endColumn));
	}

	public Boolean getAvoidOverlappingGridLines()
	{
		return properties().get("avoidOverlappingGridLines");
	}

	public void setAvoidOverlappingGridLines(final Boolean avoidOverlappingGridLines)
	{
		properties().put("avoidOverlappingGridLines", avoidOverlappingGridLines);
	}

	public String getBackgroundColor()
	{
		return properties().get("backgroundColor");
	}

	public void setBackgroundColor(final String backgroundColor)
	{
		properties().put("backgroundColor", backgroundColor);
	}

	public Timeline getTimeline()
	{
		return properties().get("timeline");
	}

	public void setTimeline(final Timeline timeline)
	{
		properties().put("timeline", timeline);
	}
}
