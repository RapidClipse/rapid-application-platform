/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
 */
package com.rapidclipse.framework.server.charts.timeline;

import java.time.LocalDate;

import com.rapidclipse.framework.server.charts.AbstractChart;
import com.rapidclipse.framework.server.charts.AllowsIFrame;
import com.rapidclipse.framework.server.charts.ChartModel;
import com.rapidclipse.framework.server.charts.Column;
import com.rapidclipse.framework.server.charts.Column.Type;
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
public class TimelineChart extends AbstractChart
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
		validateColumnType(startColumnType, "start column", Column.Type.NUMBER, Column.Type.DATE);
		validateColumnType(endColumnType, "end column", Column.Type.NUMBER, Column.Type.DATE);

		final ChartModel model = getModel().removeAll()
			.addColumn(Column.New(Column.Type.STRING, rowLabelColumn));
		if(barLabelColumn != null)
		{
			model.addColumn(Column.New(Column.Type.STRING, barLabelColumn));
		}
		if(toolTipColumn != null)
		{
			model.addColumn(Column.New(Column.Type.STRING, toolTipColumn));
		}
		return model.addColumn(Column.New(startColumnType, startColumn))
			.addColumn(Column.New(endColumnType, endColumn));
	}

	public boolean getAvoidOverlappingGridLines()
	{
		return properties().get("avoidOverlappingGridLines", true);
	}

	public void setAvoidOverlappingGridLines(final boolean avoidOverlappingGridLines)
	{
		properties().put("avoidOverlappingGridLines", avoidOverlappingGridLines);
	}

	public String getBackgroundColor()
	{
		return properties().get("backgroundColor", "white");
	}

	public void setBackgroundColor(final String backgroundColor)
	{
		properties().put("backgroundColor", backgroundColor);
	}

	public Timeline getTimeline()
	{
		return properties().get("timeline", null);
	}

	public void setTimeline(final Timeline timeline)
	{
		properties().put("timeline", timeline);
	}

	@Override
	public void showSampleData()
	{
		initDefaultColumns("Position", "Name", null, "Start", Type.DATE, "End", Type.DATE)
			.addRow("President", "George Washington", LocalDate.of(1789, 4, 30), LocalDate.of(1797, 3, 4))
			.addRow("President", "John Adams", LocalDate.of(1797, 3, 4), LocalDate.of(1801, 3, 4))
			.addRow("President", "Thomas Jefferson", LocalDate.of(1801, 3, 4), LocalDate.of(1809, 3, 4))
			.addRow("Vice President", "John Adams", LocalDate.of(1789, 4, 21), LocalDate.of(1797, 3, 4))
			.addRow("Vice President", "Thomas Jefferson", LocalDate.of(1797, 3, 4), LocalDate.of(1801, 3, 4))
			.addRow("Vice President", "Aaron Burr", LocalDate.of(1801, 3, 4), LocalDate.of(1805, 3, 4))
			.addRow("Vice President", "George Clinton", LocalDate.of(1805, 3, 4), LocalDate.of(1812, 4, 20))
			.addRow("Secretary of State", "John Jay", LocalDate.of(1789, 9, 25), LocalDate.of(1790, 3, 22))
			.addRow("Secretary of State", "Thomas Jefferson", LocalDate.of(1790, 3, 22), LocalDate.of(1793, 12, 31))
			.addRow("Secretary of State", "Edmund Randolph", LocalDate.of(1794, 1, 2), LocalDate.of(1795, 8, 20))
			.addRow("Secretary of State", "Timothy Pickering", LocalDate.of(1795, 8, 20), LocalDate.of(1800, 5, 12))
			.addRow("Secretary of State", "Charles Lee", LocalDate.of(1800, 5, 13), LocalDate.of(1800, 6, 5))
			.addRow("Secretary of State", "John Marshall", LocalDate.of(1800, 6, 13), LocalDate.of(1801, 3, 4))
			.addRow("Secretary of State", "Levi Lincoln", LocalDate.of(1801, 3, 5), LocalDate.of(1801, 5, 1))
			.addRow("Secretary of State", "James Madison", LocalDate.of(1801, 5, 2), LocalDate.of(1809, 3, 3));;
	}
}
