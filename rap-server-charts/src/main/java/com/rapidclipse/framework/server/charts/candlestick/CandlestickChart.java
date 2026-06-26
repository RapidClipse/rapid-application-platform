/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
 */
package com.rapidclipse.framework.server.charts.candlestick;

import com.rapidclipse.framework.server.charts.AbstractChart;
import com.rapidclipse.framework.server.charts.AllowsIFrame;
import com.rapidclipse.framework.server.charts.ChartModel;
import com.rapidclipse.framework.server.charts.Column;
import com.rapidclipse.framework.server.charts.Column.Type;
import com.rapidclipse.framework.server.charts.HasAggregationTarget;
import com.rapidclipse.framework.server.charts.HasAnimation;
import com.rapidclipse.framework.server.charts.HasAxisTitlesPosition;
import com.rapidclipse.framework.server.charts.HasBackground;
import com.rapidclipse.framework.server.charts.HasBar;
import com.rapidclipse.framework.server.charts.HasCandlestick;
import com.rapidclipse.framework.server.charts.HasCategories;
import com.rapidclipse.framework.server.charts.HasChartArea;
import com.rapidclipse.framework.server.charts.HasChartSize;
import com.rapidclipse.framework.server.charts.HasFocusTarget;
import com.rapidclipse.framework.server.charts.HasHAxis;
import com.rapidclipse.framework.server.charts.HasInteractivity;
import com.rapidclipse.framework.server.charts.HasLegend;
import com.rapidclipse.framework.server.charts.HasOrientation;
import com.rapidclipse.framework.server.charts.HasSelectionMode;
import com.rapidclipse.framework.server.charts.HasSeries;
import com.rapidclipse.framework.server.charts.HasTitlePosition;
import com.rapidclipse.framework.server.charts.HasVAxes;
import com.rapidclipse.framework.server.charts.Legend;
import com.vaadin.flow.component.Tag;


/**
 *
 * @author XDEV Software
 * @since 10.02.00
 */
@Tag("candlestick-chart")
public class CandlestickChart extends AbstractChart
	implements HasAggregationTarget, HasAnimation, HasAxisTitlesPosition, HasBackground, HasBar, HasCandlestick,
	HasChartArea, HasInteractivity, HasFocusTarget, AllowsIFrame, HasHAxis, HasLegend, HasCategories, HasOrientation,
	HasSelectionMode, HasTitlePosition, HasVAxes, HasChartSize, HasSeries<CandlestickSeries>
{
	public CandlestickChart()
	{
		super("CandlestickChart");
	}

	public ChartModel initDefaultColumns(final Column.Type xAxisType)
	{
		return initDefaultColumns("x", xAxisType, "min", "initial", "final", "max");
	}

	public ChartModel initDefaultColumns(
		final String xAxisColumn,
		final Column.Type xAxisType,
		final String minValueColumn,
		final String initialValueColumn,
		final String finalValueColumn,
		final String maxValueColumn)
	{
		validateColumnType(xAxisType, "x axis column", Column.Type.STRING, Column.Type.NUMBER, Column.Type.DATE,
			Column.Type.DATE_TIME, Column.Type.TIME_OF_DAY);

		return getModel().removeAll()
			.addColumn(Column.New(xAxisType, xAxisColumn))
			.addColumn(Column.New(Column.Type.NUMBER, minValueColumn))
			.addColumn(Column.New(Column.Type.NUMBER, initialValueColumn))
			.addColumn(Column.New(Column.Type.NUMBER, finalValueColumn))
			.addColumn(Column.New(Column.Type.NUMBER, maxValueColumn));
	}

	@Override
	public void showSampleData()
	{
		initDefaultColumns(Type.STRING)
			.addRow("Mon", 20, 28, 38, 45)
			.addRow("Tue", 31, 38, 55, 66)
			.addRow("Wed", 50, 55, 77, 80)
			.addRow("Thu", 77, 77, 66, 50)
			.addRow("Fri", 68, 66, 22, 15);

		setLegend(Legend.None());
	}
}
