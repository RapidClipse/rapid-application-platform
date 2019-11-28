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

package com.rapidclipse.framework.server.charts.candlestick;

import com.rapidclipse.framework.server.charts.AllowsIFrame;
import com.rapidclipse.framework.server.charts.ChartBase;
import com.rapidclipse.framework.server.charts.ChartModel;
import com.rapidclipse.framework.server.charts.Column;
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
import com.rapidclipse.framework.server.charts.HasTitlePosition;
import com.rapidclipse.framework.server.charts.HasVAxes;
import com.rapidclipse.framework.server.charts.HasVAxis;
import com.vaadin.flow.component.Tag;


/**
 *
 * @author XDEV Software
 * @since 10.02.00
 */
@Tag("candlestick-chart")
public class CandlestickChart extends ChartBase
	implements HasAggregationTarget, HasAnimation, HasAxisTitlesPosition, HasBackground, HasBar, HasCandlestick,
	HasChartArea, HasInteractivity, HasFocusTarget, AllowsIFrame, HasHAxis, HasLegend, HasCategories, HasOrientation,
	HasSelectionMode, HasTitlePosition, HasVAxis, HasVAxes, HasChartSize
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
		return getModel().removeAll()
			.addColumn(Column.New(xAxisType, xAxisColumn))
			.addColumn(Column.New(Column.Type.NUMBER, minValueColumn))
			.addColumn(Column.New(Column.Type.NUMBER, initialValueColumn))
			.addColumn(Column.New(Column.Type.NUMBER, finalValueColumn))
			.addColumn(Column.New(Column.Type.NUMBER, maxValueColumn));
	}

	public void addSeries(final int rowIndex, final Series series)
	{
		properties().putIndexed("series", rowIndex, series);
	}

	public Series removeSeries(final int rowIndex)
	{
		return properties().removeIndexed("series", rowIndex);
	}

	public void removeAllSeries()
	{
		properties().removeAllIndexed("series");
	}
}
