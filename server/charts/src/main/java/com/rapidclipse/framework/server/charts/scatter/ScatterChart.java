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

package com.rapidclipse.framework.server.charts.scatter;

import com.rapidclipse.framework.server.charts.AllowsIFrame;
import com.rapidclipse.framework.server.charts.ChartBase;
import com.rapidclipse.framework.server.charts.ChartModel;
import com.rapidclipse.framework.server.charts.Column;
import com.rapidclipse.framework.server.charts.DiffChart;
import com.rapidclipse.framework.server.charts.HasAggregationTarget;
import com.rapidclipse.framework.server.charts.HasAnimation;
import com.rapidclipse.framework.server.charts.HasAnnotations;
import com.rapidclipse.framework.server.charts.HasAxisTitlesPosition;
import com.rapidclipse.framework.server.charts.HasBackground;
import com.rapidclipse.framework.server.charts.HasChartArea;
import com.rapidclipse.framework.server.charts.HasChartSize;
import com.rapidclipse.framework.server.charts.HasColors;
import com.rapidclipse.framework.server.charts.HasCrosshair;
import com.rapidclipse.framework.server.charts.HasCurveType;
import com.rapidclipse.framework.server.charts.HasDataOpacity;
import com.rapidclipse.framework.server.charts.HasExplorer;
import com.rapidclipse.framework.server.charts.HasFont;
import com.rapidclipse.framework.server.charts.HasHAxis;
import com.rapidclipse.framework.server.charts.HasInteractivity;
import com.rapidclipse.framework.server.charts.HasLegend;
import com.rapidclipse.framework.server.charts.HasLineWidth;
import com.rapidclipse.framework.server.charts.HasOrientation;
import com.rapidclipse.framework.server.charts.HasPoints;
import com.rapidclipse.framework.server.charts.HasSelectionMode;
import com.rapidclipse.framework.server.charts.HasTheme;
import com.rapidclipse.framework.server.charts.HasTitlePosition;
import com.rapidclipse.framework.server.charts.HasTooltip;
import com.rapidclipse.framework.server.charts.HasTrendlines;
import com.rapidclipse.framework.server.charts.HasVAxis;
import com.vaadin.flow.component.Tag;


/**
 *
 * @author XDEV Software
 * @since 10.02.00
 */
@Tag("scatter-chart")
public class ScatterChart extends ChartBase
	implements HasAggregationTarget, HasAnimation, HasAnnotations, HasAxisTitlesPosition, HasBackground,
	HasChartArea, HasColors, HasCrosshair, HasCurveType, HasDataOpacity, HasInteractivity, HasExplorer,
	HasFont, AllowsIFrame, HasHAxis, HasChartSize, HasLegend, HasLineWidth, HasOrientation, HasPoints,
	HasSelectionMode, HasTheme, HasTitlePosition, HasTooltip, HasTrendlines, HasVAxis, DiffChart
{
	public ScatterChart()
	{
		super("ScatterChart");
	}

	public ScatterChart(final ChartModel before, final ChartModel after)
	{
		this();

		setModel(before, after);
	}
	
	@Override
	public void setModel(final ChartModel before, final ChartModel after)
	{
		super.setModel(before, after);
	}

	public ChartModel
		initDefaultColumnsContinuous(
			final String xValuesColumn,
			final Column.Type xValuesColumnType,
			final Column... valueColumns)
	{
		validateColumnType(xValuesColumnType, "x values column", Column.Type.STRING, Column.Type.NUMBER,
			Column.Type.DATE,
			Column.Type.DATE_TIME, Column.Type.TIME_OF_DAY);

		final ChartModel model = getModel().removeAll()
			.addColumn(Column.New(xValuesColumnType, xValuesColumn));
		for(final Column valueColumn : valueColumns)
		{
			validateColumnType(valueColumn.type(), valueColumn.label(), Column.Type.STRING, Column.Type.NUMBER,
				Column.Type.DATE,
				Column.Type.DATE_TIME, Column.Type.TIME_OF_DAY);

			model.addColumn(valueColumn);
		}
		return model;
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
