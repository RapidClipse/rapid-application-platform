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

package com.rapidclipse.framework.server.charts.area;

import com.rapidclipse.framework.server.charts.CanInterpolateNulls;
import com.rapidclipse.framework.server.charts.ChartBase;
import com.rapidclipse.framework.server.charts.ChartModel;
import com.rapidclipse.framework.server.charts.Column;
import com.rapidclipse.framework.server.charts.HasAggregationTarget;
import com.rapidclipse.framework.server.charts.HasAnimation;
import com.rapidclipse.framework.server.charts.HasAnnotations;
import com.rapidclipse.framework.server.charts.HasAreaOpacity;
import com.rapidclipse.framework.server.charts.HasAxisTitlesPosition;
import com.rapidclipse.framework.server.charts.HasBackground;
import com.rapidclipse.framework.server.charts.HasCategories;
import com.rapidclipse.framework.server.charts.HasChartArea;
import com.rapidclipse.framework.server.charts.HasChartSize;
import com.rapidclipse.framework.server.charts.HasColors;
import com.rapidclipse.framework.server.charts.HasCrosshair;
import com.rapidclipse.framework.server.charts.HasDataOpacity;
import com.rapidclipse.framework.server.charts.HasExplorer;
import com.rapidclipse.framework.server.charts.HasFocusTarget;
import com.rapidclipse.framework.server.charts.HasFont;
import com.rapidclipse.framework.server.charts.HasHAxis;
import com.rapidclipse.framework.server.charts.HasInteractivity;
import com.rapidclipse.framework.server.charts.HasLegend;
import com.rapidclipse.framework.server.charts.HasLineDashStyle;
import com.rapidclipse.framework.server.charts.HasOrientation;
import com.rapidclipse.framework.server.charts.HasPoints;
import com.rapidclipse.framework.server.charts.HasSelectionMode;
import com.rapidclipse.framework.server.charts.HasStackMode;
import com.rapidclipse.framework.server.charts.HasTheme;
import com.rapidclipse.framework.server.charts.HasTitlePosition;
import com.rapidclipse.framework.server.charts.HasTooltip;
import com.rapidclipse.framework.server.charts.HasVAxes;
import com.rapidclipse.framework.server.charts.HasVAxis;
import com.vaadin.flow.component.Tag;


/**
 *
 * @author XDEV Software
 * @since 10.02.00
 */
@Tag("area-chart")
public class AreaChart extends ChartBase
	implements HasAggregationTarget, HasAnimation, HasAnnotations, HasAreaOpacity, HasAxisTitlesPosition,
	HasBackground, HasChartArea, HasColors, HasCrosshair, HasDataOpacity, HasInteractivity, HasExplorer,
	HasFocusTarget, HasFont, HasHAxis, HasChartSize, CanInterpolateNulls, HasStackMode, HasLegend, HasLineDashStyle,
	HasOrientation, HasPoints, HasCategories, HasSelectionMode, HasTheme, HasTitlePosition, HasTooltip, HasVAxes,
	HasVAxis
{
	public AreaChart()
	{
		super("AreaChart");
	}

	public ChartModel initDefaultColumnsDiscrete(final String xColumn, final String... valueColumns)
	{
		final ChartModel model = getModel().removeAll()
			.addColumn(Column.New(Column.Type.STRING, xColumn));
		for(final String valueColumn : valueColumns)
		{
			model.addColumn(Column.New(Column.Type.NUMBER, valueColumn));
		}
		return model;
	}

	public ChartModel
		initDefaultColumnsContinuous(final String xColumn, final Column.Type xColumnType, final String... valueColumns)
	{
		validateColumnType(xColumnType, "x column", Column.Type.NUMBER, Column.Type.DATE, Column.Type.DATE_TIME,
			Column.Type.TIME_OF_DAY);

		final ChartModel model = getModel().removeAll()
			.addColumn(Column.New(xColumnType, xColumn));
		for(final String valueColumn : valueColumns)
		{
			model.addColumn(Column.New(Column.Type.NUMBER, valueColumn));
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
