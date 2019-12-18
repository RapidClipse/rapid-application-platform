/*
 * Copyright (C) 2013-2019 by XDEV Software, All Rights Reserved.
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
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *
 * Contributors:
 *     XDEV Software Corp. - initial API and implementation
 */

package com.rapidclipse.framework.server.charts.combo;

import com.rapidclipse.framework.server.charts.AbstractChart;
import com.rapidclipse.framework.server.charts.AllowsIFrame;
import com.rapidclipse.framework.server.charts.Axis;
import com.rapidclipse.framework.server.charts.CanInterpolateNulls;
import com.rapidclipse.framework.server.charts.ChartModel;
import com.rapidclipse.framework.server.charts.Column;
import com.rapidclipse.framework.server.charts.HasAggregationTarget;
import com.rapidclipse.framework.server.charts.HasAnimation;
import com.rapidclipse.framework.server.charts.HasAnnotations;
import com.rapidclipse.framework.server.charts.HasAreaOpacity;
import com.rapidclipse.framework.server.charts.HasAxisTitlesPosition;
import com.rapidclipse.framework.server.charts.HasBackground;
import com.rapidclipse.framework.server.charts.HasBar;
import com.rapidclipse.framework.server.charts.HasCandlestick;
import com.rapidclipse.framework.server.charts.HasCategories;
import com.rapidclipse.framework.server.charts.HasChartArea;
import com.rapidclipse.framework.server.charts.HasChartSize;
import com.rapidclipse.framework.server.charts.HasColors;
import com.rapidclipse.framework.server.charts.HasCrosshair;
import com.rapidclipse.framework.server.charts.HasCurveType;
import com.rapidclipse.framework.server.charts.HasDataOpacity;
import com.rapidclipse.framework.server.charts.HasFocusTarget;
import com.rapidclipse.framework.server.charts.HasFont;
import com.rapidclipse.framework.server.charts.HasHAxis;
import com.rapidclipse.framework.server.charts.HasInteractivity;
import com.rapidclipse.framework.server.charts.HasIntervals;
import com.rapidclipse.framework.server.charts.HasLegend;
import com.rapidclipse.framework.server.charts.HasLineDashStyle;
import com.rapidclipse.framework.server.charts.HasOrientation;
import com.rapidclipse.framework.server.charts.HasPoints;
import com.rapidclipse.framework.server.charts.HasSelectionMode;
import com.rapidclipse.framework.server.charts.HasSeries;
import com.rapidclipse.framework.server.charts.HasStackMode;
import com.rapidclipse.framework.server.charts.HasTheme;
import com.rapidclipse.framework.server.charts.HasTitlePosition;
import com.rapidclipse.framework.server.charts.HasTooltip;
import com.rapidclipse.framework.server.charts.HasVAxes;
import com.vaadin.flow.component.Tag;


/**
 * @author XDEV Software
 * @since 10.02.00
 *
 */
@Tag("combo-chart")
public class ComboChart extends AbstractChart
	implements HasAggregationTarget, HasAnimation, HasAnnotations, HasAreaOpacity, HasAxisTitlesPosition, HasBackground,
	HasBar, HasCandlestick, HasChartArea, HasColors, HasCrosshair, HasCurveType, HasDataOpacity, HasInteractivity,
	HasFocusTarget, HasFont, AllowsIFrame, HasHAxis, HasChartSize, CanInterpolateNulls, HasStackMode, HasLegend,
	HasLineDashStyle, HasOrientation, HasPoints, HasCategories, HasSelectionMode, HasTheme, HasTitlePosition,
	HasTooltip, HasVAxes, HasIntervals, HasSeries<ComboSeries>
{
	public ComboChart()
	{
		super("ComboChart");
	}
	
	public ChartModel initDefaultColumnsDiscrete(final String xAxisColumn, final String... valueColumns)
	{
		final ChartModel model = getModel().removeAll()
			.addColumn(Column.New(Column.Type.STRING, xAxisColumn));
		for(final String valueColumn : valueColumns)
		{
			model.addColumn(Column.New(Column.Type.NUMBER, valueColumn));
		}
		return model;
	}
	
	public ChartModel
		initDefaultColumnsContinuous(
			final String xAxisColumn,
			final Column.Type xAxisColumnType,
			final String... valueColumns)
	{
		validateColumnType(xAxisColumnType, "axis column", Column.Type.NUMBER, Column.Type.DATE, Column.Type.DATE_TIME,
			Column.Type.TIME_OF_DAY);
		
		final ChartModel model = getModel().removeAll()
			.addColumn(Column.New(xAxisColumnType, xAxisColumn));
		for(final String valueColumn : valueColumns)
		{
			model.addColumn(Column.New(Column.Type.NUMBER, valueColumn));
		}
		return model;
	}
	
	public SeriesType getSeriesType()
	{
		return properties().get("seriesType");
	}
	
	public void setSeriesType(final SeriesType seriesType)
	{
		properties().put("seriesType", seriesType);
	}
	
	@Override
	public void showSampleData()
	{
		initDefaultColumnsDiscrete("Month", "Bolivia", "Ecuador", "Madagascar", "Papua New Guinea", "Rwanda", "Average")
			.addRow("2004/05", 165, 938, 522, 998, 450, 614.6)
			.addRow("2005/06", 135, 1120, 599, 1268, 288, 682)
			.addRow("2006/07", 157, 1167, 587, 807, 397, 623)
			.addRow("2007/08", 139, 1110, 615, 968, 215, 609.4)
			.addRow("2008/09", 136, 691, 629, 1026, 366, 569.6);
		
		setTitle("Monthly Coffee Production by Country");
		setVAxis(Axis.New("Cups"));
		setHAxis(Axis.New("Month"));
		setSeriesType(SeriesType.BARS);
		addSeries(5, ComboSeries.New(SeriesType.LINE));
	}
}
