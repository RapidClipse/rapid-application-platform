/*
 * Copyright (C) 2013-2021 by XDEV Software, All Rights Reserved.
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
package com.rapidclipse.framework.server.charts.scatter;

import com.rapidclipse.framework.server.charts.AbstractChart;
import com.rapidclipse.framework.server.charts.AllowsIFrame;
import com.rapidclipse.framework.server.charts.Axis;
import com.rapidclipse.framework.server.charts.ChartModel;
import com.rapidclipse.framework.server.charts.Column;
import com.rapidclipse.framework.server.charts.Column.Type;
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
import com.rapidclipse.framework.server.charts.HasSeries;
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
public class ScatterChart extends AbstractChart
	implements HasAggregationTarget, HasAnimation, HasAnnotations, HasAxisTitlesPosition, HasBackground,
	HasChartArea, HasColors, HasCrosshair, HasCurveType, HasDataOpacity, HasInteractivity, HasExplorer,
	HasFont, AllowsIFrame, HasHAxis, HasChartSize, HasLegend, HasLineWidth, HasOrientation, HasPoints,
	HasSelectionMode, HasTheme, HasTitlePosition, HasTooltip, HasTrendlines, HasVAxis, DiffChart,
	HasSeries<ScatterSeries>
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

	@Override
	public void showSampleData()
	{
		initDefaultColumnsContinuous("Hours Studied", Type.NUMBER, Column.New(Type.NUMBER, "Final"))
			.addRow(0, 67).addRow(1, 88).addRow(2, 77)
			.addRow(3, 93).addRow(4, 85).addRow(5, 91)
			.addRow(6, 71).addRow(7, 78).addRow(8, 93)
			.addRow(9, 80).addRow(10, 82).addRow(0, 75)
			.addRow(5, 80).addRow(3, 90).addRow(1, 72)
			.addRow(5, 75).addRow(6, 68).addRow(7, 98)
			.addRow(3, 82).addRow(9, 94).addRow(2, 79)
			.addRow(2, 95).addRow(2, 86).addRow(3, 67)
			.addRow(4, 60).addRow(2, 80).addRow(6, 92)
			.addRow(2, 81).addRow(8, 79).addRow(9, 83)
			.addRow(3, 75).addRow(1, 80).addRow(3, 71)
			.addRow(3, 89).addRow(4, 92).addRow(5, 85)
			.addRow(6, 92).addRow(7, 78).addRow(6, 95)
			.addRow(3, 81).addRow(0, 64).addRow(4, 85)
			.addRow(2, 83).addRow(3, 96).addRow(4, 77)
			.addRow(5, 89).addRow(4, 89).addRow(7, 84)
			.addRow(4, 92).addRow(9, 98);

		setHAxis(Axis.New("Hours Studied"));
		setVAxis(Axis.New("Grade"));
		setTitle("Students' Final Grades");
	}

	@Override
	public Number getLineWidth()
	{
		return properties().get("lineWidth", 0);
	}
	
	@Override
	public Number getPointSize()
	{
		return properties().get("pointSize", 7);
	}
}
