/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
 */
package com.rapidclipse.framework.server.charts.steppedarea;

import com.rapidclipse.framework.server.charts.AbstractChart;
import com.rapidclipse.framework.server.charts.AllowsIFrame;
import com.rapidclipse.framework.server.charts.Axis;
import com.rapidclipse.framework.server.charts.ChartModel;
import com.rapidclipse.framework.server.charts.Column;
import com.rapidclipse.framework.server.charts.HasAggregationTarget;
import com.rapidclipse.framework.server.charts.HasAnimation;
import com.rapidclipse.framework.server.charts.HasAreaOpacity;
import com.rapidclipse.framework.server.charts.HasAxisTitlesPosition;
import com.rapidclipse.framework.server.charts.HasBackground;
import com.rapidclipse.framework.server.charts.HasCategories;
import com.rapidclipse.framework.server.charts.HasChartArea;
import com.rapidclipse.framework.server.charts.HasChartSize;
import com.rapidclipse.framework.server.charts.HasColors;
import com.rapidclipse.framework.server.charts.HasFocusTarget;
import com.rapidclipse.framework.server.charts.HasFont;
import com.rapidclipse.framework.server.charts.HasHAxis;
import com.rapidclipse.framework.server.charts.HasInteractivity;
import com.rapidclipse.framework.server.charts.HasIntervals;
import com.rapidclipse.framework.server.charts.HasLegend;
import com.rapidclipse.framework.server.charts.HasLineDashStyle;
import com.rapidclipse.framework.server.charts.HasSelectionMode;
import com.rapidclipse.framework.server.charts.HasSeries;
import com.rapidclipse.framework.server.charts.HasStackMode;
import com.rapidclipse.framework.server.charts.HasTheme;
import com.rapidclipse.framework.server.charts.HasTitlePosition;
import com.rapidclipse.framework.server.charts.HasTooltip;
import com.rapidclipse.framework.server.charts.HasVAxes;
import com.rapidclipse.framework.server.charts.StackMode;
import com.vaadin.flow.component.Tag;


/**
 *
 * @author XDEV Software
 * @since 10.02.00
 */
@Tag("steppedarea-chart")
public class SteppedAreaChart extends AbstractChart
	implements HasAggregationTarget, HasAnimation, HasAreaOpacity, HasAxisTitlesPosition, HasBackground,
	HasChartArea, HasColors, HasInteractivity, HasFocusTarget, HasFont, AllowsIFrame, HasHAxis, HasChartSize,
	HasStackMode, HasLegend, HasLineDashStyle, HasCategories, HasSelectionMode, HasTheme, HasTitlePosition,
	HasTooltip, HasVAxes, HasIntervals, HasSeries<SteppedAreaSeries>
{
	public SteppedAreaChart()
	{
		super("SteppedAreaChart");
	}

	public ChartModel initDefaultColumns(final String groupColumn, final String... dataColumns)
	{
		final ChartModel model = getModel().removeAll()
			.addColumn(Column.New(Column.Type.STRING, groupColumn));
		for(final String dataColumn : dataColumns)
		{
			model.addColumn(Column.New(Column.Type.NUMBER, dataColumn));
		}
		return model;
	}
	
	public boolean getConnectSteps()
	{
		return properties().get("connectSteps", true);
	}

	public void setConnectSteps(final boolean connectSteps)
	{
		properties().put("connectSteps", connectSteps);
	}

	@Override
	public void showSampleData()
	{
		initDefaultColumns("Director (Year)", "Rotten Tomatoes", "IMDB")
			.addRow("Alfred Hitchcock (1935)", 8.4, 7.9)
			.addRow("Ralph Thomas (1959)", 6.9, 6.5)
			.addRow("Don Sharp (1978)", 6.5, 6.4)
			.addRow("James Hawes (2008)", 4.4, 6.2);

		setTitle("The decline of 'The 39 Steps'");
		setVAxis(Axis.New("Accumulated Rating"));
		setStackMode(StackMode.TRUE);
	}
}
