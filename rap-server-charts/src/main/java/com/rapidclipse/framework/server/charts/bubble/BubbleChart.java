/*
 * Copyright (C) 2013-2023 by XDEV Software, All Rights Reserved.
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
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *
 * Contributors:
 *     XDEV Software - initial API and implementation
 */
package com.rapidclipse.framework.server.charts.bubble;

import com.rapidclipse.framework.server.charts.AbstractChart;
import com.rapidclipse.framework.server.charts.AllowsIFrame;
import com.rapidclipse.framework.server.charts.ChartModel;
import com.rapidclipse.framework.server.charts.Column;
import com.rapidclipse.framework.server.charts.Column.Type;
import com.rapidclipse.framework.server.charts.HasAnimation;
import com.rapidclipse.framework.server.charts.HasAxisTitlesPosition;
import com.rapidclipse.framework.server.charts.HasBackground;
import com.rapidclipse.framework.server.charts.HasChartArea;
import com.rapidclipse.framework.server.charts.HasChartSize;
import com.rapidclipse.framework.server.charts.HasColorAxis;
import com.rapidclipse.framework.server.charts.HasHAxis;
import com.rapidclipse.framework.server.charts.HasInteractivity;
import com.rapidclipse.framework.server.charts.HasLegend;
import com.rapidclipse.framework.server.charts.HasSelectionMode;
import com.rapidclipse.framework.server.charts.HasSeries;
import com.rapidclipse.framework.server.charts.HasSizeAxis;
import com.rapidclipse.framework.server.charts.HasTitlePosition;
import com.rapidclipse.framework.server.charts.HasVAxis;
import com.vaadin.flow.component.Tag;


/**
 *
 * @author XDEV Software
 * @since 10.02.00
 */
@Tag("bubble-chart")
public class BubbleChart extends AbstractChart
	implements HasAnimation, HasAxisTitlesPosition, HasBackground, HasChartArea, HasColorAxis, HasInteractivity,
	AllowsIFrame, HasHAxis, HasLegend, HasSelectionMode, HasSizeAxis, HasTitlePosition, HasVAxis, HasChartSize,
	HasSeries<BubbleSeries>
{
	public BubbleChart()
	{
		super("BubbleChart");
	}
	
	public ChartModel initDefaultColumns(final String idColumn, final String xColumn, final String yColumn)
	{
		return getModel().removeAll()
			.addColumn(Column.New(Column.Type.STRING, idColumn))
			.addColumn(Column.New(Column.Type.NUMBER, xColumn))
			.addColumn(Column.New(Column.Type.NUMBER, yColumn));
	}
	
	public ChartModel initDefaultColumns(
		final String idColumn,
		final String xColumn,
		final String yColumn,
		final String valueColumn,
		final Column.Type valueColumnType)
	{
		validateColumnType(valueColumnType, "value column", Column.Type.STRING, Column.Type.NUMBER);
		
		return initDefaultColumns(idColumn, xColumn, yColumn)
			.addColumn(Column.New(valueColumnType, valueColumn));
	}
	
	public ChartModel initDefaultColumns(
		final String idColumn,
		final String xColumn,
		final String yColumn,
		final String valueColumn,
		final Column.Type valueColumnType,
		final String sizeColumn)
	{
		return initDefaultColumns(idColumn, xColumn, yColumn, valueColumn, valueColumnType)
			.addColumn(Column.New(Type.NUMBER, sizeColumn));
	}
	
	public Bubble getBubble()
	{
		return properties().get("bubble", null);
	}
	
	public void setBubble(final Bubble bubble)
	{
		properties().put("bubble", bubble);
	}
	
	public boolean getSortBubblesBySize()
	{
		return properties().get("sortBubblesBySize", true);
	}
	
	public void setSortBubblesBySize(final boolean sortBubblesBySize)
	{
		properties().put("sortBubblesBySize", sortBubblesBySize);
	}
	
	@Override
	public void showSampleData()
	{
		initDefaultColumns("ID", "Life Expectancy", "Fertility Rate", "Region", Type.STRING, "Population")
			.addRow("CAN", 80.66, 1.67, "North America", 33739900)
			.addRow("DEU", 79.84, 1.36, "Europe", 81902307)
			.addRow("DNK", 78.6, 1.84, "Europe", 5523095)
			.addRow("EGY", 72.73, 2.78, "Middle East", 79716203)
			.addRow("GBR", 80.05, 2, "Europe", 61801570)
			.addRow("IRN", 72.49, 1.7, "Middle East", 73137148)
			.addRow("IRQ", 68.09, 4.77, "Middle East", 31090763)
			.addRow("ISR", 81.55, 2.96, "Middle East", 7485600)
			.addRow("RUS", 68.6, 1.54, "Europe", 141850000)
			.addRow("USA", 78.09, 2.05, "North America", 307007000);
		
		setTitle("Correlation between life expectancy, fertility rate and population of some world countries (2010)");
	}
}
