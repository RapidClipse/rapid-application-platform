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

package com.rapidclipse.framework.server.charts.bubble;

import com.rapidclipse.framework.server.charts.AllowsIFrame;
import com.rapidclipse.framework.server.charts.ChartBase;
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
public class BubbleChart extends ChartBase
	implements HasAnimation, HasAxisTitlesPosition, HasBackground, HasChartArea, HasColorAxis, HasInteractivity,
	AllowsIFrame, HasHAxis, HasLegend, HasSelectionMode, HasSizeAxis, HasTitlePosition, HasVAxis, HasChartSize
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
	
	public Bubble getBubble()
	{
		return properties().get("bubble");
	}
	
	public void setBubble(final Bubble bubble)
	{
		properties().put("bubble", bubble);
	}
	
	public Boolean getSortBubblesBySize()
	{
		return properties().get("sortBubblesBySize");
	}
	
	public void setSortBubblesBySize(final Boolean sortBubblesBySize)
	{
		properties().put("sortBubblesBySize", sortBubblesBySize);
	}
}
