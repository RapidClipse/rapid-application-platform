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

import java.util.LinkedHashMap;
import java.util.Map;

import com.rapidclipse.framework.server.charts.Animation;
import com.rapidclipse.framework.server.charts.Area;
import com.rapidclipse.framework.server.charts.Axis;
import com.rapidclipse.framework.server.charts.AxisTitlesPosition;
import com.rapidclipse.framework.server.charts.Background;
import com.rapidclipse.framework.server.charts.Chart;
import com.rapidclipse.framework.server.charts.ChartModel;
import com.rapidclipse.framework.server.charts.Column;
import com.rapidclipse.framework.server.charts.Column.Type;
import com.rapidclipse.framework.server.charts.Legend;
import com.rapidclipse.framework.server.charts.SelectionMode;
import com.rapidclipse.framework.server.charts.TextPosition;
import com.rapidclipse.framework.server.util.JavaScriptable.ObjectHelper;
import com.vaadin.flow.component.Tag;


/**
 *
 * @author XDEV Software
 * @since 10.02.00
 */
@Tag("bubble-chart")
public class BubbleChart extends Chart
{
	private Animation                 animation;
	private AxisTitlesPosition        axisTitlesPosition;
	private Background                background;
	private Bubbles                   bubbles;
	private Area                      chartArea;
	private ColorAxis                 colorAxis;
	private Boolean                   enableInteractivity;
	private Boolean                   forceIFrame;
	private Axis                      hAxis;
	private Legend                    legend;
	private SelectionMode             selectionMode;
	private final Map<String, Series> series = new LinkedHashMap<>();
	private SizeAxis                  sizeAxis;
	private Boolean                   sortBubblesBySize;
	private TextPosition              titlePosition;
	private Axis                      vAxis;

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

	public BubbleChart addSeries(final String name, final Series series)
	{
		this.series.put(name, series);
		return this;
	}

	public Series removeSeries(final String name)
	{
		return this.series.remove(name);
	}

	public BubbleChart removeAllSeries()
	{
		this.series.clear();
		return this;
	}

	public Animation getAnimation()
	{
		return this.animation;
	}

	public void setAnimation(final Animation animation)
	{
		this.animation = animation;
	}

	public AxisTitlesPosition getAxisTitlesPosition()
	{
		return this.axisTitlesPosition;
	}

	public void setAxisTitlesPosition(final AxisTitlesPosition axisTitlesPosition)
	{
		this.axisTitlesPosition = axisTitlesPosition;
	}

	public Background getBackground()
	{
		return this.background;
	}

	public void setBackground(final Background background)
	{
		this.background = background;
	}

	public Bubbles getBubbles()
	{
		return this.bubbles;
	}

	public void setBubbles(final Bubbles bubbles)
	{
		this.bubbles = bubbles;
	}

	public Area getChartArea()
	{
		return this.chartArea;
	}

	public void setChartArea(final Area chartArea)
	{
		this.chartArea = chartArea;
	}

	public ColorAxis getColorAxis()
	{
		return this.colorAxis;
	}

	public void setColorAxis(final ColorAxis colorAxis)
	{
		this.colorAxis = colorAxis;
	}

	public Boolean getEnableInteractivity()
	{
		return this.enableInteractivity;
	}

	public void setEnableInteractivity(final Boolean enableInteractivity)
	{
		this.enableInteractivity = enableInteractivity;
	}

	public Boolean getForceIFrame()
	{
		return this.forceIFrame;
	}

	public void setForceIFrame(final Boolean forceIFrame)
	{
		this.forceIFrame = forceIFrame;
	}

	public Axis getHAxis()
	{
		return this.hAxis;
	}

	public void setHAxis(final Axis hAxis)
	{
		this.hAxis = hAxis;
	}

	public Legend getLegend()
	{
		return this.legend;
	}

	public void setLegend(final Legend legend)
	{
		this.legend = legend;
	}

	public SelectionMode getSelectionMode()
	{
		return this.selectionMode;
	}

	public void setSelectionMode(final SelectionMode selectionMode)
	{
		this.selectionMode = selectionMode;
	}

	public SizeAxis getSizeAxis()
	{
		return this.sizeAxis;
	}

	public void setSizeAxis(final SizeAxis sizeAxis)
	{
		this.sizeAxis = sizeAxis;
	}

	public Boolean getSortBubblesBySize()
	{
		return this.sortBubblesBySize;
	}

	public void setSortBubblesBySize(final Boolean sortBubblesBySize)
	{
		this.sortBubblesBySize = sortBubblesBySize;
	}

	public TextPosition getTitlePosition()
	{
		return this.titlePosition;
	}

	public void setTitlePosition(final TextPosition titlePosition)
	{
		this.titlePosition = titlePosition;
	}

	public Axis getVAxis()
	{
		return this.vAxis;
	}

	public void setVAxis(final Axis vAxis)
	{
		this.vAxis = vAxis;
	}

	@Override
	protected void createConfiguration(final ObjectHelper obj)
	{
		super.createConfiguration(obj);

		obj.putIfNotNull("animation", this.animation);
		obj.putIfNotNull("axisTitlesPosition", this.axisTitlesPosition);
		obj.putIfNotNull("bubble", this.bubbles);
		obj.putIfNotNull("backgroundColor", this.background);
		obj.putIfNotNull("chartArea", this.chartArea);
		obj.putIfNotNull("colorAxis", this.colorAxis);
		obj.putIfNotNull("enableInteractivity", this.enableInteractivity);
		obj.putIfNotNull("forceIFrame", this.forceIFrame);
		obj.putIfNotNull("hAxis", this.hAxis);
		obj.putIfNotNull("legend", this.legend);
		obj.putIfNotNull("selectionMode", this.selectionMode);
		obj.putIfNotNull("sizeAxis", this.sizeAxis);
		obj.putIfNotNull("sortBubblesBySize", this.sortBubblesBySize);
		obj.putIfNotNull("titlePosition", this.titlePosition);
		obj.putIfNotNull("vAxis", this.vAxis);

		putIfNotNull(obj, "series", this.series);
	}
}
