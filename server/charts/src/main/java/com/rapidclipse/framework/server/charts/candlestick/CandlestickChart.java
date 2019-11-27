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

import java.util.LinkedHashMap;
import java.util.Map;

import com.rapidclipse.framework.server.charts.AggregationTarget;
import com.rapidclipse.framework.server.charts.Animation;
import com.rapidclipse.framework.server.charts.Area;
import com.rapidclipse.framework.server.charts.Axis;
import com.rapidclipse.framework.server.charts.AxisTitlesPosition;
import com.rapidclipse.framework.server.charts.Background;
import com.rapidclipse.framework.server.charts.Chart;
import com.rapidclipse.framework.server.charts.ChartModel;
import com.rapidclipse.framework.server.charts.Column;
import com.rapidclipse.framework.server.charts.FocusTarget;
import com.rapidclipse.framework.server.charts.Legend;
import com.rapidclipse.framework.server.charts.Orientation;
import com.rapidclipse.framework.server.charts.SelectionMode;
import com.rapidclipse.framework.server.charts.TextPosition;
import com.rapidclipse.framework.server.util.JavaScriptable.ObjectHelper;
import com.vaadin.flow.component.Tag;


/**
 *
 * @author XDEV Software
 * @since 10.02.00
 */
@Tag("candlestick-chart")
public class CandlestickChart extends Chart
{
	private AggregationTarget          aggregationTarget;
	private Animation                  animation;
	private AxisTitlesPosition         axisTitlesPosition;
	private Background                 background;
	private String                     barGroupWidth;
	private Candlestick                candlestick;
	private Area                       chartArea;
	private Boolean                    enableInteractivity;
	private FocusTarget                focusTarget;
	private Boolean                    forceIFrame;
	private Axis                       hAxis;
	private Legend                     legend;
	private Boolean                    reverseCategories;
	private Orientation                orientation;
	private SelectionMode              selectionMode;
	private final Map<Integer, Series> series = new LinkedHashMap<>();
	private TextPosition               titlePosition;
	private Axis                       vAxis;
	private final Map<Integer, Axis>   vAxes  = new LinkedHashMap<>();

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

	public CandlestickChart addVAxis(final int rowIndex, final Axis axis)
	{
		this.vAxes.put(rowIndex, axis);
		return this;
	}

	public Axis removeVAxis(final int rowIndex)
	{
		return this.vAxes.remove(rowIndex);
	}

	public CandlestickChart removeAllVAxes()
	{
		this.vAxes.clear();
		return this;
	}

	public CandlestickChart addSeries(final int rowIndex, final Series series)
	{
		this.series.put(rowIndex, series);
		return this;
	}

	public Series removeSeries(final int rowIndex)
	{
		return this.series.remove(rowIndex);
	}

	public CandlestickChart removeAllSeries()
	{
		this.series.clear();
		return this;
	}

	public AggregationTarget getAggregationTarget()
	{
		return this.aggregationTarget;
	}

	public void setAggregationTarget(final AggregationTarget aggregationTarget)
	{
		this.aggregationTarget = aggregationTarget;
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

	public String getBarGroupWidth()
	{
		return this.barGroupWidth;
	}

	public void setBarGroupWidth(final String barGroupWidth)
	{
		this.barGroupWidth = barGroupWidth;
	}

	public Candlestick getCandlestick()
	{
		return this.candlestick;
	}

	public void setCandlestick(final Candlestick candlestick)
	{
		this.candlestick = candlestick;
	}

	public Area getChartArea()
	{
		return this.chartArea;
	}

	public void setChartArea(final Area chartArea)
	{
		this.chartArea = chartArea;
	}

	public Boolean getEnableInteractivity()
	{
		return this.enableInteractivity;
	}

	public void setEnableInteractivity(final Boolean enableInteractivity)
	{
		this.enableInteractivity = enableInteractivity;
	}

	public FocusTarget getFocusTarget()
	{
		return this.focusTarget;
	}

	public void setFocusTarget(final FocusTarget focusTarget)
	{
		this.focusTarget = focusTarget;
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

	public Boolean getReverseCategories()
	{
		return this.reverseCategories;
	}

	public void setReverseCategories(final Boolean reverseCategories)
	{
		this.reverseCategories = reverseCategories;
	}

	public Orientation getOrientation()
	{
		return this.orientation;
	}

	public void setOrientation(final Orientation orientation)
	{
		this.orientation = orientation;
	}

	public SelectionMode getSelectionMode()
	{
		return this.selectionMode;
	}

	public void setSelectionMode(final SelectionMode selectionMode)
	{
		this.selectionMode = selectionMode;
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

		obj.putIfNotNull("aggregationTarget", this.aggregationTarget);
		obj.putIfNotNull("animation", this.animation);
		obj.putIfNotNull("axisTitlesPosition", this.axisTitlesPosition);
		obj.putIfNotNull("backgroundColor", this.background);
		if(this.barGroupWidth != null)
		{
			obj.putIfNotNull("bar", new ObjectHelper().put("groupWidth", this.barGroupWidth));
		}
		obj.putIfNotNull("candlestick", this.candlestick);
		obj.putIfNotNull("chartArea", this.chartArea);
		obj.putIfNotNull("enableInteractivity", this.enableInteractivity);
		obj.putIfNotNull("focusTarget", this.focusTarget);
		obj.putIfNotNull("forceIFrame", this.forceIFrame);
		obj.putIfNotNull("hAxis", this.hAxis);
		obj.putIfNotNull("legend", this.legend);
		obj.putIfNotNull("reverseCategories", this.reverseCategories);
		obj.putIfNotNull("orientation", this.orientation);
		obj.putIfNotNull("selectionMode", this.selectionMode);
		obj.putIfNotNull("titlePosition", this.titlePosition);
		obj.putIfNotNull("vAxis", this.vAxis);

		putIfNotNull(obj, "series", this.series);
		putIfNotNull(obj, "vAxes", this.vAxes);
	}
}
