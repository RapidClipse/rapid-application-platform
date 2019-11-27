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

package com.rapidclipse.framework.server.charts.bar;

import java.util.LinkedHashMap;
import java.util.Map;

import com.rapidclipse.framework.server.charts.Animation;
import com.rapidclipse.framework.server.charts.Annotations;
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
import com.rapidclipse.framework.server.charts.StackMode;
import com.rapidclipse.framework.server.charts.TextPosition;
import com.rapidclipse.framework.server.charts.area.Series;
import com.rapidclipse.framework.server.util.JavaScriptable.ObjectHelper;
import com.vaadin.flow.component.Tag;


/**
 *
 * @author XDEV Software
 * @since 10.02.00
 */
@Tag("bar-chart")
public class BarChart extends Chart
{
	private Animation                     animation;
	private Annotations                   annotations;
	private AxisTitlesPosition            axisTitlesPosition;
	private Background                    background;
	private String                        barGroupWidth;
	private Area                          chartArea;
	private Double                        dataOpacity;
	private Boolean                       enableInteractivity;
	private FocusTarget                   focusTarget;
	private Boolean                       forceIFrame;
	private Axis                          hAxis;
	private final Map<Integer, Axis>      hAxes      = new LinkedHashMap<>();
	private StackMode                     stackMode;
	private Legend                        legend;
	private Boolean                       reverseCategories;
	private Orientation                   orientation;
	private final Map<Integer, Series>    series     = new LinkedHashMap<>();
	private TextPosition                  titlePosition;
	private final Map<Integer, Trendline> trendlines = new LinkedHashMap<>();
	private Axis                          vAxis;
	
	public BarChart()
	{
		super("BarChart");
	}
	
	@Override
	protected void initModelDefaults(final ChartModel model)
	{
		model.addColumn(Column.New(Column.Type.STRING, "ycaption", "ycaption"));
	}
	
	public BarChart addHAxis(final int rowIndex, final Axis axis)
	{
		this.hAxes.put(rowIndex, axis);
		return this;
	}
	
	public Axis removeHAxis(final int rowIndex)
	{
		return this.hAxes.remove(rowIndex);
	}
	
	public BarChart removeAllHAxes()
	{
		this.hAxes.clear();
		return this;
	}
	
	public BarChart addSeries(final int rowIndex, final Series series)
	{
		this.series.put(rowIndex, series);
		return this;
	}
	
	public Series removeSeries(final int rowIndex)
	{
		return this.series.remove(rowIndex);
	}
	
	public BarChart removeAllSeries()
	{
		this.series.clear();
		return this;
	}
	
	public BarChart addTrendline(final int rowIndex, final Trendline trendline)
	{
		this.trendlines.put(rowIndex, trendline);
		return this;
	}
	
	public Trendline removeTrendline(final int rowIndex)
	{
		return this.trendlines.remove(rowIndex);
	}
	
	public BarChart removeAllTrendlines()
	{
		this.trendlines.clear();
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
	
	public Annotations getAnnotations()
	{
		return this.annotations;
	}
	
	public void setAnnotations(final Annotations annotations)
	{
		this.annotations = annotations;
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
	
	public Area getChartArea()
	{
		return this.chartArea;
	}
	
	public void setChartArea(final Area chartArea)
	{
		this.chartArea = chartArea;
	}
	
	public Double getDataOpacity()
	{
		return this.dataOpacity;
	}
	
	public void setDataOpacity(final Double dataOpacity)
	{
		this.dataOpacity = dataOpacity;
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
	
	public StackMode getStackMode()
	{
		return this.stackMode;
	}
	
	public void setStackMode(final StackMode stackMode)
	{
		this.stackMode = stackMode;
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
		obj.putIfNotNull("annotations", this.annotations);
		obj.putIfNotNull("axisTitlesPosition", this.axisTitlesPosition);
		obj.putIfNotNull("backgroundColor", this.background);
		if(this.barGroupWidth != null)
		{
			obj.putIfNotNull("bar", new ObjectHelper().put("groupWidth", this.barGroupWidth));
		}
		obj.putIfNotNull("chartArea", this.chartArea);
		obj.putIfNotNull("dataOpacity", this.dataOpacity);
		obj.putIfNotNull("enableInteractivity", this.enableInteractivity);
		obj.putIfNotNull("focusTarget", this.focusTarget);
		obj.putIfNotNull("forceIFrame", this.forceIFrame);
		obj.putIfNotNull("hAxis", this.hAxis);
		obj.putIfNotNull("isStacked", this.stackMode);
		obj.putIfNotNull("legend", this.legend);
		obj.putIfNotNull("reverseCategories", this.reverseCategories);
		obj.putIfNotNull("orientation", this.orientation);
		obj.putIfNotNull("titlePosition", this.titlePosition);
		obj.putIfNotNull("vAxis", this.vAxis);
		
		putIfNotNull(obj, "hAxes", this.hAxes);
		putIfNotNull(obj, "series", this.series);
		putIfNotNull(obj, "trendlines", this.trendlines);
	}
}
