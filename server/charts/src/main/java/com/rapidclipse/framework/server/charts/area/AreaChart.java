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

import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import com.rapidclipse.framework.server.charts.Animation;
import com.rapidclipse.framework.server.charts.Annotations;
import com.rapidclipse.framework.server.charts.Area;
import com.rapidclipse.framework.server.charts.Axis;
import com.rapidclipse.framework.server.charts.Background;
import com.rapidclipse.framework.server.charts.Chart;
import com.rapidclipse.framework.server.charts.Crosshair;
import com.rapidclipse.framework.server.charts.Legend;
import com.rapidclipse.framework.server.charts.Orientation;
import com.rapidclipse.framework.server.charts.PointShape;
import com.rapidclipse.framework.server.charts.SelectionMode;
import com.rapidclipse.framework.server.charts.TextPosition;
import com.rapidclipse.framework.server.util.JavaScriptable;
import com.rapidclipse.framework.server.util.JavaScriptable.ArrayHelper;
import com.rapidclipse.framework.server.util.JavaScriptable.ObjectHelper;
import com.vaadin.flow.component.Tag;

import elemental.json.Json;


/**
 *
 * @author XDEV Software
 * @since 10.02.00
 */
@Tag("area-chart")
public class AreaChart extends Chart
{
	public static enum AggregationTarget implements JavaScriptable
	{
		CATEGORY("category"),
		SERIES("series"),
		AUTO("auto"),
		NONE("none");

		private final String js;

		private AggregationTarget(final String js)
		{
			this.js = Json.create(js).toJson();
		}

		@Override
		public String js()
		{
			return this.js;
		}
	}

	public static enum AxisTitlesPosition implements JavaScriptable
	{
		IN("in"),
		OUT("out"),
		NONE("none");

		private final String js;

		private AxisTitlesPosition(final String js)
		{
			this.js = Json.create(js).toJson();
		}

		@Override
		public String js()
		{
			return this.js;
		}
	}

	public static enum FocusTarget implements JavaScriptable
	{
		DATUM("datum"),
		CATEGORY("category");

		private final String js;

		private FocusTarget(final String js)
		{
			this.js = Json.create(js).toJson();
		}

		@Override
		public String js()
		{
			return this.js;
		}
	}

	public static enum StackMode implements JavaScriptable
	{
		FALSE("false"),
		TRUE("true"),
		PERCENT("'percent'"),
		RELATIVE("'relative'"),
		ABSOLUTE("'absolute'");

		private final String js;

		private StackMode(final String js)
		{
			this.js = js;
		}

		@Override
		public String js()
		{
			return this.js;
		}
	}

	private AggregationTarget          aggregationTarget;
	private Animation                  animation;
	private Annotations                annotations;
	private Double                     areaOpacity;
	private AxisTitlesPosition         axisTitlesPosition;
	private Background                 background;
	private Area                       chartArea;
	private Crosshair                  crosshair;
	private Double                     dataOpacity;
	private Boolean                    enableInteractivity;
	private FocusTarget                focusTarget;
	private Boolean                    forceIFrame;
	private Axis                       hAxis;
	private Boolean                    interpolateNulls;
	private StackMode                  stackMode;
	private Legend                     legend;
	private List<Double>               lineDashStyle;
	private Double                     lineWidth;
	private Orientation                orientation;
	private PointShape.Type            pointShape;
	private Double                     pointSize;
	private Boolean                    pointsVisible;
	private Boolean                    reverseCategories;
	private SelectionMode              selectionMode;
	private final Map<Integer, Series> series = new LinkedHashMap<>();
	private TextPosition               titlePosition;
	private Axis                       vAxis;
	private final Map<Integer, Axis>   vAxes  = new LinkedHashMap<>();

	public AreaChart()
	{
		super("AreaChart");
	}

	public AreaChart addSeries(final int rowIndex, final Series series)
	{
		this.series.put(rowIndex, series);
		return this;
	}

	public Series removeSeries(final int rowIndex)
	{
		return this.series.remove(rowIndex);
	}

	public AreaChart removeAllSeries()
	{
		this.series.clear();
		return this;
	}

	public AreaChart addVAxis(final int rowIndex, final Axis axis)
	{
		this.vAxes.put(rowIndex, axis);
		return this;
	}

	public Axis removeVAxis(final int rowIndex)
	{
		return this.vAxes.remove(rowIndex);
	}

	public AreaChart removeAllVAxes()
	{
		this.vAxes.clear();
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

	public Annotations getAnnotations()
	{
		return this.annotations;
	}

	public void setAnnotations(final Annotations annotations)
	{
		this.annotations = annotations;
	}

	public Double getAreaOpacity()
	{
		return this.areaOpacity;
	}

	public void setAreaOpacity(final Double areaOpacity)
	{
		this.areaOpacity = areaOpacity;
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

	public Area getChartArea()
	{
		return this.chartArea;
	}

	public void setChartArea(final Area chartArea)
	{
		this.chartArea = chartArea;
	}

	public Crosshair getCrosshair()
	{
		return this.crosshair;
	}

	public void setCrosshair(final Crosshair crosshair)
	{
		this.crosshair = crosshair;
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

	public Boolean getInterpolateNulls()
	{
		return this.interpolateNulls;
	}

	public void setInterpolateNulls(final Boolean interpolateNulls)
	{
		this.interpolateNulls = interpolateNulls;
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

	public List<Double> getLineDashStyle()
	{
		return this.lineDashStyle;
	}

	public void setLineDashStyle(final List<Double> lineDashStyle)
	{
		this.lineDashStyle = lineDashStyle;
	}

	public Double getLineWidth()
	{
		return this.lineWidth;
	}

	public void setLineWidth(final Double lineWidth)
	{
		this.lineWidth = lineWidth;
	}

	public Orientation getOrientation()
	{
		return this.orientation;
	}

	public void setOrientation(final Orientation orientation)
	{
		this.orientation = orientation;
	}

	public PointShape.Type getPointShape()
	{
		return this.pointShape;
	}

	public void setPointShape(final PointShape.Type pointShape)
	{
		this.pointShape = pointShape;
	}

	public Double getPointSize()
	{
		return this.pointSize;
	}

	public void setPointSize(final Double pointSize)
	{
		this.pointSize = pointSize;
	}

	public Boolean getPointsVisible()
	{
		return this.pointsVisible;
	}

	public void setPointsVisible(final Boolean pointsVisible)
	{
		this.pointsVisible = pointsVisible;
	}

	public Boolean getReverseCategories()
	{
		return this.reverseCategories;
	}

	public void setReverseCategories(final Boolean reverseCategories)
	{
		this.reverseCategories = reverseCategories;
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
		obj.putIfNotNull("annotations", this.annotations);
		obj.putIfNotNull("areaOpacity", this.areaOpacity);
		obj.putIfNotNull("axisTitlesPosition", this.axisTitlesPosition);
		obj.putIfNotNull("backgroundColor", this.background);
		obj.putIfNotNull("chartArea", this.chartArea);
		obj.putIfNotNull("crosshair", this.crosshair);
		obj.putIfNotNull("dataOpacity", this.dataOpacity);
		obj.putIfNotNull("enableInteractivity", this.enableInteractivity);
		obj.putIfNotNull("focusTarget", this.focusTarget);
		obj.putIfNotNull("forceIFrame", this.forceIFrame);
		obj.putIfNotNull("hAxis", this.hAxis);
		obj.putIfNotNull("interpolateNulls", this.interpolateNulls);
		obj.putIfNotNull("isStacked", this.stackMode);
		obj.putIfNotNull("legend", this.legend);
		obj.putIfNotNull("lineDashStyle", new ArrayHelper().addAllNumbers(this.lineDashStyle));
		obj.putIfNotNull("lineWidth", this.lineWidth);
		obj.putIfNotNull("orientation", this.orientation);
		obj.putIfNotNull("pointShape", this.pointShape);
		obj.putIfNotNull("pointSize", this.pointSize);
		obj.putIfNotNull("pointsVisible", this.pointsVisible);
		obj.putIfNotNull("reverseCategories", this.reverseCategories);
		obj.putIfNotNull("selectionMode", this.selectionMode);
		obj.putIfNotNull("titlePosition", this.titlePosition);
		obj.putIfNotNull("vAxis", this.vAxis);
		
		putIfNotNull(obj, "series", this.series);
		putIfNotNull(obj, "vAxes", this.vAxes);
	}
}
