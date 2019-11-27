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

package com.rapidclipse.framework.server.charts.pie;

import java.util.HashMap;
import java.util.Map;

import com.rapidclipse.framework.server.charts.Area;
import com.rapidclipse.framework.server.charts.Background;
import com.rapidclipse.framework.server.charts.Chart;
import com.rapidclipse.framework.server.charts.ChartModel;
import com.rapidclipse.framework.server.charts.Column;
import com.rapidclipse.framework.server.charts.Legend;
import com.rapidclipse.framework.server.charts.TextStyle;
import com.rapidclipse.framework.server.util.JavaScriptable.ObjectHelper;
import com.vaadin.flow.component.Tag;


/**
 *
 * @author XDEV Software
 * @since 10.02.00
 */
@Tag("pie-chart")
public class PieChart extends Chart
{
	private Background                background;
	private Area                      chartArea;
	private Boolean                   enableInteractivity;
	private Boolean                   forceIFrame;
	private Boolean                   is3D;
	private Legend                    legend;
	private Double                    pieHole;
	private String                    pieSliceBorderColor;
	private String                    pieSliceText;
	private TextStyle                 pieSliceTextStyle;
	private Double                    pieStartAngle;
	private Boolean                   reverseCategories;
	private String                    pieResidueSliceColor;
	private String                    pieResidueSliceLabel;
	private Double                    sliceVisibilityThreshold;
	private final Map<Integer, Slice> slices = new HashMap<>();

	public PieChart()
	{
		super("PieChart");
	}
	
	@Override
	protected void initModelDefaults(final ChartModel model)
	{
		model.addColumn(Column.New(Column.Type.STRING, "category", "category"));
		model.addColumn(Column.New(Column.Type.NUMBER, "value", "value"));
	}

	public PieChart addSlice(final int rowIndex, final Slice slice)
	{
		this.slices.put(rowIndex, slice);
		return this;
	}

	public Slice removeSlice(final int rowIndex)
	{
		return this.slices.remove(rowIndex);
	}

	public PieChart removeAllSlices()
	{
		this.slices.clear();
		return this;
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

	public Boolean getIs3D()
	{
		return this.is3D;
	}

	public void setIs3D(final Boolean is3d)
	{
		this.is3D = is3d;
	}

	public Legend getLegend()
	{
		return this.legend;
	}

	public void setLegend(final Legend legend)
	{
		this.legend = legend;
	}

	public Double getPieHole()
	{
		return this.pieHole;
	}

	public void setPieHole(final Double pieHole)
	{
		this.pieHole = pieHole;
	}

	public String getPieSliceBorderColor()
	{
		return this.pieSliceBorderColor;
	}

	public void setPieSliceBorderColor(final String pieSliceBorderColor)
	{
		this.pieSliceBorderColor = pieSliceBorderColor;
	}

	public String getPieSliceText()
	{
		return this.pieSliceText;
	}

	public void setPieSliceText(final String pieSliceText)
	{
		this.pieSliceText = pieSliceText;
	}

	public TextStyle getPieSliceTextStyle()
	{
		return this.pieSliceTextStyle;
	}

	public void setPieSliceTextStyle(final TextStyle pieSliceTextStyle)
	{
		this.pieSliceTextStyle = pieSliceTextStyle;
	}

	public Double getPieStartAngle()
	{
		return this.pieStartAngle;
	}

	public void setPieStartAngle(final Double pieStartAngle)
	{
		this.pieStartAngle = pieStartAngle;
	}

	public Boolean getReverseCategories()
	{
		return this.reverseCategories;
	}

	public void setReverseCategories(final Boolean reverseCategories)
	{
		this.reverseCategories = reverseCategories;
	}

	public String getPieResidueSliceColor()
	{
		return this.pieResidueSliceColor;
	}

	public void setPieResidueSliceColor(final String pieResidueSliceColor)
	{
		this.pieResidueSliceColor = pieResidueSliceColor;
	}

	public String getPieResidueSliceLabel()
	{
		return this.pieResidueSliceLabel;
	}

	public void setPieResidueSliceLabel(final String pieResidueSliceLabel)
	{
		this.pieResidueSliceLabel = pieResidueSliceLabel;
	}

	public Double getSliceVisibilityThreshold()
	{
		return this.sliceVisibilityThreshold;
	}

	public void setSliceVisibilityThreshold(final Double sliceVisibilityThreshold)
	{
		this.sliceVisibilityThreshold = sliceVisibilityThreshold;
	}

	@Override
	protected void createConfiguration(final ObjectHelper obj)
	{
		super.createConfiguration(obj);

		obj.putIfNotNull("backgroundColor", this.background);
		obj.putIfNotNull("chartArea", this.chartArea);
		obj.putIfNotNull("enableInteractivity", this.enableInteractivity);
		obj.putIfNotNull("forceIFrame", this.forceIFrame);
		obj.putIfNotNull("is3D", this.is3D);
		obj.putIfNotNull("legend", this.legend);
		obj.putIfNotNull("pieHole", this.pieHole);
		obj.putIfNotNull("pieSliceBorderColor", this.pieSliceBorderColor);
		obj.putIfNotNull("pieSliceText", this.pieSliceText);
		obj.putIfNotNull("pieSliceTextStyle", this.pieSliceTextStyle);
		obj.putIfNotNull("pieStartAngle", this.pieStartAngle);
		obj.putIfNotNull("reverseCategories", this.reverseCategories);
		obj.putIfNotNull("pieResidueSliceColor", this.pieResidueSliceColor);
		obj.putIfNotNull("pieResidueSliceLabel", this.pieResidueSliceLabel);
		obj.putIfNotNull("sliceVisibilityThreshold", this.sliceVisibilityThreshold);

		putIfNotNull(obj, "slices", this.slices);
	}
}
