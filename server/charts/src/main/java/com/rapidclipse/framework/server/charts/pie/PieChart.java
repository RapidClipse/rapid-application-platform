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

import com.rapidclipse.framework.server.charts.AllowsIFrame;
import com.rapidclipse.framework.server.charts.ChartBase;
import com.rapidclipse.framework.server.charts.ChartModel;
import com.rapidclipse.framework.server.charts.Column;
import com.rapidclipse.framework.server.charts.HasBackground;
import com.rapidclipse.framework.server.charts.HasCategories;
import com.rapidclipse.framework.server.charts.HasChartArea;
import com.rapidclipse.framework.server.charts.HasChartSize;
import com.rapidclipse.framework.server.charts.HasColors;
import com.rapidclipse.framework.server.charts.HasFont;
import com.rapidclipse.framework.server.charts.HasInteractivity;
import com.rapidclipse.framework.server.charts.HasLegend;
import com.rapidclipse.framework.server.charts.HasTitle;
import com.rapidclipse.framework.server.charts.HasTooltip;
import com.rapidclipse.framework.server.charts.TextStyle;
import com.vaadin.flow.component.Tag;


/**
 *
 * @author XDEV Software
 * @since 10.02.00
 */
@Tag("pie-chart")
public class PieChart extends ChartBase
	implements HasBackground, HasChartArea, HasColors, HasInteractivity, HasFont, AllowsIFrame, HasChartSize,
	HasLegend, HasCategories, HasTitle, HasTooltip
{
	
	public PieChart()
	{
		super("PieChart");
	}
	
	public ChartModel initDefaultColumns(final String labelColumn, final String valueColumn)
	{
		return getModel().removeAll()
			.addColumn(Column.New(Column.Type.STRING, labelColumn))
			.addColumn(Column.New(Column.Type.NUMBER, valueColumn));
	}
	
	public void addSlice(final int rowIndex, final Slice slice)
	{
		properties().putIndexed("slices", rowIndex, slice);
	}
	
	public Slice removeSlice(final int rowIndex)
	{
		return properties().removeIndexed("slices", rowIndex);
	}
	
	public void removeAllSlices()
	{
		properties().removeAllIndexed("slices");
	}

	public Boolean getIs3D()
	{
		return properties().get("is3D");
	}
	
	public void setIs3D(final Boolean is3D)
	{
		properties().put("is3D", is3D);
	}
	
	public Double getPieHole()
	{
		return properties().get("pieHole");
	}
	
	public void setPieHole(final Double pieHole)
	{
		properties().put("pieHole", pieHole);
	}
	
	public String getPieSliceBorderColor()
	{
		return properties().get("pieSliceBorderColor");
	}
	
	public void setPieSliceBorderColor(final String pieSliceBorderColor)
	{
		properties().put("pieSliceBorderColor", pieSliceBorderColor);
	}
	
	public String getPieSliceText()
	{
		return properties().get("pieSliceText");
	}
	
	public void setPieSliceText(final String pieSliceText)
	{
		properties().put("pieSliceText", pieSliceText);
	}
	
	public TextStyle getPieSliceTextStyle()
	{
		return properties().get("pieSliceTextStyle");
	}
	
	public void setPieSliceTextStyle(final TextStyle pieSliceTextStyle)
	{
		properties().put("pieSliceTextStyle", pieSliceTextStyle);
	}
	
	public Double getPieStartAngle()
	{
		return properties().get("pieStartAngle");
	}
	
	public void setPieStartAngle(final Double pieStartAngle)
	{
		properties().put("pieStartAngle", pieStartAngle);
	}
	
	public String getPieResidueSliceColor()
	{
		return properties().get("pieResidueSliceColor");
	}
	
	public void setPieResidueSliceColor(final String pieResidueSliceColor)
	{
		properties().put("pieResidueSliceColor", pieResidueSliceColor);
	}
	
	public String getPieResidueSliceLabel()
	{
		return properties().get("pieResidueSliceLabel");
	}
	
	public void setPieResidueSliceLabel(final String pieResidueSliceLabel)
	{
		properties().put("pieResidueSliceLabel", pieResidueSliceLabel);
	}
	
	public Double getSliceVisibilityThreshold()
	{
		return properties().get("sliceVisibilityThreshold");
	}
	
	public void setSliceVisibilityThreshold(final Double sliceVisibilityThreshold)
	{
		properties().put("sliceVisibilityThreshold", sliceVisibilityThreshold);
	}
}
