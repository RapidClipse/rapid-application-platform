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
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *
 * Contributors:
 *     XDEV Software Corp. - initial API and implementation
 */
package com.rapidclipse.framework.server.charts.pie;

import com.rapidclipse.framework.server.charts.AbstractChart;
import com.rapidclipse.framework.server.charts.AllowsIFrame;
import com.rapidclipse.framework.server.charts.ChartModel;
import com.rapidclipse.framework.server.charts.Column;
import com.rapidclipse.framework.server.charts.DiffChart;
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
public class PieChart extends AbstractChart
	implements HasBackground, HasChartArea, HasColors, HasInteractivity, HasFont, AllowsIFrame, HasChartSize,
	HasLegend, HasCategories, HasTitle, HasTooltip, DiffChart
{

	public PieChart()
	{
		super("PieChart");
	}

	public PieChart(final ChartModel before, final ChartModel after)
	{
		this();

		setModel(before, after);
	}

	@Override
	public void setModel(final ChartModel before, final ChartModel after)
	{
		super.setModel(before, after);
	}

	public ChartModel initDefaultColumns()
	{
		return initDefaultColumns("label", "value");
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

	public boolean getIs3D()
	{
		return properties().get("is3D", false);
	}

	public void setIs3D(final boolean is3D)
	{
		properties().put("is3D", is3D);
	}

	public Number getPieHole()
	{
		return properties().get("pieHole", 0);
	}

	public void setPieHole(final Number pieHole)
	{
		properties().put("pieHole", pieHole);
	}

	public String getPieSliceBorderColor()
	{
		return properties().get("pieSliceBorderColor", "white");
	}

	public void setPieSliceBorderColor(final String pieSliceBorderColor)
	{
		properties().put("pieSliceBorderColor", pieSliceBorderColor);
	}

	public PieSliceText getPieSliceText()
	{
		return properties().get("pieSliceText", PieSliceText.PERCENTAGE);
	}

	public void setPieSliceText(final PieSliceText pieSliceText)
	{
		properties().put("pieSliceText", pieSliceText);
	}

	public TextStyle getPieSliceTextStyle()
	{
		return properties().get("pieSliceTextStyle", null);
	}

	public void setPieSliceTextStyle(final TextStyle pieSliceTextStyle)
	{
		properties().put("pieSliceTextStyle", pieSliceTextStyle);
	}

	public Number getPieStartAngle()
	{
		return properties().get("pieStartAngle", 0);
	}

	public void setPieStartAngle(final Number pieStartAngle)
	{
		properties().put("pieStartAngle", pieStartAngle);
	}

	public String getPieResidueSliceColor()
	{
		return properties().get("pieResidueSliceColor", "#ccc");
	}

	public void setPieResidueSliceColor(final String pieResidueSliceColor)
	{
		properties().put("pieResidueSliceColor", pieResidueSliceColor);
	}

	public String getPieResidueSliceLabel()
	{
		return properties().get("pieResidueSliceLabel", "Other");
	}

	public void setPieResidueSliceLabel(final String pieResidueSliceLabel)
	{
		properties().put("pieResidueSliceLabel", pieResidueSliceLabel);
	}

	public Number getSliceVisibilityThreshold()
	{
		return properties().get("sliceVisibilityThreshold", 0.0014);
	}

	public void setSliceVisibilityThreshold(final Number sliceVisibilityThreshold)
	{
		properties().put("sliceVisibilityThreshold", sliceVisibilityThreshold);
	}

	@Override
	public void showSampleData()
	{
		initDefaultColumns("Task", "Hours per Day")
			.addRow("Work", 11)
			.addRow("Eat", 2)
			.addRow("Commute", 2)
			.addRow("Watch TV", 2)
			.addRow("Sleep", 7);

		setTitle("Daily Activities");
		setIs3D(true);
	}
}
