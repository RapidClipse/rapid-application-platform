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

package com.rapidclipse.framework.server.charts.treemap;

import com.rapidclipse.framework.server.charts.AllowsIFrame;
import com.rapidclipse.framework.server.charts.AbstractChart;
import com.rapidclipse.framework.server.charts.ChartModel;
import com.rapidclipse.framework.server.charts.Column;
import com.rapidclipse.framework.server.charts.HasFont;
import com.rapidclipse.framework.server.charts.HasTitle;
import com.rapidclipse.framework.server.charts.TextStyle;
import com.vaadin.flow.component.Tag;


/**
 *
 * @author XDEV Software
 * @since 10.02.00
 */
@Tag("treemap-chart")
public class TreeMapChart extends AbstractChart
	implements HasFont, AllowsIFrame, HasTitle
{
	public TreeMapChart()
	{
		super("TreeMap", "treemap");
	}
	
	public ChartModel
		initDefaultColumns(
			final String idColumn,
			final String parentIdColumn,
			final String sizeColumn)
	{
		return getModel().removeAll()
			.addColumn(Column.New(Column.Type.STRING, idColumn))
			.addColumn(Column.New(Column.Type.STRING, parentIdColumn))
			.addColumn(Column.New(Column.Type.NUMBER, sizeColumn));
	}
	
	public ChartModel
		initDefaultColumns(
			final String idColumn,
			final String parentIdColumn,
			final String sizeColumn,
			final String colorColumn)
	{
		return initDefaultColumns(idColumn, parentIdColumn, sizeColumn)
			.addColumn(Column.New(Column.Type.NUMBER, colorColumn));
	}
	
	public String getHeaderColor()
	{
		return properties().get("headerColor");
	}
	
	public void setHeaderColor(final String headerColor)
	{
		properties().put("headerColor", headerColor);
	}
	
	public Integer getHeaderHeight()
	{
		return properties().get("headerHeight");
	}
	
	public void setHeaderHeight(final Integer headerHeight)
	{
		properties().put("headerHeight", headerHeight);
	}
	
	public String getHeaderHighlightColor()
	{
		return properties().get("headerHighlightColor");
	}
	
	public void setHeaderHighlightColor(final String headerHighlightColor)
	{
		properties().put("headerHighlightColor", headerHighlightColor);
	}
	
	public Boolean getHighlightOnMouseOver()
	{
		return properties().get("highlightOnMouseOver");
	}
	
	public void setHighlightOnMouseOver(final Boolean highlightOnMouseOver)
	{
		properties().put("highlightOnMouseOver", highlightOnMouseOver);
	}
	
	public Number getHintOpacity()
	{
		return properties().get("hintOpacity");
	}
	
	public void setHintOpacity(final Number hintOpacity)
	{
		properties().put("hintOpacity", hintOpacity);
	}
	
	public String getMaxColor()
	{
		return properties().get("maxColor");
	}
	
	public void setMaxColor(final String maxColor)
	{
		properties().put("maxColor", maxColor);
	}
	
	public Integer getMaxDepth()
	{
		return properties().get("maxDepth");
	}
	
	public void setMaxDepth(final Integer maxDepth)
	{
		properties().put("maxDepth", maxDepth);
	}
	
	public String getMaxHighlightColor()
	{
		return properties().get("maxHighlightColor");
	}
	
	public void setMaxHighlightColor(final String maxHighlightColor)
	{
		properties().put("maxHighlightColor", maxHighlightColor);
	}
	
	public Integer getMaxPostDepth()
	{
		return properties().get("maxPostDepth");
	}
	
	public void setMaxPostDepth(final Integer maxPostDepth)
	{
		properties().put("maxPostDepth", maxPostDepth);
	}
	
	public Integer getMaxColorValue()
	{
		return properties().get("maxColorValue");
	}
	
	public void setMaxColorValue(final Integer maxColorValue)
	{
		properties().put("maxColorValue", maxColorValue);
	}
	
	public String getMidColor()
	{
		return properties().get("midColor");
	}
	
	public void setMidColor(final String midColor)
	{
		properties().put("midColor", midColor);
	}
	
	public String getMidHighlightColor()
	{
		return properties().get("midHighlightColor");
	}
	
	public void setMidHighlightColor(final String midHighlightColor)
	{
		properties().put("midHighlightColor", midHighlightColor);
	}
	
	public String getMinColor()
	{
		return properties().get("minColor");
	}
	
	public void setMinColor(final String minColor)
	{
		properties().put("minColor", minColor);
	}
	
	public String getMinHighlightColor()
	{
		return properties().get("minHighlightColor");
	}
	
	public void setMinHighlightColor(final String minHighlightColor)
	{
		properties().put("minHighlightColor", minHighlightColor);
	}
	
	public Integer getMinColorValue()
	{
		return properties().get("minColorValue");
	}
	
	public void setMinColorValue(final Integer minColorValue)
	{
		properties().put("minColorValue", minColorValue);
	}
	
	public String getNoColor()
	{
		return properties().get("noColor");
	}
	
	public void setNoColor(final String noColor)
	{
		properties().put("noColor", noColor);
	}
	
	public String getNoHighlightColor()
	{
		return properties().get("noHighlightColor");
	}
	
	public void setNoHighlightColor(final String noHighlightColor)
	{
		properties().put("noHighlightColor", noHighlightColor);
	}
	
	public Boolean getShowScale()
	{
		return properties().get("showScale");
	}
	
	public void setShowScale(final Boolean showScale)
	{
		properties().put("showScale", showScale);
	}
	
	public Boolean getShowTooltips()
	{
		return properties().get("showTooltips");
	}
	
	public void setShowTooltips(final Boolean showTooltips)
	{
		properties().put("showTooltips", showTooltips);
	}
	
	public TextStyle getTextStyle()
	{
		return properties().get("textStyle");
	}
	
	public void setTextStyle(final TextStyle textStyle)
	{
		properties().put("textStyle", textStyle);
	}
	
	public Boolean getUseWeightedAverageForAggregation()
	{
		return properties().get("useWeightedAverageForAggregation");
	}
	
	public void setUseWeightedAverageForAggregation(final Boolean useWeightedAverageForAggregation)
	{
		properties().put("useWeightedAverageForAggregation", useWeightedAverageForAggregation);
	}
}
