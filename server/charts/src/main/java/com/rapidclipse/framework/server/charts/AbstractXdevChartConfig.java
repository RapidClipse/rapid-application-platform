/*
 * Copyright (C) 2013-2019 by XDEV Software, All Rights Reserved.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 *
 * For further information see
 * <http://www.rapidclipse.com/en/legal/license/license.html>.
 */

package com.rapidclipse.framework.server.charts;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import com.rapidclipse.framework.server.charts.config.Tooltip;


/**
 *
 * @author XDEV Software (SS)
 * @since 4.0
 */
public abstract class AbstractXdevChartConfig
{
	private String       title;
	private String       fontName = "Arial";
	private Integer      fontSize;
	private List<String> colors;
	private Tooltip      tooltip;

	private BackgroundStyle backgroundColor = new BackgroundStyle();
	private TextStyle       titleTextStyle  = new TextStyle();
	private LegendOptions   legend          = new LegendOptions();
	private ChartArea       chartArea;
	
	public HashMap<String, Object> getOptions()
	{
		final HashMap<String, Object> options = new HashMap<>();
		options.put("titel", this.title);
		options.put("fontName", this.fontName);
		options.put("fontSize", this.fontSize);
		options.put("colors", this.colors);
		options.put("tooltip", this.tooltip);
		options.put("backgroundColor", this.backgroundColor);
		options.put("titleTextStyle", this.titleTextStyle);
		options.put("legend", this.legend);
		options.put("chartArea", this.chartArea);

		return options;
	}

	public String getTitle()
	{
		return this.title;
	}

	/**
	 * Text to display above the chart.
	 *
	 * @param title
	 */
	public void setTitle(final String title)
	{
		this.title = title;
	}

	public String getFontName()
	{
		return this.fontName;
	}

	/**
	 * The default font face for all text in the chart.
	 *
	 * @param fontName
	 */
	public void setFontName(final String fontName)
	{
		this.fontName = fontName;
	}

	public Integer getFontSize()
	{
		return this.fontSize;
	}

	/**
	 * The default font size, in pixels, of all text in the chart.
	 *
	 * @param fontSize
	 */
	public void setFontSize(final Integer fontSize)
	{
		this.fontSize = fontSize;
	}

	public List<String> getColors()
	{
		return this.colors;
	}
	
	public void setColors(final List<String> colors)
	{
		final List<String> colorString = new ArrayList<>();
		for(final String color : colors)
		{
			colorString.add("'" + color + "'");
		}
		this.colors = colorString;
	}

	public Tooltip getTooltip()
	{
		return this.tooltip;
	}

	public void setTooltip(final Tooltip tooltip)
	{
		this.tooltip = tooltip;
	}

	public BackgroundStyle getBackgroundColor()
	{
		return this.backgroundColor;
	}

	public void setBackgroundColor(final BackgroundStyle backgroundColor)
	{
		this.backgroundColor = backgroundColor;
	}

	public TextStyle getTitleTextStyle()
	{
		return this.titleTextStyle;
	}

	public void setTitleTextStyle(final TextStyle titleTextStyle)
	{
		this.titleTextStyle = titleTextStyle;
	}

	public LegendOptions getLegend()
	{
		return this.legend;
	}

	public void setLegend(final LegendOptions legend)
	{
		this.legend = legend;
	}

	public ChartArea getChartArea()
	{
		return this.chartArea;
	}

	/**
	 * An object with members to configure the placement and size of the chart area
	 * (where the chart itself is drawn, excluding axis and legends). <br>
	 *
	 * @param chartArea
	 */
	public void setChartArea(final ChartArea chartArea)
	{
		this.chartArea = chartArea;
	}

}
