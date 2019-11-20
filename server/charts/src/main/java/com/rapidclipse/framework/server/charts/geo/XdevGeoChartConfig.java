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
package com.rapidclipse.framework.server.charts.geo;

import java.io.Serializable;
import java.util.HashMap;

import com.rapidclipse.framework.server.charts.config.BackgroundStyle;
import com.rapidclipse.framework.server.charts.config.ColorAxis;


/**
 *
 * @author XDEV Software
 * @since 10.02.00
 */
public class XdevGeoChartConfig implements Serializable
{
	private String region              = "world";
	private String displayMode         = "auto";
	private String defaultColor        = "#267114";
	private String datalessRegionColor = "#F5F5F5";
	private String domain;
	private String resolution;

	private BackgroundStyle backgroundColor = new BackgroundStyle();
	private ColorAxis       colorAxis;
	
	public HashMap<String, Object> getOptions()
	{
		final HashMap<String, Object> options = new HashMap<>();
		options.put("region", this.region);
		options.put("displayMode", this.displayMode);
		options.put("defaultColor", this.defaultColor);
		options.put("datalessRegionColor", this.datalessRegionColor);
		options.put("backgroundColor", this.backgroundColor);
		options.put("colorAxis", this.colorAxis);
		options.put("domain", this.domain);
		options.put("resolution", this.resolution);
		return options;
	}

	public String getRegion()
	{
		return this.region;
	}

	/**
	 * The area to display on the geochart. (Surrounding areas will be displayed as
	 * well.) Can be one of the following: <br>
	 * <ul>
	 * <li>'world' - A geochart of the entire world.</li>
	 * <li>A continent or a sub-continent, specified by its 3-digit code, e.g.,
	 * '011' for Western Africa.</li>
	 * <li>A country, specified by its ISO 3166-1 alpha-2 code, e.g., 'AU' for
	 * Australia.</li>
	 * </ul>
	 *
	 * @param region
	 */
	public void setRegion(final String region)
	{
		this.region = region;
	}

	public String getDisplayMode()
	{
		return this.displayMode;
	}

	/**
	 * Which type of geochart this is. The DataTable format must match the value
	 * specified. The following values are supported: <br>
	 * <ul>
	 * <li>'auto' - Choose based on the format of the DataTable.</li>
	 * <li>'regions' - Color the regions on the geochart.</li>
	 * <li>'markers' - Place markers on the regions.</li>
	 * <li>'text' - Label the regions with text from the DataTable.</li>
	 * </ul>
	 * <br>
	 * Default: auto <br>
	 *
	 * @param displayMode
	 */
	public void setDisplayMode(final String displayMode)
	{
		this.displayMode = displayMode;
	}

	public BackgroundStyle getBackgroundColor()
	{
		return this.backgroundColor;
	}

	public void setBackgroundColor(final BackgroundStyle backgroundColor)
	{
		this.backgroundColor = backgroundColor;
	}

	/**
	 * @return the defaultColor
	 */
	public String getDefaultColor()
	{
		return this.defaultColor;
	}

	/**
	 * The color to use for data points in a geochart when the location (e.g., 'US'
	 * ) is present but the value is either null or unspecified. This is distinct
	 * from datalessRegionColor, which is the color used when data is missing. <br>
	 *
	 * @param defaultColor
	 *            the defaultColor to set
	 */
	public void setDefaultColor(final String defaultColor)
	{
		this.defaultColor = defaultColor;
	}

	/**
	 * @return the datalessRegionColor
	 */
	public String getDatalessRegionColor()
	{
		return this.datalessRegionColor;
	}

	/**
	 * Color to assign to regions with no associated data. <br>
	 * Default: '#F5F5F5' <br>
	 *
	 * @param datalessRegionColor
	 *            the datalessRegionColor to set
	 */
	public void setDatalessRegionColor(final String datalessRegionColor)
	{
		this.datalessRegionColor = datalessRegionColor;
	}

	/**
	 * @return the colorAxis
	 */
	public ColorAxis getColorAxis()
	{
		return this.colorAxis;
	}

	/**
	 * An object that specifies a mapping between color column values and colors or
	 * a gradient scale. <br>
	 *
	 * @param colorAxis
	 *            the colorAxis to set
	 */
	public void setColorAxis(final ColorAxis colorAxis)
	{
		this.colorAxis = colorAxis;
	}
	
	public String getDomain()
	{
		return this.domain;
	}
	
	/**
	 * Show the geochart as though it were being served from this region.
	 * For instance, setting domain to 'IN' will display Kashmir as belonging to India rather than as a disputed
	 * territory.
	 *
	 * @param domain
	 *            ISO 3166-1 alpha-2 code for a country
	 */
	public void setDomain(final String domain)
	{
		this.domain = domain;
	}
	
	public String getResolution()
	{
		return this.resolution;
	}
	
	/**
	 * The resolution of the geochart borders. Choose one of the following values:
	 *
	 * <li>'countries' - Supported for all regions, except for US state regions.</li>
	 * <li>'provinces' - Supported only for country regions and US state regions. Not supported for all countries;
	 * please
	 * test a country to see whether this option is supported.</li>
	 * <li>'metros' - Supported for the US country region and US state regions only.</li>
	 *
	 * @param resolution
	 */
	public void setResolution(final String resolution)
	{
		this.resolution = resolution;
	}
	
}
