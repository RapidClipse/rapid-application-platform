/*
 * Copyright (C) 2013-2022 by XDEV Software, All Rights Reserved.
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
 *     XDEV Software - initial API and implementation
 */
package com.rapidclipse.framework.server.charts.maps;

import com.rapidclipse.framework.server.charts.AllowsIFrame;
import com.rapidclipse.framework.server.charts.ChartModel;
import com.rapidclipse.framework.server.charts.Column;
import com.rapidclipse.framework.server.charts.HasBackground;
import com.rapidclipse.framework.server.charts.HasChartSize;
import com.rapidclipse.framework.server.charts.HasColorAxis;
import com.rapidclipse.framework.server.charts.HasSizeAxis;
import com.rapidclipse.framework.server.charts.HasTooltip;
import com.vaadin.flow.component.Tag;


/**
 * @author XDEV Software
 * @since 10.02.00
 *
 */
@Tag("geo-chart")
public class GeoChart extends AbstractMapsChart
	implements HasBackground, HasColorAxis, AllowsIFrame, HasChartSize, HasSizeAxis, HasTooltip
{
	public GeoChart()
	{
		super("GeoChart", "geochart");
	}

	public ChartModel initDefaultColumnsRegion()
	{
		return getModel().removeAll()
			.addColumn(Column.New(Column.Type.STRING, "location"));
	}

	public ChartModel initDefaultColumnsRegionWithColor()
	{
		return initDefaultColumnsRegion()
			.addColumn(Column.New(Column.Type.STRING, "color"));
	}

	public ChartModel initDefaultColumnsMarker()
	{
		return getModel().removeAll()
			.addColumn(Column.New(Column.Type.STRING, "location"));
	}

	public ChartModel initDefaultColumnsMarkerWithColor()
	{
		return initDefaultColumnsMarker()
			.addColumn(Column.New(Column.Type.STRING, "color"));
	}

	public ChartModel initDefaultColumnsMarkerWithColorAndSize()
	{
		return initDefaultColumnsMarkerWithColor()
			.addColumn(Column.New(Column.Type.STRING, "size"));
	}

	public ChartModel initDefaultColumnsMarkerLatLong()
	{
		return getModel().removeAll()
			.addColumn(Column.New(Column.Type.STRING, "latitude"))
			.addColumn(Column.New(Column.Type.STRING, "longitude"));
	}

	public ChartModel initDefaultColumnsMarkerLatLongWithColor()
	{
		return initDefaultColumnsMarkerLatLong()
			.addColumn(Column.New(Column.Type.STRING, "color"));
	}

	public ChartModel initDefaultColumnsMarkerLatLongWithColorAndSize()
	{
		return initDefaultColumnsMarkerLatLongWithColor()
			.addColumn(Column.New(Column.Type.STRING, "size"));
	}

	public ChartModel initDefaultColumnsText()
	{
		return getModel().removeAll()
			.addColumn(Column.New(Column.Type.STRING, "text"));
	}

	public ChartModel initDefaultColumnsTextWithSize()
	{
		return initDefaultColumnsText()
			.addColumn(Column.New(Column.Type.NUMBER, "size"));
	}

	public String getDatalessRegionColor()
	{
		return properties().get("datalessRegionColor", "#F5F5F5");
	}

	public void setDatalessRegionColor(final String datalessRegionColor)
	{
		properties().put("datalessRegionColor", datalessRegionColor);
	}

	public String getDefaultColor()
	{
		return properties().get("defaultColor", "#267114");
	}

	public void setDefaultColor(final String defaultColor)
	{
		properties().put("defaultColor", defaultColor);
	}

	public DisplayMode getDisplayMode()
	{
		return properties().get("displayMode", DisplayMode.AUTO);
	}

	public void setDisplayMode(final DisplayMode displayMode)
	{
		properties().put("displayMode", displayMode);
	}

	public String getDomain()
	{
		return properties().get("domain", null);
	}

	public void setDomain(final String domain)
	{
		properties().put("domain", domain);
	}

	public RegionInteractivity getRegionInteractivity()
	{
		return properties().get("enableRegionInteractivity", RegionInteractivity.AUTO);
	}

	public void setRegionInteractivity(final RegionInteractivity regionInteractivity)
	{
		properties().put("enableRegionInteractivity",
			regionInteractivity == RegionInteractivity.AUTO ? null : regionInteractivity);
	}

	public boolean getKeepAspectRatio()
	{
		return properties().get("keepAspectRatio", true);
	}

	public void setKeepAspectRatio(final boolean keepAspectRatio)
	{
		properties().put("keepAspectRatio", keepAspectRatio);
	}

	public Legend getLegend()
	{
		return properties().get("legend", null);
	}

	public void setLegend(final Legend legend)
	{
		properties().put("legend", legend);
	}

	public String getRegion()
	{
		return properties().get("region", "world");
	}

	public void setRegion(final String region)
	{
		properties().put("region", region);
	}

	public MagnifyingGlass getMagnifyingGlass()
	{
		return properties().get("magnifyingGlass", null);
	}

	public void setMagnifyingGlass(final MagnifyingGlass magnifyingGlass)
	{
		properties().put("magnifyingGlass", magnifyingGlass);
	}

	public double getMarkerOpacity()
	{
		return properties().get("markerOpacity", 1.0);
	}

	public void setMarkerOpacity(final double markerOpacity)
	{
		properties().put("markerOpacity", markerOpacity);
	}

	public Resolution getResolution()
	{
		return properties().get("resolution", Resolution.COUNTRIES);
	}

	public void setResolution(final Resolution resolution)
	{
		properties().put("resolution", resolution);
	}

	@Override
	public void showSampleData()
	{
		getModel().removeAll()
			.addColumn(Column.New(Column.Type.STRING, "Country"))
			.addColumn(Column.New(Column.Type.NUMBER, "Popularity"))
			.addRow("Germany", 200)
			.addRow("United States", 300)
			.addRow("Brazil", 400)
			.addRow("Canada", 500)
			.addRow("France", 600)
			.addRow("RU", 700);
	}
}
