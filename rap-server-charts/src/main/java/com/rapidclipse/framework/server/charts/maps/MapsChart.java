/*
 * Copyright (C) 2013-2023 by XDEV Software, All Rights Reserved.
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

import java.util.List;

import com.rapidclipse.framework.server.charts.ChartModel;
import com.rapidclipse.framework.server.charts.Column;
import com.rapidclipse.framework.server.charts.HasLineColor;
import com.vaadin.flow.component.Tag;


/**
 * @author XDEV Software
 * @since 10.02.00
 *
 */
@Tag("maps-chart")
public class MapsChart extends AbstractMapsChart
	implements HasLineColor
{
	public final static String MAP_TYPE_NORMAL    = "normal";
	public final static String MAP_TYPE_TERRAIN   = "terrain";
	public final static String MAP_TYPE_SATELLITE = "satellite";
	public final static String MAP_TYPE_HYBRID    = "hybrid";
	
	public MapsChart()
	{
		super("Map", "map");
	}
	
	public ChartModel initDefaultColumnsLatLong()
	{
		return getModel().removeAll()
			.addColumn(Column.New(Column.Type.STRING, "Latitude"))
			.addColumn(Column.New(Column.Type.STRING, "Longitude"));
	}
	
	public ChartModel initDefaultColumnsLatLongDescription()
	{
		return initDefaultColumnsLatLong()
			.addColumn(Column.New(Column.Type.STRING, "Description"));
	}
	
	public ChartModel initDefaultColumnsAddress()
	{
		return getModel().removeAll()
			.addColumn(Column.New(Column.Type.STRING, "Address"));
	}
	
	public ChartModel initDefaultColumnsAddressDescription()
	{
		return initDefaultColumnsAddress()
			.addColumn(Column.New(Column.Type.STRING, "Description"));
	}
	
	public void addIcon(final String marker, final Icon icon)
	{
		properties().putIndexed("icons", marker, icon);
	}
	
	public Icon removeIcon(final String marker)
	{
		return properties().removeIndexed("icons", marker);
	}
	
	public void removeAllIcons()
	{
		properties().removeAllIndexed("icons");
	}
	
	public void addMap(final String mapId, final Map map)
	{
		properties().putIndexed("maps", mapId, map);
	}
	
	public Icon removeMap(final String mapId)
	{
		return properties().removeIndexed("maps", mapId);
	}
	
	public void removeAllMaps()
	{
		properties().removeAllIndexed("maps");
	}
	
	public boolean getEnableScrollWheel()
	{
		return properties().get("enableScrollWheel", false);
	}
	
	public void setEnableScrollWheel(final boolean enableScrollWheel)
	{
		properties().put("enableScrollWheel", enableScrollWheel);
	}
	
	public String getMapType()
	{
		return properties().get("mapType", "hybrid");
	}
	
	public void setMapType(final String mapType)
	{
		properties().put("mapType", mapType);
	}
	
	public List<String> getMapTypeIds()
	{
		return properties().get("mapTypeIds", null);
	}
	
	public void setMapTypeIds(final List<String> mapTypeIds)
	{
		properties().put("mapTypeIds", mapTypeIds);
	}
	
	public boolean getShowInfoWindow()
	{
		return properties().get("showInfoWindow", false);
	}
	
	public void setShowInfoWindow(final boolean showInfoWindow)
	{
		properties().put("showInfoWindow", showInfoWindow);
	}
	
	public boolean getShowLine()
	{
		return properties().get("showLine", false);
	}
	
	public void setShowLine(final boolean showLine)
	{
		properties().put("showLine", showLine);
	}
	
	public boolean getShowTooltip()
	{
		return properties().get("showTooltip", false);
	}
	
	public void setShowTooltip(final boolean showTooltip)
	{
		properties().put("showTooltip", showTooltip);
	}
	
	public boolean getUseMapTypeControl()
	{
		return properties().get("useMapTypeControl", false);
	}
	
	public void setUseMapTypeControl(final boolean useMapTypeControl)
	{
		properties().put("useMapTypeControl", useMapTypeControl);
	}
	
	public Number getZoomLevel()
	{
		return properties().get("zoomLevel", null);
	}
	
	public void setZoomLevel(final Number zoomLevel)
	{
		properties().put("zoomLevel", zoomLevel);
	}

	@Override
	public Number getLineWidth()
	{
		return properties().get("lineWidth", 10);
	}
	
	@Override
	public void showSampleData()
	{
		initDefaultColumnsAddressDescription()
			.addRow("China", "China: 1,363,800,000")
			.addRow("India", "India: 1,242,620,000")
			.addRow("US", "US: 317,842,000")
			.addRow("Indonesia", "Indonesia: 247,424,598")
			.addRow("Brazil", "Brazil: 201,032,714")
			.addRow("Pakistan", "Pakistan: 186,134,000")
			.addRow("Nigeria", "Nigeria: 173,615,000")
			.addRow("Bangladesh", "Bangladesh: 152,518,015")
			.addRow("Russia", "Russia: 146,019,512")
			.addRow("Japan", "Japan: 127,120,000");
		
		setShowTooltip(true);
		setShowInfoWindow(true);
	}
}
