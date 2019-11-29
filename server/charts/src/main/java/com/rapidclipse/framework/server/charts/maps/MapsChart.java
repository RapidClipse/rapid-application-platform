
package com.rapidclipse.framework.server.charts.maps;

import java.util.List;

import com.rapidclipse.framework.server.charts.ChartModel;
import com.rapidclipse.framework.server.charts.Column;
import com.rapidclipse.framework.server.charts.HasLineColor;
import com.vaadin.flow.component.Tag;


/**
 * @author XDEV Software
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
			.addColumn(Column.New(Column.Type.STRING, "latitude"))
			.addColumn(Column.New(Column.Type.STRING, "longitude"));
	}

	public ChartModel initDefaultColumnsLatLongDescription()
	{
		return initDefaultColumnsLatLong()
			.addColumn(Column.New(Column.Type.STRING, "description"));
	}

	public ChartModel initDefaultColumnsAddress()
	{
		return getModel().removeAll()
			.addColumn(Column.New(Column.Type.STRING, "latitude"))
			.addColumn(Column.New(Column.Type.STRING, "longitude"));
	}

	public ChartModel initDefaultColumnsAddressDescription()
	{
		return initDefaultColumnsAddress()
			.addColumn(Column.New(Column.Type.STRING, "description"));
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

	public Boolean getEnableScrollWheel()
	{
		return properties().get("enableScrollWheel");
	}

	public void setEnableScrollWheel(final Boolean enableScrollWheel)
	{
		properties().put("enableScrollWheel", enableScrollWheel);
	}

	public String getMapType()
	{
		return properties().get("mapType");
	}

	public void setMapType(final String mapType)
	{
		properties().put("mapType", mapType);
	}

	public List<String> getMapTypeIds()
	{
		return properties().get("mapTypeIds");
	}

	public void setMapTypeIds(final List<String> mapTypeIds)
	{
		properties().put("mapTypeIds", mapTypeIds);
	}

	public Boolean getShowInfoWindow()
	{
		return properties().get("showInfoWindow");
	}

	public void setShowInfoWindow(final Boolean showInfoWindow)
	{
		properties().put("showInfoWindow", showInfoWindow);
	}

	public Boolean getShowLine()
	{
		return properties().get("showLine");
	}

	public void setShowLine(final Boolean showLine)
	{
		properties().put("showLine", showLine);
	}

	public Boolean getShowTooltip()
	{
		return properties().get("showTooltip");
	}

	public void setShowTooltip(final Boolean showTooltip)
	{
		properties().put("showTooltip", showTooltip);
	}

	public Boolean getUseMapTypeControl()
	{
		return properties().get("useMapTypeControl");
	}

	public void setUseMapTypeControl(final Boolean useMapTypeControl)
	{
		properties().put("useMapTypeControl", useMapTypeControl);
	}

	public Number getZoomLevel()
	{
		return properties().get("zoomLevel");
	}

	public void setZoomLevel(final Number zoomLevel)
	{
		properties().put("zoomLevel", zoomLevel);
	}
}
