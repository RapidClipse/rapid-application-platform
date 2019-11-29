
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
		return properties().get("datalessRegionColor");
	}

	public void setDatalessRegionColor(final String datalessRegionColor)
	{
		properties().put("datalessRegionColor", datalessRegionColor);
	}

	public String getDefaultColor()
	{
		return properties().get("defaultColor");
	}

	public void setDefaultColor(final String defaultColor)
	{
		properties().put("defaultColor", defaultColor);
	}

	public DisplayMode getDisplayMode()
	{
		return properties().get("displayMode");
	}

	public void setDisplayMode(final DisplayMode displayMode)
	{
		properties().put("displayMode", displayMode);
	}

	public String getDomain()
	{
		return properties().get("domain");
	}

	public void setDomain(final String domain)
	{
		properties().put("domain", domain);
	}

	public Boolean getEnableRegionInteractivity()
	{
		return properties().get("enableRegionInteractivity");
	}

	public void setEnableRegionInteractivity(final Boolean enableRegionInteractivity)
	{
		properties().put("enableRegionInteractivity", enableRegionInteractivity);
	}

	public Boolean getKeepAspectRatio()
	{
		return properties().get("keepAspectRatio");
	}

	public void setKeepAspectRatio(final Boolean keepAspectRatio)
	{
		properties().put("keepAspectRatio", keepAspectRatio);
	}

	public Legend getLegend()
	{
		return properties().get("legend");
	}

	public void setLegend(final Legend legend)
	{
		properties().put("legend", legend);
	}

	public String getRegion()
	{
		return properties().get("region");
	}

	public void setRegion(final String region)
	{
		properties().put("region", region);
	}

	public MagnifyingGlass getMagnifyingGlass()
	{
		return properties().get("magnifyingGlass");
	}

	public void setMagnifyingGlass(final MagnifyingGlass magnifyingGlass)
	{
		properties().put("magnifyingGlass", magnifyingGlass);
	}

	public Number getMarkerOpacity()
	{
		return properties().get("markerOpacity");
	}

	public void setMarkerOpacity(final Number markerOpacity)
	{
		properties().put("markerOpacity", markerOpacity);
	}

	public Resolution getResolution()
	{
		return properties().get("resolution");
	}

	public void setResolution(final Resolution resolution)
	{
		properties().put("resolution", resolution);
	}
}
