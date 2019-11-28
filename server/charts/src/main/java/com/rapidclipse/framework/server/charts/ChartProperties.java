
package com.rapidclipse.framework.server.charts;

import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import java.util.stream.Collectors;


/**
 * @author XDEV Software
 *
 */
public class ChartProperties
{
	public static void main(final String[] args)
	{
		final Map<String, List<String>> map = createMap();

		final List<String> properties = map.values().stream()
			.flatMap(List::stream)
			.distinct()
			.sorted()
			.collect(Collectors.toList());

		final Map<String, Integer> propertyCount       =
			properties.stream().collect(Collectors.toMap(p -> p, p -> count(map, p)));
		final Map<String, Integer> singlePropertyCount = propertyCount.entrySet().stream()
			.filter(e -> e.getValue().intValue() == 1)
			.collect(Collectors.toMap(e -> e.getKey(), e -> e.getValue(), (k, v) -> k, TreeMap::new));
		// final Map<String, Integer> multiPropertyCount = propertyCount.entrySet().stream()
		// .filter(e -> e.getValue().intValue() > 1)
		// .collect(Collectors.toMap(e -> e.getKey(), e -> e.getValue(), (k, v) -> k, TreeMap::new));

		map.entrySet().forEach(e -> {
			System.out.println(e.getKey());
			System.out.println(e.getValue().stream().filter(singlePropertyCount::containsKey)
				.collect(Collectors.joining(", ")));
			System.out.println();
		});
	}

	static int count(final Map<String, List<String>> map, final String property)
	{
		return (int)map.values().stream()
			.filter(list -> list.contains(property))
			.count();
	}

	static Map<String, List<String>> createMap()
	{
		final Map<String, List<String>> map = new HashMap<>();

		map.put("Annotation",
			Arrays.asList("allowHtml", "allValuesSuffix", "annotationsWidth", "colors", "dateFormat",
				"displayAnnotations", "displayAnnotationsFilter", "displayDateBarSeparator", "displayExactValues",
				"displayLegendDots", "displayLegendValues", "displayRangeSelector", "displayZoomButtons", "fill",
				"legendPosition", "max", "min", "numberFormats", "scaleColumns", "scaleFormat", "scaleType", "table",
				"thickness", "zoomEndTime", "zoomStartTime"));

		map.put("Area",
			Arrays.asList("aggregationTarget", "animation", "annotations", "areaOpacity", "axisTitlesPosition",
				"backgroundColor", "chartArea", "colors", "crosshair", "dataOpacity", "enableInteractivity", "explorer",
				"focusTarget", "fontSize", "fontName", "forceIFrame", "hAxis", "height", "interpolateNulls",
				"isStacked", "legend", "lineDashStyle", "lineWidth", "orientation", "pointShape", "pointSize",
				"pointsVisible", "reverseCategories", "selectionMode", "series", "theme", "title", "titlePosition",
				"titleTextStyle", "tooltip", "vAxes", "vAxis", "width"));

		map.put("Bar",
			Arrays.asList("animation", "annotations", "axisTitlesPosition", "backgroundColor", "bar", "bars",
				"chartArea", "colors", "dataOpacity", "enableInteractivity", "explorer", "focusTarget", "fontSize",
				"fontName", "forceIFrame", "hAxes", "hAxis", "height", "isStacked", "legend", "orientation",
				"reverseCategories", "series", "theme", "title", "titlePosition", "titleTextStyle", "tooltip",
				"trendlines", "vAxis", "width"));

		map.put("Bubble",
			Arrays.asList("animation", "axisTitlesPosition", "backgroundColor", "bubble", "chartArea", "colors",
				"colorAxis", "enableInteractivity", "explorer", "fontSize", "fontName", "forceIFrame", "hAxis",
				"height", "legend", "selectionMode", "series", "sizeAxis", "sortBubblesBySize", "theme", "title",
				"titlePosition", "titleTextStyle", "tooltip", "vAxis", "width"));

		map.put("Calendar",
			Arrays.asList("calendar", "colorAxis", "forceIFrame", "height", "noDataPattern", "tooltip", "width"));

		map.put("Candlestick",
			Arrays.asList("aggregationTarget", "animation", "axisTitlesPosition", "backgroundColor", "bar",
				"candlestick", "chartArea", "colors", "enableInteractivity", "focusTarget", "fontSize", "fontName",
				"forceIFrame", "hAxis", "height", "legend", "orientation", "reverseCategories", "selectionMode",
				"series", "theme", "title", "titlePosition", "titleTextStyle", "tooltip", "vAxes", "vAxis", "width"));

		map.put("Column",
			Arrays.asList("animation", "annotations", "axisTitlesPosition", "backgroundColor", "bar", "bars",
				"chartArea", "colors", "dataOpacity", "enableInteractivity", "explorer", "focusTarget", "fontSize",
				"fontName", "forceIFrame", "hAxis", "height", "isStacked", "legend", "orientation", "reverseCategories",
				"series", "theme", "title", "titlePosition", "titleTextStyle", "tooltip", "trendlines", "vAxes",
				"vAxis", "width"));

		map.put("Combo",
			Arrays.asList("aggregationTarget", "animation", "annotations", "areaOpacity", "axisTitlesPosition",
				"backgroundColor", "bar", "candlestick", "chartArea", "colors", "crosshair", "curveType",
				"dataOpacity", "enableInteractivity", "focusTarget", "fontSize", "fontName", "forceIFrame", "hAxis",
				"height", "interpolateNulls", "isStacked", "legend", "lineDashStyle", "lineWidth", "orientation",
				"pointShape", "pointSize", "pointsVisible", "reverseCategories", "selectionMode", "series",
				"seriesType", "theme", "title", "titlePosition", "titleTextStyle", "tooltip", "vAxes", "vAxis",
				"width"));

		map.put("Gantt",
			Arrays.asList("backgroundColor.fill", "gantt", "width", "height"));

		map.put("Gauge",
			Arrays.asList("animation", "forceIFrame", "greenColor", "greenFrom", "greenTo", "height", "majorTicks",
				"max", "min", "minorTicks", "redColor", "redFrom", "redTo", "width", "yellowColor", "yellowFrom",
				"yellowTo"));

		map.put("Geo",
			Arrays.asList("backgroundColor", "colorAxis", "datalessRegionColor", "defaultColor", "displayMode",
				"domain", "enableRegionInteractivity", "forceIFrame", "height", "keepAspectRatio", "legend", "region",
				"magnifyingGlass", "markerOpacity", "resolution", "sizeAxis", "tooltip", "width"));

		map.put("Histogram",
			Arrays.asList("animation", "axisTitlesPosition", "backgroundColor", "bar",
				"chartArea", "colors", "dataOpacity", "enableInteractivity", "focusTarget", "fontSize",
				"fontName", "forceIFrame", "hAxis", "histogram", "height", "interpolateNulls", "isStacked", "legend",
				"orientation", "reverseCategories", "series", "theme", "title", "titlePosition", "titleTextStyle",
				"tooltip", "vAxes", "vAxis", "width"));

		map.put("Line",
			Arrays.asList("aggregationTarget", "animation", "annotations", "axisTitlesPosition", "backgroundColor",
				"chartArea", "colors", "crosshair", "curveType", "dataOpacity", "enableInteractivity", "explorer",
				"focusTarget", "fontSize", "fontName", "forceIFrame", "hAxis", "height", "interpolateNulls", "legend",
				"lineDashStyle", "lineWidth", "orientation", "pointShape", "pointSize", "pointsVisible",
				"reverseCategories", "selectionMode", "series", "theme", "title", "titlePosition", "titleTextStyle",
				"tooltip", "trendlines", "vAxes", "vAxis", "width"));

		map.put("Maps",
			Arrays.asList("enableScrollWheel", "icons", "lineColor", "lineWidth", "maps", "mapType", "mapTypeIds",
				"showInfoWindow", "showLine", "showTooltip", "useMapTypeControl", "zoomLevel"));

		map.put("Org",
			Arrays.asList("allowCollapse", "allowHtml", "color", "nodeClass", "selectedNodeClass", "selectionColor",
				"size"));

		map.put("Pie",
			Arrays.asList("backgroundColor", "chartArea", "colors", "enableInteractivity", "fontSize", "fontName",
				"forceIFrame", "height", "is3D", "legend", "pieHole", "pieSliceBorderColor", "pieSliceText",
				"pieSliceTextStyle", "pieStartAngle", "reverseCategories", "pieResidueSliceColor",
				"pieResidueSliceLabel", "slices", "sliceVisibilityThreshold", "title", "titleTextStyle", "tooltip",
				"width"));

		map.put("Sankey",
			Arrays.asList("forceIFrame", "height", "sankey", "tooltip", "width"));

		map.put("Scatter",
			Arrays.asList("aggregationTarget", "animation", "annotations", "axisTitlesPosition", "backgroundColor",
				"chartArea", "colors", "crosshair", "curveType", "dataOpacity", "enableInteractivity", "explorer",
				"fontSize", "fontName", "forceIFrame", "hAxis", "height", "legend", "lineWidth", "orientation",
				"pointShape", "pointSize", "pointsVisible", "selectionMode", "series", "theme", "title",
				"titlePosition", "titleTextStyle", "tooltip", "trendlines", "vAxis", "width"));

		map.put("SteppedArea",
			Arrays.asList("aggregationTarget", "animation", "areaOpacity", "axisTitlesPosition", "backgroundColor",
				"chartArea", "colors", "connectSteps", "enableInteractivity", "focusTarget", "fontSize", "fontName",
				"forceIFrame", "hAxis", "height", "isStacked", "legend", "lineDashStyle", "reverseCategories",
				"selectionMode", "series", "theme", "title", "titlePosition", "titleTextStyle", "tooltip", "vAxes",
				"vAxis", "width"));

		map.put("Table",
			Arrays.asList("allowHtml", "alternatingRowStyle", "cssClassNames", "firstRowNumber", "frozenColumns",
				"height", "page", "pageSize", "pagingButtons", "rtlTable", "scrollLeftStartPosition", "showRowNumber",
				"sort", "sortAscending", "sortColumn", "startPage", "width"));

		map.put("Timeline",
			Arrays.asList("avoidOverlappingGridLines", "backgroundColor", "colors", "enableInteractivity", "fontName",
				"fontSize", "forceIFrame", "height", "timeline", "tooltip", "width"));

		map.put("TreeMap",
			Arrays.asList("fontColor", "fontFamily", "fontSize", "forceIFrame", "headerColor", "headerHeight",
				"headerHighlightColor", "highlightOnMouseOver", "hintOpacity", "maxColor", "maxDepth",
				"maxHighlightColor", "maxPostDepth", "maxColorValue", "midColor", "midHighlightColor", "minColor",
				"minHighlightColor", "minColorValue", "noColor", "noHighlightColor", "showScale", "showTooltips",
				"textStyle", "title", "titleTextStyle", "useWeightedAverageForAggregation"));

		map.put("WordTree",
			Arrays.asList("colors", "forceIFrame", "fontName", "height", "maxFontSize", "width", "wordtree"));

		return map;
	}
}
