
package com.rapidclipse.framework.server.charts.annotation;

import java.util.List;

import com.rapidclipse.framework.server.charts.AllowsHtml;
import com.rapidclipse.framework.server.charts.ChartBase;
import com.rapidclipse.framework.server.charts.ChartModel;
import com.rapidclipse.framework.server.charts.Column;
import com.rapidclipse.framework.server.charts.DateValue;
import com.rapidclipse.framework.server.charts.HasColors;
import com.rapidclipse.framework.server.charts.HasValueRange;
import com.vaadin.flow.component.Tag;


/**
 * @author XDEV Software
 * @since 10.02.00
 *
 */
@Tag("annotation-chart")
public class AnnotationChart extends ChartBase
	implements AllowsHtml, HasColors, HasValueRange
{
	public AnnotationChart()
	{
		super("AnnotationChart", "annotationchart");
	}

	public ChartModel initDefaultColumns()
	{
		return initDefaultColumns(Column.Type.DATE);
	}

	public ChartModel initDefaultColumns(final Column.Type xColumnType)
	{
		return initDefaultColumns("x", xColumnType, "y", "title", "text");
	}

	public ChartModel
		initDefaultColumns(
			final String xColumn,
			final Column.Type xColumnType,
			final String yColumn,
			final String annotationTitleColumn,
			final String annotationTextColumn)
	{
		validateColumnType(xColumnType, "x column", Column.Type.DATE, Column.Type.DATE_TIME);
		
		final ChartModel model = getModel().removeAll()
			.addColumn(Column.New(xColumnType, xColumn))
			.addColumn(Column.New(Column.Type.STRING, yColumn));
		if(annotationTitleColumn != null)
		{
			model.addColumn(Column.New(Column.Type.STRING, annotationTitleColumn));
		}
		if(annotationTextColumn != null)
		{
			model.addColumn(Column.New(Column.Type.STRING, annotationTextColumn));
		}
		return model;
	}

	public String getAllValuesSuffix()
	{
		return properties().get("allValuesSuffix");
	}

	public void setAllValuesSuffix(final String allValuesSuffix)
	{
		properties().put("allValuesSuffix", allValuesSuffix);
	}

	public Number getAnnotationsWidth()
	{
		return properties().get("annotationsWidth");
	}

	public void setAnnotationsWidth(final Number annotationsWidth)
	{
		properties().put("annotationsWidth", annotationsWidth);
	}

	public String getDateFormat()
	{
		return properties().get("dateFormat");
	}

	public void setDateFormat(final String dateFormat)
	{
		properties().put("dateFormat", dateFormat);
	}

	public Boolean getDisplayAnnotations()
	{
		return properties().get("displayAnnotations");
	}

	public void setDisplayAnnotations(final Boolean displayAnnotations)
	{
		properties().put("displayAnnotations", displayAnnotations);
	}

	public Boolean getDisplayAnnotationsFilter()
	{
		return properties().get("displayAnnotationsFilter");
	}

	public void setDisplayAnnotationsFilter(final Boolean displayAnnotationsFilter)
	{
		properties().put("displayAnnotationsFilter", displayAnnotationsFilter);
	}

	public Boolean getDisplayDateBarSeparator()
	{
		return properties().get("displayDateBarSeparator");
	}

	public void setDisplayDateBarSeparator(final Boolean displayDateBarSeparator)
	{
		properties().put("displayDateBarSeparator", displayDateBarSeparator);
	}

	public Boolean getDisplayExactValues()
	{
		return properties().get("displayExactValues");
	}

	public void setDisplayExactValues(final Boolean displayExactValues)
	{
		properties().put("displayExactValues", displayExactValues);
	}

	public Boolean getDisplayLegendDots()
	{
		return properties().get("displayLegendDots");
	}

	public void setDisplayLegendDots(final Boolean displayLegendDots)
	{
		properties().put("displayLegendDots", displayLegendDots);
	}

	public Boolean getDisplayLegendValues()
	{
		return properties().get("displayLegendValues");
	}

	public void setDisplayLegendValues(final Boolean displayLegendValues)
	{
		properties().put("displayLegendValues", displayLegendValues);
	}

	public Boolean getDisplayRangeSelector()
	{
		return properties().get("displayRangeSelector");
	}

	public void setDisplayRangeSelector(final Boolean displayRangeSelector)
	{
		properties().put("displayRangeSelector", displayRangeSelector);
	}

	public Boolean getDisplayZoomButtons()
	{
		return properties().get("displayZoomButtons");
	}

	public void setDisplayZoomButtons(final Boolean displayZoomButtons)
	{
		properties().put("displayZoomButtons", displayZoomButtons);
	}

	public Number getFill()
	{
		return properties().get("fill");
	}

	public void setFill(final Number fill)
	{
		properties().put("fill", fill);
	}

	public LegendPosition getLegendPosition()
	{
		return properties().get("legendPosition");
	}

	public void setLegendPosition(final LegendPosition legendPosition)
	{
		properties().put("legendPosition", legendPosition);
	}

	public NumberFormats getNumberFormats()
	{
		return properties().get("numberFormats");
	}

	public void setNumberFormats(final NumberFormats numberFormats)
	{
		properties().put("numberFormats", numberFormats);
	}

	public List<Number> getScaleColumns()
	{
		return properties().get("scaleColumns");
	}

	public void setScaleColumns(final List<Number> scaleColumns)
	{
		properties().put("scaleColumns", scaleColumns);
	}

	public String getScaleFormat()
	{
		return properties().get("scaleFormat");
	}

	public void setScaleFormat(final String scaleFormat)
	{
		properties().put("scaleFormat", scaleFormat);
	}

	public ScaleType getScaleType()
	{
		return properties().get("scaleType");
	}

	public void setScaleType(final ScaleType scaleType)
	{
		properties().put("scaleType", scaleType);
	}

	public Table getTable()
	{
		return properties().get("table");
	}

	public void setTable(final Table tableOptions)
	{
		properties().put("table", tableOptions);
	}

	public Number getThickness()
	{
		return properties().get("thickness");
	}

	public void setThickness(final Number thickness)
	{
		properties().put("thickness", thickness);
	}

	public DateValue getZoomEndTime()
	{
		return properties().get("zoomEndTime");
	}

	public void setZoomEndTime(final DateValue zoomEndTime)
	{
		properties().put("zoomEndTime", zoomEndTime);
	}

	public DateValue getZoomStartTime()
	{
		return properties().get("zoomStartTime");
	}

	public void setZoomStartTime(final DateValue zoomStartTime)
	{
		properties().put("zoomStartTime", zoomStartTime);
	}
}
