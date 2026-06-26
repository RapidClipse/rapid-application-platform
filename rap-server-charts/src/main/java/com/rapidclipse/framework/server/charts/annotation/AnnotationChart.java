/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
 */
package com.rapidclipse.framework.server.charts.annotation;

import java.time.LocalDate;
import java.util.List;

import com.rapidclipse.framework.server.charts.AbstractChart;
import com.rapidclipse.framework.server.charts.AllowsHtml;
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
public class AnnotationChart extends AbstractChart
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
		return properties().get("allValuesSuffix", null);
	}

	public void setAllValuesSuffix(final String allValuesSuffix)
	{
		properties().put("allValuesSuffix", allValuesSuffix);
	}

	public Number getAnnotationsWidth()
	{
		return properties().get("annotationsWidth", 25);
	}

	public void setAnnotationsWidth(final Number annotationsWidth)
	{
		properties().put("annotationsWidth", annotationsWidth);
	}

	public String getDateFormat()
	{
		return properties().get("dateFormat", null);
	}

	public void setDateFormat(final String dateFormat)
	{
		properties().put("dateFormat", dateFormat);
	}

	public boolean getDisplayAnnotations()
	{
		return properties().get("displayAnnotations", true);
	}

	public void setDisplayAnnotations(final boolean displayAnnotations)
	{
		properties().put("displayAnnotations", displayAnnotations);
	}

	public boolean getDisplayAnnotationsFilter()
	{
		return properties().get("displayAnnotationsFilter", false);
	}

	public void setDisplayAnnotationsFilter(final boolean displayAnnotationsFilter)
	{
		properties().put("displayAnnotationsFilter", displayAnnotationsFilter);
	}

	public boolean getDisplayDateBarSeparator()
	{
		return properties().get("displayDateBarSeparator", true);
	}

	public void setDisplayDateBarSeparator(final boolean displayDateBarSeparator)
	{
		properties().put("displayDateBarSeparator", displayDateBarSeparator);
	}

	public boolean getDisplayExactValues()
	{
		return properties().get("displayExactValues", false);
	}

	public void setDisplayExactValues(final boolean displayExactValues)
	{
		properties().put("displayExactValues", displayExactValues);
	}

	public boolean getDisplayLegendDots()
	{
		return properties().get("displayLegendDots", true);
	}

	public void setDisplayLegendDots(final boolean displayLegendDots)
	{
		properties().put("displayLegendDots", displayLegendDots);
	}

	public boolean getDisplayLegendValues()
	{
		return properties().get("displayLegendValues", true);
	}

	public void setDisplayLegendValues(final boolean displayLegendValues)
	{
		properties().put("displayLegendValues", displayLegendValues);
	}

	public boolean getDisplayRangeSelector()
	{
		return properties().get("displayRangeSelector", true);
	}

	public void setDisplayRangeSelector(final boolean displayRangeSelector)
	{
		properties().put("displayRangeSelector", displayRangeSelector);
	}

	public boolean getDisplayZoomButtons()
	{
		return properties().get("displayZoomButtons", true);
	}

	public void setDisplayZoomButtons(final boolean displayZoomButtons)
	{
		properties().put("displayZoomButtons", displayZoomButtons);
	}

	public Number getFill()
	{
		return properties().get("fill", 0);
	}

	public void setFill(final Number fill)
	{
		properties().put("fill", fill);
	}

	public LegendPosition getLegendPosition()
	{
		return properties().get("legendPosition", LegendPosition.SAME_ROW);
	}

	public void setLegendPosition(final LegendPosition legendPosition)
	{
		properties().put("legendPosition", legendPosition);
	}

	public NumberFormats getNumberFormats()
	{
		return properties().get("numberFormats", null);
	}

	public void setNumberFormats(final NumberFormats numberFormats)
	{
		properties().put("numberFormats", numberFormats);
	}

	public List<Number> getScaleColumns()
	{
		return properties().get("scaleColumns", null);
	}

	public void setScaleColumns(final List<Number> scaleColumns)
	{
		properties().put("scaleColumns", scaleColumns);
	}

	public String getScaleFormat()
	{
		return properties().get("scaleFormat", "#");
	}

	public void setScaleFormat(final String scaleFormat)
	{
		properties().put("scaleFormat", scaleFormat);
	}

	public ScaleType getScaleType()
	{
		return properties().get("scaleType", ScaleType.FIXED);
	}

	public void setScaleType(final ScaleType scaleType)
	{
		properties().put("scaleType", scaleType);
	}

	public Table getTable()
	{
		return properties().get("table", null);
	}

	public void setTable(final Table tableOptions)
	{
		properties().put("table", tableOptions);
	}

	public Number getThickness()
	{
		return properties().get("thickness", 0);
	}

	public void setThickness(final Number thickness)
	{
		properties().put("thickness", thickness);
	}

	public DateValue getZoomEndTime()
	{
		return properties().get("zoomEndTime", null);
	}

	public void setZoomEndTime(final DateValue zoomEndTime)
	{
		properties().put("zoomEndTime", zoomEndTime);
	}

	public DateValue getZoomStartTime()
	{
		return properties().get("zoomStartTime", null);
	}

	public void setZoomStartTime(final DateValue zoomStartTime)
	{
		properties().put("zoomStartTime", zoomStartTime);
	}

	@Override
	public void showSampleData()
	{
		getModel().removeAll()
			.addColumn(Column.New(Column.Type.DATE, "Date"))
			.addColumn(Column.New(Column.Type.NUMBER, "Kepler-22b mission"))
			.addColumn(Column.New(Column.Type.STRING, "Kepler title"))
			.addColumn(Column.New(Column.Type.STRING, "Kepler text"))
			.addColumn(Column.New(Column.Type.NUMBER, "Gliese 163 mission"))
			.addColumn(Column.New(Column.Type.STRING, "Gliese title"))
			.addColumn(Column.New(Column.Type.STRING, "Gliese text"))
			.addRow(LocalDate.of(2314, 2, 15), 12400, null, null, 10645, null, null)
			.addRow(LocalDate.of(2314, 2, 16), 24045, "Lalibertines", "First encounter", 12374, null, null)
			.addRow(LocalDate.of(2314, 2, 17), 35022, "Lalibertines", "They are very tall", 15766, "Gallantors",
				"First Encounter")
			.addRow(LocalDate.of(2314, 2, 18), 12284, "Lalibertines", "Attack on our crew!", 34334, "Gallantors",
				"Statement of shared principles")
			.addRow(LocalDate.of(2314, 2, 19), 8476, "Lalibertines", "Heavy casualties", 66467, "Gallantors",
				"Mysteries revealed")
			.addRow(LocalDate.of(2314, 2, 20), 0, "Lalibertines", "All crew lost", 79463, "Gallantors",
				"Omniscience achieved");

		setDisplayAnnotations(true);
	}
}
