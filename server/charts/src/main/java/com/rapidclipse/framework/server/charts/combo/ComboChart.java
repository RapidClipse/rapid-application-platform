
package com.rapidclipse.framework.server.charts.combo;

import com.rapidclipse.framework.server.charts.AllowsIFrame;
import com.rapidclipse.framework.server.charts.CanInterpolateNulls;
import com.rapidclipse.framework.server.charts.ChartBase;
import com.rapidclipse.framework.server.charts.ChartModel;
import com.rapidclipse.framework.server.charts.Column;
import com.rapidclipse.framework.server.charts.HasAggregationTarget;
import com.rapidclipse.framework.server.charts.HasAnimation;
import com.rapidclipse.framework.server.charts.HasAnnotations;
import com.rapidclipse.framework.server.charts.HasAreaOpacity;
import com.rapidclipse.framework.server.charts.HasAxisTitlesPosition;
import com.rapidclipse.framework.server.charts.HasBackground;
import com.rapidclipse.framework.server.charts.HasBar;
import com.rapidclipse.framework.server.charts.HasCandlestick;
import com.rapidclipse.framework.server.charts.HasCategories;
import com.rapidclipse.framework.server.charts.HasChartArea;
import com.rapidclipse.framework.server.charts.HasChartSize;
import com.rapidclipse.framework.server.charts.HasColors;
import com.rapidclipse.framework.server.charts.HasCrosshair;
import com.rapidclipse.framework.server.charts.HasCurveType;
import com.rapidclipse.framework.server.charts.HasDataOpacity;
import com.rapidclipse.framework.server.charts.HasFocusTarget;
import com.rapidclipse.framework.server.charts.HasFont;
import com.rapidclipse.framework.server.charts.HasHAxis;
import com.rapidclipse.framework.server.charts.HasInteractivity;
import com.rapidclipse.framework.server.charts.HasIntervals;
import com.rapidclipse.framework.server.charts.HasLegend;
import com.rapidclipse.framework.server.charts.HasLineDashStyle;
import com.rapidclipse.framework.server.charts.HasOrientation;
import com.rapidclipse.framework.server.charts.HasPoints;
import com.rapidclipse.framework.server.charts.HasSelectionMode;
import com.rapidclipse.framework.server.charts.HasSeries;
import com.rapidclipse.framework.server.charts.HasStackMode;
import com.rapidclipse.framework.server.charts.HasTheme;
import com.rapidclipse.framework.server.charts.HasTitlePosition;
import com.rapidclipse.framework.server.charts.HasTooltip;
import com.rapidclipse.framework.server.charts.HasVAxes;
import com.vaadin.flow.component.Tag;


/**
 * @author XDEV Software
 *
 */
@Tag("combo-chart")
public class ComboChart extends ChartBase
	implements HasAggregationTarget, HasAnimation, HasAnnotations, HasAreaOpacity, HasAxisTitlesPosition, HasBackground,
	HasBar, HasCandlestick, HasChartArea, HasColors, HasCrosshair, HasCurveType, HasDataOpacity, HasInteractivity,
	HasFocusTarget, HasFont, AllowsIFrame, HasHAxis, HasChartSize, CanInterpolateNulls, HasStackMode, HasLegend,
	HasLineDashStyle, HasOrientation, HasPoints, HasCategories, HasSelectionMode, HasTheme, HasTitlePosition,
	HasTooltip, HasVAxes, HasIntervals, HasSeries<ComboSeries>
{
	public ComboChart()
	{
		super("ComboChart");
	}

	public ChartModel initDefaultColumnsDiscrete(final String xAxisColumn, final String... valueColumns)
	{
		final ChartModel model = getModel().removeAll()
			.addColumn(Column.New(Column.Type.STRING, xAxisColumn));
		for(final String valueColumn : valueColumns)
		{
			model.addColumn(Column.New(Column.Type.NUMBER, valueColumn));
		}
		return model;
	}

	public ChartModel
		initDefaultColumnsContinuous(
			final String xAxisColumn,
			final Column.Type xAxisColumnType,
			final String... valueColumns)
	{
		validateColumnType(xAxisColumnType, "axis column", Column.Type.NUMBER, Column.Type.DATE, Column.Type.DATE_TIME,
			Column.Type.TIME_OF_DAY);

		final ChartModel model = getModel().removeAll()
			.addColumn(Column.New(xAxisColumnType, xAxisColumn));
		for(final String valueColumn : valueColumns)
		{
			model.addColumn(Column.New(Column.Type.NUMBER, valueColumn));
		}
		return model;
	}

	public SeriesType getSeriesType()
	{
		return properties().get("seriesType");
	}
	
	public void setSeriesType(final SeriesType seriesType)
	{
		properties().put("seriesType", seriesType);
	}
}
