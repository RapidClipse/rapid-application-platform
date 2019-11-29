
package com.rapidclipse.framework.server.charts.line;

import com.rapidclipse.framework.server.charts.AllowsIFrame;
import com.rapidclipse.framework.server.charts.CanInterpolateNulls;
import com.rapidclipse.framework.server.charts.ChartBase;
import com.rapidclipse.framework.server.charts.ChartModel;
import com.rapidclipse.framework.server.charts.Column;
import com.rapidclipse.framework.server.charts.HasAggregationTarget;
import com.rapidclipse.framework.server.charts.HasAnimation;
import com.rapidclipse.framework.server.charts.HasAnnotations;
import com.rapidclipse.framework.server.charts.HasAxisTitlesPosition;
import com.rapidclipse.framework.server.charts.HasBackground;
import com.rapidclipse.framework.server.charts.HasCategories;
import com.rapidclipse.framework.server.charts.HasChartArea;
import com.rapidclipse.framework.server.charts.HasChartSize;
import com.rapidclipse.framework.server.charts.HasColors;
import com.rapidclipse.framework.server.charts.HasCrosshair;
import com.rapidclipse.framework.server.charts.HasCurveType;
import com.rapidclipse.framework.server.charts.HasDataOpacity;
import com.rapidclipse.framework.server.charts.HasExplorer;
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
import com.rapidclipse.framework.server.charts.HasTheme;
import com.rapidclipse.framework.server.charts.HasTitlePosition;
import com.rapidclipse.framework.server.charts.HasTooltip;
import com.rapidclipse.framework.server.charts.HasTrendlines;
import com.rapidclipse.framework.server.charts.HasVAxes;
import com.vaadin.flow.component.Tag;


/**
 * @author XDEV Software
 *
 */
@Tag("line-chart")
public class LineChart extends ChartBase
	implements HasAggregationTarget, HasAnimation, HasAnnotations, HasAxisTitlesPosition, HasBackground, HasChartArea,
	HasColors, HasCrosshair, HasCurveType, HasDataOpacity, HasInteractivity, HasExplorer, HasFocusTarget, HasFont,
	AllowsIFrame, HasHAxis, CanInterpolateNulls, HasLegend, HasLineDashStyle, HasOrientation, HasPoints, HasCategories,
	HasSelectionMode, HasTheme, HasTitlePosition, HasTooltip, HasTrendlines, HasVAxes, HasChartSize,
	HasIntervals, HasSeries<LineSeries>
{
	public LineChart()
	{
		super("LineChart");
	}
	
	public ChartModel initDefaultColumnsDiscrete(final String axisColumn, final String... valueColumns)
	{
		final ChartModel model = getModel().removeAll()
			.addColumn(Column.New(Column.Type.STRING, axisColumn));
		for(final String valueColumn : valueColumns)
		{
			model.addColumn(Column.New(Column.Type.NUMBER, valueColumn));
		}
		return model;
	}
	
	public ChartModel
		initDefaultColumnsContinuous(
			final String axisColumn,
			final Column.Type axisColumnType,
			final String... valueColumns)
	{
		validateColumnType(axisColumnType, "axis column", Column.Type.NUMBER, Column.Type.DATE, Column.Type.DATE_TIME,
			Column.Type.TIME_OF_DAY);
		
		final ChartModel model = getModel().removeAll()
			.addColumn(Column.New(axisColumnType, axisColumn));
		for(final String valueColumn : valueColumns)
		{
			model.addColumn(Column.New(Column.Type.NUMBER, valueColumn));
		}
		return model;
	}
}
