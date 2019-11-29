
package com.rapidclipse.framework.server.charts.bar;

import com.rapidclipse.framework.server.charts.AllowsIFrame;
import com.rapidclipse.framework.server.charts.ChartBase;
import com.rapidclipse.framework.server.charts.ChartModel;
import com.rapidclipse.framework.server.charts.Column;
import com.rapidclipse.framework.server.charts.DiffChart;
import com.rapidclipse.framework.server.charts.HasAnimation;
import com.rapidclipse.framework.server.charts.HasAnnotations;
import com.rapidclipse.framework.server.charts.HasAxisTitlesPosition;
import com.rapidclipse.framework.server.charts.HasBackground;
import com.rapidclipse.framework.server.charts.HasBar;
import com.rapidclipse.framework.server.charts.HasCategories;
import com.rapidclipse.framework.server.charts.HasChartArea;
import com.rapidclipse.framework.server.charts.HasChartSize;
import com.rapidclipse.framework.server.charts.HasDataOpacity;
import com.rapidclipse.framework.server.charts.HasFocusTarget;
import com.rapidclipse.framework.server.charts.HasHAxis;
import com.rapidclipse.framework.server.charts.HasInteractivity;
import com.rapidclipse.framework.server.charts.HasIntervals;
import com.rapidclipse.framework.server.charts.HasLegend;
import com.rapidclipse.framework.server.charts.HasOrientation;
import com.rapidclipse.framework.server.charts.HasSeries;
import com.rapidclipse.framework.server.charts.HasStackMode;
import com.rapidclipse.framework.server.charts.HasTitlePosition;
import com.rapidclipse.framework.server.charts.HasTrendlines;
import com.rapidclipse.framework.server.charts.HasVAxis;


/**
 * @author XDEV Software
 *
 */
public abstract class AbstractBarChart extends ChartBase
	implements HasAnimation, HasAnnotations, HasAxisTitlesPosition, HasBackground, HasBar, HasChartArea, HasDataOpacity,
	HasInteractivity, HasFocusTarget, AllowsIFrame, HasHAxis, HasStackMode, HasLegend, HasCategories, HasOrientation,
	HasTitlePosition, HasTrendlines, HasVAxis, HasChartSize, DiffChart, HasIntervals, HasSeries<BarSeries>
{
	protected AbstractBarChart(final String type, final String... packages)
	{
		super(type, packages);
	}

	@Override
	public void setModel(final ChartModel before, final ChartModel after)
	{
		super.setModel(before, after);
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
