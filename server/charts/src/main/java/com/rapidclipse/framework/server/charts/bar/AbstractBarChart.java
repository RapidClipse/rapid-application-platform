
package com.rapidclipse.framework.server.charts.bar;

import java.util.LinkedHashMap;
import java.util.Map;

import com.rapidclipse.framework.server.charts.Animation;
import com.rapidclipse.framework.server.charts.Annotations;
import com.rapidclipse.framework.server.charts.Area;
import com.rapidclipse.framework.server.charts.Axis;
import com.rapidclipse.framework.server.charts.AxisTitlesPosition;
import com.rapidclipse.framework.server.charts.Background;
import com.rapidclipse.framework.server.charts.Chart;
import com.rapidclipse.framework.server.charts.ChartModel;
import com.rapidclipse.framework.server.charts.Column;
import com.rapidclipse.framework.server.charts.FocusTarget;
import com.rapidclipse.framework.server.charts.Legend;
import com.rapidclipse.framework.server.charts.Orientation;
import com.rapidclipse.framework.server.charts.StackMode;
import com.rapidclipse.framework.server.charts.TextPosition;
import com.rapidclipse.framework.server.util.JavaScriptable.ObjectHelper;


/**
 * @author XDEV Software
 *
 */
public abstract class AbstractBarChart<T extends AbstractBarChart<T>> extends Chart
{
	private Animation                     animation;
	private Annotations                   annotations;
	private AxisTitlesPosition            axisTitlesPosition;
	private Background                    background;
	private String                        barGroupWidth;
	private Area                          chartArea;
	private Double                        dataOpacity;
	private Boolean                       enableInteractivity;
	private FocusTarget                   focusTarget;
	private Boolean                       forceIFrame;
	private Axis                          hAxis;
	private StackMode                     stackMode;
	private Legend                        legend;
	private Boolean                       reverseCategories;
	private Orientation                   orientation;
	private final Map<Integer, Series>    series     = new LinkedHashMap<>();
	private TextPosition                  titlePosition;
	private final Map<Integer, Trendline> trendlines = new LinkedHashMap<>();
	private Axis                          vAxis;
	
	protected AbstractBarChart(final String type, final String... packages)
	{
		super(type, packages);
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
		final ChartModel model = getModel().removeAll()
			.addColumn(Column.New(axisColumnType, axisColumn));
		for(final String valueColumn : valueColumns)
		{
			model.addColumn(Column.New(Column.Type.NUMBER, valueColumn));
		}
		return model;
	}
	
	@SuppressWarnings("unchecked")
	private T $()
	{
		return (T)this;
	}
	
	public T addSeries(final int rowIndex, final Series series)
	{
		this.series.put(rowIndex, series);
		return $();
	}
	
	public Series removeSeries(final int rowIndex)
	{
		return this.series.remove(rowIndex);
	}
	
	public T removeAllSeries()
	{
		this.series.clear();
		return $();
	}
	
	public T addTrendline(final int rowIndex, final Trendline trendline)
	{
		this.trendlines.put(rowIndex, trendline);
		return $();
	}
	
	public Trendline removeTrendline(final int rowIndex)
	{
		return this.trendlines.remove(rowIndex);
	}
	
	public T removeAllTrendlines()
	{
		this.trendlines.clear();
		return $();
	}
	
	public Animation getAnimation()
	{
		return this.animation;
	}
	
	public void setAnimation(final Animation animation)
	{
		this.animation = animation;
	}
	
	public Annotations getAnnotations()
	{
		return this.annotations;
	}
	
	public void setAnnotations(final Annotations annotations)
	{
		this.annotations = annotations;
	}
	
	public AxisTitlesPosition getAxisTitlesPosition()
	{
		return this.axisTitlesPosition;
	}
	
	public void setAxisTitlesPosition(final AxisTitlesPosition axisTitlesPosition)
	{
		this.axisTitlesPosition = axisTitlesPosition;
	}
	
	public Background getBackground()
	{
		return this.background;
	}
	
	public void setBackground(final Background background)
	{
		this.background = background;
	}
	
	public String getBarGroupWidth()
	{
		return this.barGroupWidth;
	}
	
	public void setBarGroupWidth(final String barGroupWidth)
	{
		this.barGroupWidth = barGroupWidth;
	}
	
	public Area getChartArea()
	{
		return this.chartArea;
	}
	
	public void setChartArea(final Area chartArea)
	{
		this.chartArea = chartArea;
	}
	
	public Double getDataOpacity()
	{
		return this.dataOpacity;
	}
	
	public void setDataOpacity(final Double dataOpacity)
	{
		this.dataOpacity = dataOpacity;
	}
	
	public Boolean getEnableInteractivity()
	{
		return this.enableInteractivity;
	}
	
	public void setEnableInteractivity(final Boolean enableInteractivity)
	{
		this.enableInteractivity = enableInteractivity;
	}
	
	public FocusTarget getFocusTarget()
	{
		return this.focusTarget;
	}
	
	public void setFocusTarget(final FocusTarget focusTarget)
	{
		this.focusTarget = focusTarget;
	}
	
	public Boolean getForceIFrame()
	{
		return this.forceIFrame;
	}
	
	public void setForceIFrame(final Boolean forceIFrame)
	{
		this.forceIFrame = forceIFrame;
	}
	
	public Axis getHAxis()
	{
		return this.hAxis;
	}
	
	public void setHAxis(final Axis hAxis)
	{
		this.hAxis = hAxis;
	}
	
	public StackMode getStackMode()
	{
		return this.stackMode;
	}
	
	public void setStackMode(final StackMode stackMode)
	{
		this.stackMode = stackMode;
	}
	
	public Legend getLegend()
	{
		return this.legend;
	}
	
	public void setLegend(final Legend legend)
	{
		this.legend = legend;
	}
	
	public Boolean getReverseCategories()
	{
		return this.reverseCategories;
	}
	
	public void setReverseCategories(final Boolean reverseCategories)
	{
		this.reverseCategories = reverseCategories;
	}
	
	public Orientation getOrientation()
	{
		return this.orientation;
	}
	
	public void setOrientation(final Orientation orientation)
	{
		this.orientation = orientation;
	}
	
	public TextPosition getTitlePosition()
	{
		return this.titlePosition;
	}
	
	public void setTitlePosition(final TextPosition titlePosition)
	{
		this.titlePosition = titlePosition;
	}
	
	public Axis getVAxis()
	{
		return this.vAxis;
	}
	
	public void setVAxis(final Axis vAxis)
	{
		this.vAxis = vAxis;
	}
	
	@Override
	protected void createConfiguration(final ObjectHelper obj)
	{
		super.createConfiguration(obj);
		
		obj.putIfNotNull("animation", this.animation);
		obj.putIfNotNull("annotations", this.annotations);
		obj.putIfNotNull("axisTitlesPosition", this.axisTitlesPosition);
		obj.putIfNotNull("backgroundColor", this.background);
		if(this.barGroupWidth != null)
		{
			obj.putIfNotNull("bar", new ObjectHelper().put("groupWidth", this.barGroupWidth));
		}
		obj.putIfNotNull("chartArea", this.chartArea);
		obj.putIfNotNull("dataOpacity", this.dataOpacity);
		obj.putIfNotNull("enableInteractivity", this.enableInteractivity);
		obj.putIfNotNull("focusTarget", this.focusTarget);
		obj.putIfNotNull("forceIFrame", this.forceIFrame);
		obj.putIfNotNull("hAxis", this.hAxis);
		obj.putIfNotNull("isStacked", this.stackMode);
		obj.putIfNotNull("legend", this.legend);
		obj.putIfNotNull("reverseCategories", this.reverseCategories);
		obj.putIfNotNull("orientation", this.orientation);
		obj.putIfNotNull("titlePosition", this.titlePosition);
		obj.putIfNotNull("vAxis", this.vAxis);
		
		putIfNotNull(obj, "series", this.series);
		putIfNotNull(obj, "trendlines", this.trendlines);
	}
}
