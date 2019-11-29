
package com.rapidclipse.framework.server.charts;

import java.io.Serializable;
import java.util.Collections;
import java.util.List;

import com.rapidclipse.framework.server.util.JavaScriptable;

import elemental.json.Json;


/**
 * @author XDEV Software
 *
 */
public interface Axis extends Serializable, JavaScriptable
{
	public static enum Direction implements JavaScriptable
	{
		REVERSE("-1"),
		DEFAULT("1");
		
		private final String js;
		
		private Direction(final String js)
		{
			this.js = js;
		}
		
		@Override
		public String js()
		{
			return this.js;
		}
	}
	
	public static enum ScaleType implements JavaScriptable
	{
		LOG("log"),
		MIRROR_LOG("mirrorLog");
		
		private final String js;
		
		private ScaleType(final String js)
		{
			this.js = Json.create(js).toJson();
		}
		
		@Override
		public String js()
		{
			return this.js;
		}
	}
	
	public static enum ViewWindowMode implements JavaScriptable
	{
		PRETTY("pretty"),
		MAXIMIZED("maximized"),
		EXPLICIT("explicit");
		
		private final String js;
		
		private ViewWindowMode(final String js)
		{
			this.js = Json.create(js).toJson();
		}
		
		@Override
		public String js()
		{
			return this.js;
		}
	}
	
	public Integer baseline();
	
	public String baselineColor();
	
	public Direction direction();
	
	public String format();
	
	public GridLines gridlines();
	
	public Boolean logScale();
	
	public GridLines minorGridlines();
	
	public ScaleType scaleType();
	
	public TextPosition textPosition();
	
	public TextStyle textStyle();
	
	public List<Tick> ticks();
	
	public String title();
	
	public TextStyle titleTextStyle();
	
	public Boolean allowContainerBoundaryTextCufoff();
	
	public Boolean slantedText();
	
	public Integer maxAlternation();
	
	public Integer maxTextLines();
	
	public Number minTextSpacing();
	
	public Integer showTextEvery();
	
	public Number maxValue();
	
	public Number minValue();
	
	public ViewWindowMode viewWindowMode();
	
	public Number viewWindowMax();
	
	public Number viewWindowMin();
	
	public static Axis New(final String title)
	{
		return Builder().title(title).build();
	}
	
	public static Builder Builder()
	{
		return new Builder.Default();
	}
	
	public static interface Builder
	{
		public Builder baseline(Integer baseline);
		
		public Builder baselineColor(String baselineColor);
		
		public Builder direction(Direction direction);
		
		public Builder format(String format);
		
		public Builder gridlines(GridLines gridlines);
		
		public Builder logScale(Boolean logScale);
		
		public Builder minorGridlines(GridLines minorGridlines);
		
		public Builder scaleType(ScaleType scaleType);
		
		public Builder textPosition(TextPosition textPosition);
		
		public Builder textStyle(TextStyle textStyle);
		
		public Builder ticks(List<Tick> ticks);
		
		public Builder title(String title);
		
		public Builder titleTextStyle(TextStyle titleTextStyle);
		
		public Builder allowContainerBoundaryTextCufoff(Boolean allowContainerBoundaryTextCufoff);
		
		public Builder slantedText(Boolean slantedText);
		
		public Builder maxAlternation(Integer maxAlternation);
		
		public Builder maxTextLines(Integer maxTextLines);
		
		public Builder minTextSpacing(Number minTextSpacing);
		
		public Builder minTextSpacing(Integer showTextEvery);
		
		public Builder maxValue(Number maxValue);
		
		public Builder minValue(Number minValue);
		
		public Builder viewWindowMode(ViewWindowMode viewWindowMode);
		
		public Builder viewWindowMax(Number viewWindowMax);
		
		public Builder viewWindowMin(Number viewWindowMin);
		
		public Axis build();
		
		public static class Default implements Builder
		{
			private Integer        baseline;
			private String         baselineColor;
			private Direction      direction;
			private String         format;
			private GridLines      gridlines;
			private Boolean        logScale;
			private GridLines      minorGridlines;
			private ScaleType      scaleType;
			private TextPosition   textPosition;
			private TextStyle      textStyle;
			private List<Tick>     ticks;
			private String         title;
			private TextStyle      titleTextStyle;
			private Boolean        allowContainerBoundaryTextCufoff;
			private Boolean        slantedText;
			private Integer        maxAlternation;
			private Integer        maxTextLines;
			private Number         minTextSpacing;
			private Integer        showTextEvery;
			private Number         maxValue;
			private Number         minValue;
			private ViewWindowMode viewWindowMode;
			private Number         viewWindowMax;
			private Number         viewWindowMin;
			
			Default()
			{
				super();
			}
			
			@Override
			public Builder baseline(final Integer baseline)
			{
				this.baseline = baseline;
				return this;
			}
			
			@Override
			public Builder baselineColor(final String baselineColor)
			{
				this.baselineColor = baselineColor;
				return this;
			}
			
			@Override
			public Builder direction(final Direction direction)
			{
				this.direction = direction;
				return this;
			}
			
			@Override
			public Builder format(final String format)
			{
				this.format = format;
				return this;
			}
			
			@Override
			public Builder gridlines(final GridLines gridlines)
			{
				this.gridlines = gridlines;
				return this;
			}
			
			@Override
			public Builder logScale(final Boolean logScale)
			{
				this.logScale = logScale;
				return this;
			}
			
			@Override
			public Builder minorGridlines(final GridLines minorGridlines)
			{
				this.minorGridlines = minorGridlines;
				return this;
			}
			
			@Override
			public Builder scaleType(final ScaleType scaleType)
			{
				this.scaleType = scaleType;
				return this;
			}
			
			@Override
			public Builder textPosition(final TextPosition textPosition)
			{
				this.textPosition = textPosition;
				return this;
			}
			
			@Override
			public Builder textStyle(final TextStyle textStyle)
			{
				this.textStyle = textStyle;
				return this;
			}
			
			@Override
			public Builder ticks(final List<Tick> ticks)
			{
				this.ticks = ticks;
				return this;
			}
			
			@Override
			public Builder title(final String title)
			{
				this.title = title;
				return this;
			}
			
			@Override
			public Builder titleTextStyle(final TextStyle titleTextStyle)
			{
				this.titleTextStyle = titleTextStyle;
				return this;
			}
			
			@Override
			public Builder allowContainerBoundaryTextCufoff(final Boolean allowContainerBoundaryTextCufoff)
			{
				this.allowContainerBoundaryTextCufoff = allowContainerBoundaryTextCufoff;
				return this;
			}
			
			@Override
			public Builder slantedText(final Boolean slantedText)
			{
				this.slantedText = slantedText;
				return this;
			}
			
			@Override
			public Builder maxAlternation(final Integer maxAlternation)
			{
				this.maxAlternation = maxAlternation;
				return this;
			}
			
			@Override
			public Builder maxTextLines(final Integer maxTextLines)
			{
				this.maxTextLines = maxTextLines;
				return this;
			}
			
			@Override
			public Builder minTextSpacing(final Number minTextSpacing)
			{
				this.minTextSpacing = minTextSpacing;
				return this;
			}
			
			@Override
			public Builder minTextSpacing(final Integer showTextEvery)
			{
				this.showTextEvery = showTextEvery;
				return this;
			}
			
			@Override
			public Builder maxValue(final Number maxValue)
			{
				this.maxValue = maxValue;
				return this;
			}
			
			@Override
			public Builder minValue(final Number minValue)
			{
				this.minValue = minValue;
				return this;
			}
			
			@Override
			public Builder viewWindowMode(final ViewWindowMode viewWindowMode)
			{
				this.viewWindowMode = viewWindowMode;
				return this;
			}
			
			@Override
			public Builder viewWindowMax(final Number viewWindowMax)
			{
				this.viewWindowMax = viewWindowMax;
				return this;
			}
			
			@Override
			public Builder viewWindowMin(final Number viewWindowMin)
			{
				this.viewWindowMin = viewWindowMin;
				return this;
			}
			
			@Override
			public Axis build()
			{
				return new Axis.Default(this.baseline, this.baselineColor, this.direction, this.format, this.gridlines,
					this.logScale, this.minorGridlines, this.scaleType, this.textPosition, this.textStyle, this.ticks,
					this.title, this.titleTextStyle, this.allowContainerBoundaryTextCufoff, this.slantedText,
					this.maxAlternation, this.maxTextLines, this.minTextSpacing, this.showTextEvery, this.maxValue,
					this.minValue, this.viewWindowMode, this.viewWindowMax, this.viewWindowMin);
			}
		}
		
	}
	
	public static class Default implements Axis
	{
		private final Integer        baseline;
		private final String         baselineColor;
		private final Direction      direction;
		private final String         format;
		private final GridLines      gridlines;
		private final Boolean        logScale;
		private final GridLines      minorGridlines;
		private final ScaleType      scaleType;
		private final TextPosition   textPosition;
		private final TextStyle      textStyle;
		private final List<Tick>     ticks;
		private final String         title;
		private final TextStyle      titleTextStyle;
		private final Boolean        allowContainerBoundaryTextCufoff;
		private final Boolean        slantedText;
		private final Integer        maxAlternation;
		private final Integer        maxTextLines;
		private final Number         minTextSpacing;
		private final Integer        showTextEvery;
		private final Number         maxValue;
		private final Number         minValue;
		private final ViewWindowMode viewWindowMode;
		private final Number         viewWindowMax;
		private final Number         viewWindowMin;
		
		Default(
			final Integer baseline,
			final String baselineColor,
			final Direction direction,
			final String format,
			final GridLines gridlines,
			final Boolean logScale,
			final GridLines minorGridlines,
			final ScaleType scaleType,
			final TextPosition textPosition,
			final TextStyle textStyle,
			final List<Tick> ticks,
			final String title,
			final TextStyle titleTextStyle,
			final Boolean allowContainerBoundaryTextCufoff,
			final Boolean slantedText,
			final Integer maxAlternation,
			final Integer maxTextLines,
			final Number minTextSpacing,
			final Integer showTextEvery,
			final Number maxValue,
			final Number minValue,
			final ViewWindowMode viewWindowMode,
			final Number viewWindowMax,
			final Number viewWindowMin)
		{
			super();
			
			this.baseline                         = baseline;
			this.baselineColor                    = baselineColor;
			this.direction                        = direction;
			this.format                           = format;
			this.gridlines                        = gridlines;
			this.logScale                         = logScale;
			this.minorGridlines                   = minorGridlines;
			this.scaleType                        = scaleType;
			this.textPosition                     = textPosition;
			this.textStyle                        = textStyle;
			this.ticks                            = ticks != null ? Collections.unmodifiableList(ticks) : null;
			this.title                            = title;
			this.titleTextStyle                   = titleTextStyle;
			this.allowContainerBoundaryTextCufoff = allowContainerBoundaryTextCufoff;
			this.slantedText                      = slantedText;
			this.maxAlternation                   = maxAlternation;
			this.maxTextLines                     = maxTextLines;
			this.minTextSpacing                   = minTextSpacing;
			this.showTextEvery                    = showTextEvery;
			this.maxValue                         = maxValue;
			this.minValue                         = minValue;
			this.viewWindowMode                   = viewWindowMode;
			this.viewWindowMax                    = viewWindowMax;
			this.viewWindowMin                    = viewWindowMin;
		}
		
		@Override
		public Integer baseline()
		{
			return this.baseline;
		}
		
		@Override
		public String baselineColor()
		{
			return this.baselineColor;
		}
		
		@Override
		public Direction direction()
		{
			return this.direction;
		}
		
		@Override
		public String format()
		{
			return this.format;
		}
		
		@Override
		public GridLines gridlines()
		{
			return this.gridlines;
		}
		
		@Override
		public Boolean logScale()
		{
			return this.logScale;
		}
		
		@Override
		public GridLines minorGridlines()
		{
			return this.minorGridlines;
		}
		
		@Override
		public ScaleType scaleType()
		{
			return this.scaleType;
		}
		
		@Override
		public TextPosition textPosition()
		{
			return this.textPosition;
		}
		
		@Override
		public TextStyle textStyle()
		{
			return this.textStyle;
		}
		
		@Override
		public List<Tick> ticks()
		{
			return this.ticks;
		}
		
		@Override
		public String title()
		{
			return this.title;
		}
		
		@Override
		public TextStyle titleTextStyle()
		{
			return this.titleTextStyle;
		}
		
		@Override
		public Boolean allowContainerBoundaryTextCufoff()
		{
			return this.allowContainerBoundaryTextCufoff;
		}
		
		@Override
		public Boolean slantedText()
		{
			return this.slantedText;
		}
		
		@Override
		public Integer maxAlternation()
		{
			return this.maxAlternation;
		}
		
		@Override
		public Integer maxTextLines()
		{
			return this.maxTextLines;
		}
		
		@Override
		public Number minTextSpacing()
		{
			return this.minTextSpacing;
		}
		
		@Override
		public Integer showTextEvery()
		{
			return this.showTextEvery;
		}
		
		@Override
		public Number maxValue()
		{
			return this.maxValue;
		}
		
		@Override
		public Number minValue()
		{
			return this.minValue;
		}
		
		@Override
		public ViewWindowMode viewWindowMode()
		{
			return this.viewWindowMode;
		}
		
		@Override
		public Number viewWindowMax()
		{
			return this.viewWindowMax;
		}
		
		@Override
		public Number viewWindowMin()
		{
			return this.viewWindowMin;
		}
		
		@Override
		public String js()
		{
			final ObjectHelper obj = new ObjectHelper();
			obj.putIfNotNull("baseline", this.baseline);
			obj.putIfNotNull("baselineColor", this.baselineColor);
			obj.putIfNotNull("direction", this.direction);
			obj.putIfNotNull("format", this.format);
			obj.putIfNotNull("gridlines", this.gridlines);
			obj.putIfNotNull("logScale", this.logScale);
			obj.putIfNotNull("minorGridlines", this.minorGridlines);
			obj.putIfNotNull("scaleType", this.scaleType);
			obj.putIfNotNull("textPosition", this.textPosition);
			obj.putIfNotNull("textStyle", this.textStyle);
			obj.putIfNotNull("ticks", new ArrayHelper().addAllScriptables(this.ticks));
			obj.putIfNotNull("title", this.title);
			obj.putIfNotNull("titleTextStyle", this.titleTextStyle);
			obj.putIfNotNull("allowContainerBoundaryTextCufoff", this.allowContainerBoundaryTextCufoff);
			obj.putIfNotNull("slantedText", this.slantedText);
			obj.putIfNotNull("maxAlternation", this.maxAlternation);
			obj.putIfNotNull("maxTextLines", this.maxTextLines);
			obj.putIfNotNull("minTextSpacing", this.minTextSpacing);
			obj.putIfNotNull("showTextEvery", this.showTextEvery);
			obj.putIfNotNull("maxValue", this.maxValue);
			obj.putIfNotNull("minValue", this.minValue);
			obj.putIfNotNull("viewWindowMode", this.viewWindowMode);
			obj.putIfNotNull("viewWindow", new ObjectHelper()
				.putIfNotNull("min", this.viewWindowMax)
				.putIfNotNull("max", this.viewWindowMin));
			return obj.js();
		}
		
	}
	
}
