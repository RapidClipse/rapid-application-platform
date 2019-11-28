
package com.rapidclipse.framework.server.charts.gantt;

import java.io.Serializable;

import com.rapidclipse.framework.server.charts.DateValue;
import com.rapidclipse.framework.server.charts.TextStyle;
import com.rapidclipse.framework.server.util.JavaScriptable;


/**
 * @author XDEV Software
 *
 */
public interface Gantt extends Serializable, JavaScriptable
{
	public Arrow arrow();

	public Double barCornerRadius();

	public Double barHeight();

	public Boolean criticalPathEnabled();

	public LineStyle criticalPathStyle();

	public DateValue defaultStartDate();

	public LineStyle innerGridHorizLine();

	public BarStyle innerGridTrack();

	public BarStyle innerGridDarkTrack();

	public Double labelMaxWidth();

	public TextStyle labelStyle();

	public Boolean percentEnabled();

	public BarStyle percentStyle();

	public Boolean shadowEnabled();

	public String shadowColor();

	public Double shadowOffset();

	public Double trackHeight();

	public static Builder Builder()
	{
		return new Builder.Default();
	}

	public static interface Builder
	{
		public Builder arrow(Arrow arrow);

		public Builder barCornerRadius(Double barCornerRadius);

		public Builder barHeight(Double barHeight);

		public Builder criticalPathEnabled(Boolean criticalPathEnabled);

		public Builder criticalPathStyle(LineStyle criticalPathStyle);

		public Builder defaultStartDate(DateValue defaultStartDate);

		public Builder innerGridHorizLine(LineStyle innerGridHorizLine);

		public Builder innerGridTrack(BarStyle innerGridTrack);

		public Builder innerGridDarkTrack(BarStyle innerGridDarkTrack);

		public Builder labelMaxWidth(Double labelMaxWidth);

		public Builder labelStyle(TextStyle labelStyle);

		public Builder percentEnabled(Boolean percentEnabled);

		public Builder percentStyle(BarStyle percentStyle);

		public Builder shadowEnabled(Boolean shadowEnabled);

		public Builder shadowColor(String shadowColor);

		public Builder shadowOffset(Double shadowOffset);

		public Builder trackHeight(Double trackHeight);

		public Gantt build();

		public static class Default implements Builder
		{
			private Arrow     arrow;
			private Double    barCornerRadius;
			private Double    barHeight;
			private Boolean   criticalPathEnabled;
			private LineStyle criticalPathStyle;
			private DateValue defaultStartDate;
			private LineStyle innerGridHorizLine;
			private BarStyle  innerGridTrack;
			private BarStyle  innerGridDarkTrack;
			private Double    labelMaxWidth;
			private TextStyle labelStyle;
			private Boolean   percentEnabled;
			private BarStyle  percentStyle;
			private Boolean   shadowEnabled;
			private String    shadowColor;
			private Double    shadowOffset;
			private Double    trackHeight;

			Default()
			{
				super();
			}

			@Override
			public Builder arrow(final Arrow arrow)
			{
				this.arrow = arrow;
				return this;
			}

			@Override
			public Builder barCornerRadius(final Double barCornerRadius)
			{
				this.barCornerRadius = barCornerRadius;
				return this;
			}

			@Override
			public Builder barHeight(final Double barHeight)
			{
				this.barHeight = barHeight;
				return this;
			}

			@Override
			public Builder criticalPathEnabled(final Boolean criticalPathEnabled)
			{
				this.criticalPathEnabled = criticalPathEnabled;
				return this;
			}

			@Override
			public Builder criticalPathStyle(final LineStyle criticalPathStyle)
			{
				this.criticalPathStyle = criticalPathStyle;
				return this;
			}

			@Override
			public Builder defaultStartDate(final DateValue defaultStartDate)
			{
				this.defaultStartDate = defaultStartDate;
				return this;
			}

			@Override
			public Builder innerGridHorizLine(final LineStyle innerGridHorizLine)
			{
				this.innerGridHorizLine = innerGridHorizLine;
				return this;
			}

			@Override
			public Builder innerGridTrack(final BarStyle innerGridTrack)
			{
				this.innerGridTrack = innerGridTrack;
				return this;
			}

			@Override
			public Builder innerGridDarkTrack(final BarStyle innerGridDarkTrack)
			{
				this.innerGridDarkTrack = innerGridDarkTrack;
				return this;
			}

			@Override
			public Builder labelMaxWidth(final Double labelMaxWidth)
			{
				this.labelMaxWidth = labelMaxWidth;
				return this;
			}

			@Override
			public Builder labelStyle(final TextStyle labelStyle)
			{
				this.labelStyle = labelStyle;
				return this;
			}

			@Override
			public Builder percentEnabled(final Boolean percentEnabled)
			{
				this.percentEnabled = percentEnabled;
				return this;
			}

			@Override
			public Builder percentStyle(final BarStyle percentStyle)
			{
				this.percentStyle = percentStyle;
				return this;
			}

			@Override
			public Builder shadowEnabled(final Boolean shadowEnabled)
			{
				this.shadowEnabled = shadowEnabled;
				return this;
			}

			@Override
			public Builder shadowColor(final String shadowColor)
			{
				this.shadowColor = shadowColor;
				return this;
			}

			@Override
			public Builder shadowOffset(final Double shadowOffset)
			{
				this.shadowOffset = shadowOffset;
				return this;
			}

			@Override
			public Builder trackHeight(final Double trackHeight)
			{
				this.trackHeight = trackHeight;
				return this;
			}

			@Override
			public Gantt build()
			{
				return new Gantt.Default(this.arrow, this.barCornerRadius, this.barHeight, this.criticalPathEnabled,
					this.criticalPathStyle, this.defaultStartDate, this.innerGridHorizLine, this.innerGridTrack,
					this.innerGridDarkTrack, this.labelMaxWidth, this.labelStyle, this.percentEnabled,
					this.percentStyle, this.shadowEnabled, this.shadowColor, this.shadowOffset, this.trackHeight);
			}

		}

	}

	public final static class Default implements Gantt
	{
		private final Arrow     arrow;
		private final Double    barCornerRadius;
		private final Double    barHeight;
		private final Boolean   criticalPathEnabled;
		private final LineStyle criticalPathStyle;
		private final DateValue defaultStartDate;
		private final LineStyle innerGridHorizLine;
		private final BarStyle  innerGridTrack;
		private final BarStyle  innerGridDarkTrack;
		private final Double    labelMaxWidth;
		private final TextStyle labelStyle;
		private final Boolean   percentEnabled;
		private final BarStyle  percentStyle;
		private final Boolean   shadowEnabled;
		private final String    shadowColor;
		private final Double    shadowOffset;
		private final Double    trackHeight;

		Default(
			final Arrow arrow,
			final Double barCornerRadius,
			final Double barHeight,
			final Boolean criticalPathEnabled,
			final LineStyle criticalPathStyle,
			final DateValue defaultStartDate,
			final LineStyle innerGridHorizLine,
			final BarStyle innerGridTrack,
			final BarStyle innerGridDarkTrack,
			final Double labelMaxWidth,
			final TextStyle labelStyle,
			final Boolean percentEnabled,
			final BarStyle percentStyle,
			final Boolean shadowEnabled,
			final String shadowColor,
			final Double shadowOffset,
			final Double trackHeight)
		{
			super();

			this.arrow               = arrow;
			this.barCornerRadius     = barCornerRadius;
			this.barHeight           = barHeight;
			this.criticalPathEnabled = criticalPathEnabled;
			this.criticalPathStyle   = criticalPathStyle;
			this.defaultStartDate    = defaultStartDate;
			this.innerGridHorizLine  = innerGridHorizLine;
			this.innerGridTrack      = innerGridTrack;
			this.innerGridDarkTrack  = innerGridDarkTrack;
			this.labelMaxWidth       = labelMaxWidth;
			this.labelStyle          = labelStyle;
			this.percentEnabled      = percentEnabled;
			this.percentStyle        = percentStyle;
			this.shadowEnabled       = shadowEnabled;
			this.shadowColor         = shadowColor;
			this.shadowOffset        = shadowOffset;
			this.trackHeight         = trackHeight;
		}

		@Override
		public Arrow arrow()
		{
			return this.arrow;
		}

		@Override
		public Double barCornerRadius()
		{
			return this.barCornerRadius;
		}

		@Override
		public Double barHeight()
		{
			return this.barHeight;
		}

		@Override
		public Boolean criticalPathEnabled()
		{
			return this.criticalPathEnabled;
		}

		@Override
		public LineStyle criticalPathStyle()
		{
			return this.criticalPathStyle;
		}

		@Override
		public DateValue defaultStartDate()
		{
			return this.defaultStartDate;
		}

		@Override
		public LineStyle innerGridHorizLine()
		{
			return this.innerGridHorizLine;
		}

		@Override
		public BarStyle innerGridTrack()
		{
			return this.innerGridTrack;
		}

		@Override
		public BarStyle innerGridDarkTrack()
		{
			return this.innerGridDarkTrack;
		}

		@Override
		public Double labelMaxWidth()
		{
			return this.labelMaxWidth;
		}

		@Override
		public TextStyle labelStyle()
		{
			return this.labelStyle;
		}

		@Override
		public Boolean percentEnabled()
		{
			return this.percentEnabled;
		}

		@Override
		public BarStyle percentStyle()
		{
			return this.percentStyle;
		}

		@Override
		public Boolean shadowEnabled()
		{
			return this.shadowEnabled;
		}

		@Override
		public String shadowColor()
		{
			return this.shadowColor;
		}

		@Override
		public Double shadowOffset()
		{
			return this.shadowOffset;
		}

		@Override
		public Double trackHeight()
		{
			return this.trackHeight;
		}

		@Override
		public String js()
		{
			final ObjectHelper obj = new ObjectHelper();
			obj.putIfNotNull("arrow", this.arrow);
			obj.putIfNotNull("barCornerRadius", this.barCornerRadius);
			obj.putIfNotNull("barHeight", this.barHeight);
			obj.putIfNotNull("criticalPathEnabled", this.criticalPathEnabled);
			obj.putIfNotNull("criticalPathStyle", this.criticalPathStyle);
			obj.putIfNotNull("defaultStartDate", this.defaultStartDate);
			obj.putIfNotNull("innerGridHorizLine", this.innerGridHorizLine);
			obj.putIfNotNull("innerGridTrack", this.innerGridTrack);
			obj.putIfNotNull("innerGridDarkTrack", this.innerGridDarkTrack);
			obj.putIfNotNull("labelMaxWidth", this.labelMaxWidth);
			obj.putIfNotNull("labelStyle", this.labelStyle);
			obj.putIfNotNull("percentEnabled", this.percentEnabled);
			obj.putIfNotNull("percentStyle", this.percentStyle);
			obj.putIfNotNull("shadowEnabled", this.shadowEnabled);
			obj.putIfNotNull("shadowColor", this.shadowColor);
			obj.putIfNotNull("shadowOffset", this.shadowOffset);
			obj.putIfNotNull("trackHeight", this.trackHeight);
			return obj.js();
		}

	}

}