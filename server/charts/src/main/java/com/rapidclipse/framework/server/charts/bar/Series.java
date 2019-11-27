
package com.rapidclipse.framework.server.charts.bar;

import java.io.Serializable;

import com.rapidclipse.framework.server.charts.Annotations;
import com.rapidclipse.framework.server.util.JavaScriptable;


/**
 * @author XDEV Software
 *
 */
public interface Series extends Serializable, JavaScriptable
{
	public Annotations annotations();
	
	public String color();
	
	public Boolean labelInLegend();
	
	public Integer targetAxisIndex();
	
	public Boolean visibleInLegend();

	public static Builder Builder()
	{
		return new Builder.Default();
	}

	public static interface Builder
	{
		public Builder annotations(Annotations annotations);
		
		public Builder color(String color);
		
		public Builder labelInLegend(Boolean labelInLegend);
		
		public Builder targetAxisIndex(Integer targetAxisIndex);
		
		public Builder visibleInLegend(Boolean visibleInLegend);
		
		public Series build();
		
		public static class Default implements Builder
		{
			private Annotations annotations;
			private String      color;
			private Boolean     labelInLegend;
			private Integer     targetAxisIndex;
			private Boolean     visibleInLegend;
			
			Default()
			{
				super();
			}
			
			@Override
			public Builder annotations(final Annotations annotations)
			{
				this.annotations = annotations;
				return this;
			}
			
			@Override
			public Builder color(final String color)
			{
				this.color = color;
				return this;
			}
			
			@Override
			public Builder labelInLegend(final Boolean labelInLegend)
			{
				this.labelInLegend = labelInLegend;
				return this;
			}
			
			@Override
			public Builder targetAxisIndex(final Integer targetAxisIndex)
			{
				this.targetAxisIndex = targetAxisIndex;
				return this;
			}
			
			@Override
			public Builder visibleInLegend(final Boolean visibleInLegend)
			{
				this.visibleInLegend = visibleInLegend;
				return this;
			}
			
			@Override
			public Series build()
			{
				return new Series.Default(this.annotations, this.color, this.labelInLegend,
					this.targetAxisIndex, this.visibleInLegend);
			}

		}

	}
	
	public static class Default implements Series
	{
		private final Annotations annotations;
		private final String      color;
		private final Boolean     labelInLegend;
		private final Integer     targetAxisIndex;
		private final Boolean     visibleInLegend;

		Default(
			final Annotations annotations,
			final String color,
			final Boolean labelInLegend,
			final Integer targetAxisIndex,
			final Boolean visibleInLegend)
		{
			super();
			
			this.annotations     = annotations;
			this.color           = color;
			this.labelInLegend   = labelInLegend;
			this.targetAxisIndex = targetAxisIndex;
			this.visibleInLegend = visibleInLegend;
		}

		@Override
		public Annotations annotations()
		{
			return this.annotations;
		}

		@Override
		public String color()
		{
			return this.color;
		}
		
		@Override
		public Boolean labelInLegend()
		{
			return this.labelInLegend;
		}
		
		@Override
		public Integer targetAxisIndex()
		{
			return this.targetAxisIndex;
		}
		
		@Override
		public Boolean visibleInLegend()
		{
			return this.visibleInLegend;
		}

		@Override
		public String js()
		{
			final ObjectHelper obj = new ObjectHelper();
			obj.putIfNotNull("annotations", this.annotations);
			obj.putIfNotNull("color", this.color);
			obj.putIfNotNull("labelInLegend", this.labelInLegend);
			obj.putIfNotNull("targetAxisIndex", this.targetAxisIndex);
			obj.putIfNotNull("visibleInLegend", this.visibleInLegend);
			return obj.js();
		}
		
	}
	
}
