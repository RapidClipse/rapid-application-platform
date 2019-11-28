
package com.rapidclipse.framework.server.charts.sankey;

import java.io.Serializable;
import java.util.List;

import com.rapidclipse.framework.server.charts.TextStyle;
import com.rapidclipse.framework.server.util.JavaScriptable;


/**
 * @author XDEV Software
 *
 */
public interface Node extends Serializable, JavaScriptable
{
	public TextStyle label();

	public Boolean interactivity();

	public Double labelPadding();

	public Double nodePadding();

	public Double width();

	public List<String> colors();
	
	public static Builder Builder()
	{
		return new Builder.Default();
	}
	
	public static interface Builder
	{
		public Builder label(TextStyle label);

		public Builder interactivity(Boolean interactivity);

		public Builder labelPadding(Double labelPadding);

		public Builder nodePadding(Double nodePadding);

		public Builder width(Double width);

		public Builder colors(List<String> colors);

		public Node build();

		public static class Default implements Builder
		{
			private TextStyle    label;
			private Boolean      interactivity;
			private Double       labelPadding;
			private Double       nodePadding;
			private Double       width;
			private List<String> colors;

			Default()
			{
				super();
			}

			@Override
			public Builder label(final TextStyle label)
			{
				this.label = label;
				return this;
			}

			@Override
			public Builder interactivity(final Boolean interactivity)
			{
				this.interactivity = interactivity;
				return this;
			}

			@Override
			public Builder labelPadding(final Double labelPadding)
			{
				this.labelPadding = labelPadding;
				return this;
			}

			@Override
			public Builder nodePadding(final Double nodePadding)
			{
				this.nodePadding = nodePadding;
				return this;
			}

			@Override
			public Builder width(final Double width)
			{
				this.width = width;
				return this;
			}

			@Override
			public Builder colors(final List<String> colors)
			{
				this.colors = colors;
				return this;
			}

			@Override
			public Node build()
			{
				return new Node.Default(this.label, this.interactivity, this.labelPadding, this.nodePadding, this.width,
					this.colors);
			}

		}

	}

	public static class Default implements Node
	{
		private final TextStyle    label;
		private final Boolean      interactivity;
		private final Double       labelPadding;
		private final Double       nodePadding;
		private final Double       width;
		private final List<String> colors;

		Default(
			final TextStyle label,
			final Boolean interactivity,
			final Double labelPadding,
			final Double nodePadding,
			final Double width,
			final List<String> colors)
		{
			super();
			
			this.label         = label;
			this.interactivity = interactivity;
			this.labelPadding  = labelPadding;
			this.nodePadding   = nodePadding;
			this.width         = width;
			this.colors        = colors;
		}

		@Override
		public TextStyle label()
		{
			return this.label;
		}

		@Override
		public Boolean interactivity()
		{
			return this.interactivity;
		}

		@Override
		public Double labelPadding()
		{
			return this.labelPadding;
		}

		@Override
		public Double nodePadding()
		{
			return this.nodePadding;
		}

		@Override
		public Double width()
		{
			return this.width;
		}

		@Override
		public List<String> colors()
		{
			return this.colors;
		}
		
		@Override
		public String js()
		{
			final ObjectHelper obj = new ObjectHelper();
			obj.putIfNotNull("label", this.label);
			obj.putIfNotNull("interactivity", this.interactivity);
			obj.putIfNotNull("labelPadding", this.labelPadding);
			obj.putIfNotNull("nodePadding", this.nodePadding);
			obj.putIfNotNull("width", this.width);
			obj.putIfNotNull("colors", new ArrayHelper().addAllStrings(this.colors));
			return obj.js();
		}

	}

}
