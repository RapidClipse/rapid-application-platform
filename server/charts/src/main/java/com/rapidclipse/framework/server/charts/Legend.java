
package com.rapidclipse.framework.server.charts;

import java.io.Serializable;

import com.rapidclipse.framework.server.util.JavaScriptable;

import elemental.json.Json;


/**
 * @author XDEV Software
 *
 */
public interface Legend extends Serializable, JavaScriptable
{
	public static enum Alignment implements JavaScriptable
	{
		START("start"),
		CENTER("center"),
		END("end");

		private final String js;

		private Alignment(final String js)
		{
			this.js = Json.create(js).toJson();
		}

		@Override
		public String js()
		{
			return this.js;
		}
	}

	public Alignment alignment();

	public Integer maxLines();

	public String position();

	public TextStyle textStyle();

	public static Builder Builder()
	{
		return new Builder.Default();
	}

	public static interface Builder
	{
		public Builder alignment(Alignment alignment);

		public Builder maxLines(Integer maxLines);

		public Builder position(String position);

		public Builder textStyle(TextStyle textStyle);

		public Legend build();

		public static class Default implements Builder
		{
			private Alignment alignment;
			private Integer   maxLines;
			private String    position;
			private TextStyle textStyle;

			Default()
			{
				super();
			}

			@Override
			public Builder alignment(final Alignment alignment)
			{
				this.alignment = alignment;
				return this;
			}

			@Override
			public Builder maxLines(final Integer maxLines)
			{
				this.maxLines = maxLines;
				return this;
			}

			@Override
			public Builder position(final String position)
			{
				this.position = position;
				return this;
			}

			@Override
			public Builder textStyle(final TextStyle textStyle)
			{
				this.textStyle = textStyle;
				return this;
			}

			@Override
			public Legend build()
			{
				return new Legend.Default(this.alignment, this.maxLines, this.position, this.textStyle);
			}
		}
	}

	public static class Default implements Legend
	{
		private final Alignment alignment;
		private final Integer   maxLines;
		private final String    position;
		private final TextStyle textStyle;

		Default(final Alignment alignment, final Integer maxLines, final String position, final TextStyle textStyle)
		{
			super();

			this.alignment = alignment;
			this.maxLines  = maxLines;
			this.position  = position;
			this.textStyle = textStyle;
		}

		@Override
		public Alignment alignment()
		{
			return this.alignment;
		}

		@Override
		public Integer maxLines()
		{
			return this.maxLines;
		}

		@Override
		public String position()
		{
			return this.position;
		}

		@Override
		public TextStyle textStyle()
		{
			return this.textStyle;
		}

		@Override
		public String js()
		{
			final ObjectHelper obj = new ObjectHelper();
			obj.putIfNotNull("alignment", this.alignment);
			obj.putIfNotNull("maxLines", this.maxLines);
			obj.putIfNotNull("position", this.position);
			obj.putIfNotNull("textStyle", this.textStyle);
			return obj.js();
		}

	}

}