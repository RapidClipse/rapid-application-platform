/*
 * Copyright (C) 2013-2019 by XDEV Software, All Rights Reserved.
 *
 * This file is part of the RapidClipse Application Platform (RAP).
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *
 * Contributors:
 *     XDEV Software Corp. - initial API and implementation
 */

package com.rapidclipse.framework.server.charts;

import java.io.Serializable;

import elemental.json.Json;


/**
 * @author XDEV Software
 * @since 10.02.00
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

	public static enum Position implements JavaScriptable
	{
		BOTTOM("bottom"),
		LEFT("left"),
		IN("in"),
		NONE("none"),
		RIGHT("right"),
		TOP("top");

		private final String js;

		private Position(final String js)
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

	public Position position();

	public TextStyle textStyle();

	public static Legend None()
	{
		return New(Position.NONE);
	}

	public static Legend New(final Position position)
	{
		return Builder().position(position).build();
	}

	public static Builder Builder()
	{
		return new Builder.Default();
	}

	public static interface Builder
	{
		public Builder alignment(Alignment alignment);

		public Builder maxLines(Integer maxLines);

		public Builder position(Position position);

		public Builder textStyle(TextStyle textStyle);

		public Legend build();

		public static class Default implements Builder
		{
			private Alignment alignment;
			private Integer   maxLines;
			private Position  position;
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
			public Builder position(final Position position)
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
		private final Position  position;
		private final TextStyle textStyle;

		Default(final Alignment alignment, final Integer maxLines, final Position position, final TextStyle textStyle)
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
		public Position position()
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
