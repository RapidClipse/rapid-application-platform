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
package com.rapidclipse.framework.server.charts.wordtree;

import java.io.Serializable;

import com.rapidclipse.framework.server.charts.JavaScriptable;


/**
 * @author XDEV Software
 * @since 10.02.00
 *
 */
public interface WordTree extends Serializable, JavaScriptable
{
	public Format format();

	public String sentenceSeparator();

	public Type type();

	public String word();

	public String wordSeparator();

	public static Builder Builder()
	{
		return new Builder.Default();
	}

	public static interface Builder
	{
		public Builder format(Format format);

		public Builder sentenceSeparator(String sentenceSeparator);

		public Builder type(Type type);

		public Builder word(String word);

		public Builder wordSeparator(String wordSeparator);

		public WordTree build();
		
		public static class Default implements Builder
		{
			private Format format;
			private String sentenceSeparator;
			private Type   type;
			private String word;
			private String wordSeparator;
			
			Default()
			{
				super();
			}
			
			@Override
			public Builder format(final Format format)
			{
				this.format = format;
				return this;
			}

			@Override
			public Builder sentenceSeparator(final String sentenceSeparator)
			{
				this.sentenceSeparator = sentenceSeparator;
				return this;
			}

			@Override
			public Builder type(final Type type)
			{
				this.type = type;
				return this;
			}

			@Override
			public Builder word(final String word)
			{
				this.word = word;
				return this;
			}

			@Override
			public Builder wordSeparator(final String wordSeparator)
			{
				this.wordSeparator = wordSeparator;
				return this;
			}
			
			@Override
			public WordTree build()
			{
				return new WordTree.Default(this.format, this.sentenceSeparator, this.type, this.word,
					this.wordSeparator);
			}
			
		}
		
	}
	
	public static class Default implements WordTree
	{
		private final Format format;
		private final String sentenceSeparator;
		private final Type   type;
		private final String word;
		private final String wordSeparator;

		Default(
			final Format format,
			final String sentenceSeparator,
			final Type type,
			final String word,
			final String wordSeparator)
		{
			super();
			
			this.format            = format;
			this.sentenceSeparator = sentenceSeparator;
			this.type              = type;
			this.word              = word;
			this.wordSeparator     = wordSeparator;
		}
		
		@Override
		public Format format()
		{
			return this.format;
		}

		@Override
		public String sentenceSeparator()
		{
			return this.sentenceSeparator;
		}

		@Override
		public Type type()
		{
			return this.type;
		}

		@Override
		public String word()
		{
			return this.word;
		}

		@Override
		public String wordSeparator()
		{
			return this.wordSeparator;
		}
		
		@Override
		public String js()
		{
			final ObjectHelper obj = new ObjectHelper();
			obj.putIfNotNull("format", this.format);
			obj.putIfNotNull("sentenceSeparator", this.sentenceSeparator);
			obj.putIfNotNull("type", this.type);
			obj.putIfNotNull("word", this.word);
			obj.putIfNotNull("wordSeparator", this.wordSeparator);
			return obj.js();
		}
		
	}
	
}
