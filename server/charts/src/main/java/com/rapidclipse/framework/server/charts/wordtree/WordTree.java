
package com.rapidclipse.framework.server.charts.wordtree;

import java.io.Serializable;

import com.rapidclipse.framework.server.util.JavaScriptable;


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
