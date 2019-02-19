
package com.rapidclipse.framework.server.data.filter;

/**
 * @author XDEV Software
 *
 */
public interface Between extends Filter
{
	public Object identifier();
	
	public Comparable<?> start();
	
	public Comparable<?> end();
	
	public static class Implementation implements Between
	{
		private final Object        identifier;
		private final Comparable<?> start;
		private final Comparable<?> end;
		
		public Implementation(
			final Object identifier,
			final Comparable<?> start,
			final Comparable<?> end)
		{
			super();

			this.identifier = identifier;
			this.start      = start;
			this.end        = end;
		}
		
		@Override
		public Object identifier()
		{
			return this.identifier;
		}
		
		@Override
		public Comparable<?> start()
		{
			return this.start;
		}
		
		@Override
		public Comparable<?> end()
		{
			return this.end;
		}
	}
}
