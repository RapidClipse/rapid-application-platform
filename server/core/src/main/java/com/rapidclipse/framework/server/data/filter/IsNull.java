
package com.rapidclipse.framework.server.data.filter;

/**
 * @author XDEV Software
 *
 */
public interface IsNull extends Filter
{
	public Object identifier();
	
	public static class Implementation implements IsNull
	{
		private final Object identifier;
		
		public Implementation(final Object identifier)
		{
			super();
			this.identifier = identifier;
		}
		
		@Override
		public Object identifier()
		{
			return this.identifier;
		}
	}
}
