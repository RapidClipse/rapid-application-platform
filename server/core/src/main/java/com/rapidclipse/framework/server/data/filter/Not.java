
package com.rapidclipse.framework.server.data.filter;

/**
 * @author XDEV Software
 *
 */
public interface Not extends Filter
{
	public Filter filter();
	
	public static class Implementation implements Not
	{
		private final Filter filter;
		
		public Implementation(final Filter filter)
		{
			super();
			this.filter = filter;
		}
		
		@Override
		public Filter filter()
		{
			return this.filter;
		}
	}
}
