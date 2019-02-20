
package com.rapidclipse.framework.server.ui.filter;

/**
 * @author XDEV Software
 *
 */
public interface FilterSubjectFactory
{
	public boolean supports(Object source);
	
	public FilterSubject createFilterSubject(Object source);
}
