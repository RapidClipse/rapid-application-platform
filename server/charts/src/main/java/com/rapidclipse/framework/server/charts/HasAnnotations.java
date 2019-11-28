
package com.rapidclipse.framework.server.charts;

/**
 * @author XDEV Software
 *
 */
public interface HasAnnotations extends Chart
{
	public default Annotations getAnnotations()
	{
		return properties().get("annotations");
	}
	
	public default void setAnnotations(final Annotations annotations)
	{
		properties().put("annotations", annotations);
	}
}
