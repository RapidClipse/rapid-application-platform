
package com.rapidclipse.framework.server.charts;

/**
 * @author XDEV Software
 * @since 10.02.00
 *
 */
public interface HasAnimation extends Chart
{
	public default Animation getAnimation()
	{
		return properties().get("animation");
	}
	
	public default void setAnimation(final Animation animation)
	{
		properties().put("animation", animation);
	}
}
