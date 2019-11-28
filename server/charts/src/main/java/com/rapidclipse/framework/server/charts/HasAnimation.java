
package com.rapidclipse.framework.server.charts;

/**
 * @author XDEV Software
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
