
package com.rapidclipse.framework.server.webapi.screen;

import java.io.Serializable;


/**
 *
 * @author XDEV Software
 * @since 10.02.00
 */
public class FullscreenOptions implements Serializable
{
	private NavigationUI navigationUI;
	
	public static final FullscreenOptions DEFAULT = new FullscreenOptions(NavigationUI.auto);
	
	public FullscreenOptions()
	{
		super();
	}
	
	public FullscreenOptions(final NavigationUI navigationUI)
	{
		this.navigationUI = navigationUI;
	}
	
	public NavigationUI getNavigationUI()
	{
		return this.navigationUI;
	}
	
	public FullscreenOptions setNavigationUI(final NavigationUI navigationUI)
	{
		this.navigationUI = navigationUI;
		return this;
	}
	
	public enum NavigationUI
	{
		hide,
		show,
		auto
	}
}
