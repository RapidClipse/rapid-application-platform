
package com.rapidclipse.framework.server.webapi.visibility;

/**
 * The visibility state of an application. If the user can see the application its visibility state is visible.
 * 
 * @author XDEV Software
 * @since 10.02.00
 */
public enum VisibilityState
{
	/**
	 * The user can see the application.
	 */
	visible,
	
	/**
	 * The user can not see the application.
	 */
	hidden,
	
	/**
	 * This is usually set when the application is first loaded. The state will never change to this value
	 * afterwards.
	 */
	prerender,
	
	/**
	 * This can be set when the application is being unloaded.
	 */
	unloaded
}
