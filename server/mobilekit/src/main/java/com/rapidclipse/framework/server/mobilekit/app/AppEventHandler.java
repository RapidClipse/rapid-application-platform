
package com.rapidclipse.framework.server.mobilekit.app;

import java.util.EventListener;


/**
 * Handler for events fired by {@link AppService}.
 *
 * @author XDEV Software
 *
 */
@FunctionalInterface
public interface AppEventHandler extends EventListener
{
	/**
	 * Called when an app event happened.
	 * <p>
	 * Propagation to other listeners can be prevented by calling
	 * {@link AppEvent#consume()}.
	 *
	 * @param event
	 */
	public void handleAppEvent(AppEvent event);
}
