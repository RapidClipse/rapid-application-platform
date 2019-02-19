
package com.rapidclipse.framework.server.mobilekit;

import com.vaadin.flow.component.UI;


/**
 * @author XDEV Software
 *
 */
public interface MobileService
{
	public static <M extends MobileService> M getCurrent(final Class<M> type)
	{
		return getCurrent(type, UI.getCurrent());
	}
	
	public static <M extends MobileService> M getCurrent(final Class<M> type, final UI ui)
	{
		return ui.getChildren().filter(type::isInstance).map(type::cast).findFirst().orElse(null);
	}
}
