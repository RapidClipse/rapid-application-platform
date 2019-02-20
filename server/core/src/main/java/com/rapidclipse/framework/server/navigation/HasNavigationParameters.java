
package com.rapidclipse.framework.server.navigation;

import com.vaadin.flow.router.BeforeEvent;
import com.vaadin.flow.router.HasUrlParameter;


/**
 * @author XDEV Software
 *
 */
public interface HasNavigationParameters extends HasUrlParameter<String>
{
	@Override
	default void setParameter(final BeforeEvent event, final String parameter)
	{
		NavigationUtils.injectParameters(this, event, parameter);
		navigationParametersUpdated();
	}
	
	default void navigationParametersUpdated()
	{
	}
}
