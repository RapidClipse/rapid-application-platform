
package com.rapidclipse.framework.server.ui.persistence;

/**
 * @author XDEV Software
 *
 */
@FunctionalInterface
public interface GuiPersistenceHandlerProvider
{
	public void registerHandlers(GuiPersistenceHandlerRegistry registry);
}
