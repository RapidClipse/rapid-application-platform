
package com.rapidclipse.framework.server.ui.persistence;

import com.vaadin.flow.component.Component;


public interface GuiPersistenceHandler<C extends Component>
{
	public Class<C> handledType();
	
	public GuiPersistenceEntry persist(C component);
	
	public void restore(C component, GuiPersistenceEntry entry);
}
