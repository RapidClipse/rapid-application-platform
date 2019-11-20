
package com.rapidclipse.framework.server.ui.filter.readabelHelper;

import com.rapidclipse.framework.server.resources.StringResourceUtils;
import com.vaadin.flow.component.Key;
import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.button.ButtonVariant;
import com.vaadin.flow.component.icon.VaadinIcon;


/**
 * @author XDEV Software
 *
 */
public class AddButton extends Button
{

	public void defineButton()
	{
		this.setClassName("addButton");
		this.setIcon(VaadinIcon.PLUS.create());
		this.addThemeVariants(ButtonVariant.LUMO_SMALL, ButtonVariant.LUMO_TERTIARY_INLINE);
		this.getElement().setProperty("title", StringResourceUtils.getResourceString("addHover", this));
		this.addClickShortcut(Key.ENTER);
	}
	
}
