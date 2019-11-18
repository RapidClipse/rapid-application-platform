
package com.rapidclipse.framework.server.ui.filter.readabelHelper;

import com.rapidclipse.framework.server.resources.StringResourceUtils;
import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.button.ButtonVariant;
import com.vaadin.flow.component.icon.VaadinIcon;


/**
 * @author XDEV Software
 *
 */
public class UpdateButton extends Button
{
	
	/**
	 * Defines the Button with Classname, etc.
	 *
	 * Classname = addButton -> getting through
	 * {@link StringResourceUtils #getResourceString(String, java.util.Locale)}
	 */
	public void defineButton()
	{
		this.setIcon(VaadinIcon.CHECK.create());
		this.setClassName(StringResourceUtils.getResourceString("addButton", this));
		this.addThemeVariants(ButtonVariant.LUMO_SMALL, ButtonVariant.LUMO_TERTIARY_INLINE);
	}
}
