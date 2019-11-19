
package com.rapidclipse.framework.server.ui.filter.readabelHelper;

import com.rapidclipse.framework.server.resources.StringResourceUtils;
import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.button.ButtonVariant;
import com.vaadin.flow.component.icon.VaadinIcon;


/**
 * @author XDEV Software
 *
 */
public class DeleteButton extends Button
{
	/**
	 * Defines the Button with Classname, etc.
	 * <br>
	 * Classname = deleteButton -> getting through
	 * {@link StringResourceUtils #getResourceString(String, java.util.Locale)}
	 */
	public void defineButton()
	{
		this.setClassName(StringResourceUtils.getResourceString("deleteButton", this));
		this.setIcon(VaadinIcon.MINUS.create());
		this.addThemeVariants(ButtonVariant.LUMO_SMALL, ButtonVariant.LUMO_TERTIARY_INLINE);
		this.getElement().setProperty("title", StringResourceUtils.getResourceString("deleteHover", this));
	}
	
}
