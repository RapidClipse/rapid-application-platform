
package com.rapidclipse.framework.server.ui.filter.helper;

import java.util.logging.Logger;

import com.rapidclipse.framework.server.resources.StringResourceUtils;
import com.rapidclipse.framework.server.ui.filter.FilterComponent;
import com.vaadin.flow.component.Key;
import com.vaadin.flow.component.button.ButtonVariant;
import com.vaadin.flow.component.icon.VaadinIcon;


/**
 * @author XDEV Software
 *
 */
public class AddButton extends Buttons
{

	@Override
	public void defineButton()
	{
		this.setClassName("addButton");
		this.setIcon(VaadinIcon.CHECK.create());
		this.addThemeVariants(ButtonVariant.LUMO_SMALL, ButtonVariant.LUMO_TERTIARY_INLINE);
		this.getElement().setProperty("title", StringResourceUtils.getResourceString("addHover", this));
		this.addClickShortcut(Key.ENTER);
	}
	
	/**
	 * This method is not coded!!!<br>
	 * 'Cause the Method is <b> yet</b> not needed, their is <b> NO CODE </b>!
	 */
	@Override
	public void setClickListener(final FilterComponent component, final ReplaceabelEditor editor)
	{
		final Logger logger = Logger.getLogger(Logger.GLOBAL_LOGGER_NAME);
		logger.warning("Method not coded!");
		
	}
	
}
