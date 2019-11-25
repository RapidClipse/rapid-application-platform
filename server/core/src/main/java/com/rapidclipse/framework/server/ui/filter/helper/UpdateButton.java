
package com.rapidclipse.framework.server.ui.filter.helper;

import com.rapidclipse.framework.server.resources.StringResourceUtils;
import com.rapidclipse.framework.server.ui.filter.FilterComponent;
import com.vaadin.flow.component.Key;
import com.vaadin.flow.component.button.ButtonVariant;
import com.vaadin.flow.component.icon.VaadinIcon;


/**
 * @author XDEV Software
 *
 */
public class UpdateButton extends Buttons
{

	/**
	 * Defines the Button with Classname, etc.
	 *
	 * Classname = addButton -> getting through
	 * {@link StringResourceUtils #getResourceString(String, java.util.Locale)}
	 */
	@Override
	public void defineButton()
	{
		this.setIcon(VaadinIcon.CHECK.create());
		this.setClassName(StringResourceUtils.getResourceString("addButton", this));
		this.addThemeVariants(ButtonVariant.LUMO_SMALL, ButtonVariant.LUMO_TERTIARY_INLINE);
		this.getElement().setProperty("title", StringResourceUtils.getResourceString("updateHover", this));
		this.addClickShortcut(Key.ENTER);
	}
	
	/**
	 * {@inheritDoc}
	 */
	@Override
	public void setClickListener(final FilterComponent component, final ReplaceabelEditor editor)
	{
		this.addClickListener(listener -> {
			component.getLabelDiv().updateRow(component, editor, new LabelButtons(component));
			component.newFilterEntry();
			component.updateFilterData();
		});

	}
	
}
