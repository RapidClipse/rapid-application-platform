
package com.rapidclipse.framework.server.ui.filter.helper;

import com.rapidclipse.framework.server.resources.StringResourceUtils;
import com.rapidclipse.framework.server.ui.filter.helper.interfaces.FilterComponentInterface;
import com.rapidclipse.framework.server.ui.filter.helper.interfaces.Replaceabel;
import com.vaadin.flow.component.button.ButtonVariant;
import com.vaadin.flow.component.icon.VaadinIcon;


/**
 * @author XDEV Software
 *
 */
public class EditButton extends Buttons<FilterComponentInterface>
{
	/**
	 * Defines the Button with Classname, etc.
	 *
	 * Classname= editButton -> getting through
	 * {@link StringResourceUtils #getResourceString(String, java.util.Locale)}
	 */
	@Override
	public void defineButton()
	{
		this.setClassName(StringResourceUtils.getResourceString("editButton", this));
		this.setIcon(VaadinIcon.EDIT.create());
		this.addThemeVariants(ButtonVariant.LUMO_SMALL, ButtonVariant.LUMO_TERTIARY_INLINE);
		this.getElement().setProperty("title", StringResourceUtils.getResourceString("editHover", this));
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void setClickListener(final FilterComponentInterface component, final Replaceabel editor)
	{
		this.addClickListener(listener -> {
			component.getComboDiv().removeAll();
			component.getComboDiv().updateComboBox(editor, new ComboBoxButtons(component));
		});
		
	}

}
