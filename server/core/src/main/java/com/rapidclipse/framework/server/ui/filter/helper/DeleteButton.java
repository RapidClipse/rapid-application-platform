
package com.rapidclipse.framework.server.ui.filter.helper;

import com.rapidclipse.framework.server.resources.StringResourceUtils;
import com.rapidclipse.framework.server.ui.filter.helper.interfaces.RemoveFilterEditor;
import com.rapidclipse.framework.server.ui.filter.helper.interfaces.Replaceabel;
import com.vaadin.flow.component.button.ButtonVariant;
import com.vaadin.flow.component.icon.VaadinIcon;


/**
 * @author XDEV Software
 *
 */
public class DeleteButton extends Buttons<RemoveFilterEditor>
{
	/**
	 * Defines the Button with Classname, etc.
	 * <br>
	 * Classname = deleteButton -> getting through
	 * {@link StringResourceUtils #getResourceString(String, java.util.Locale)}
	 */
	@Override
	public void defineButton()
	{
		this.setClassName(StringResourceUtils.getResourceString("deleteButton", this));
		this.setIcon(VaadinIcon.CLOSE.create());
		this.addThemeVariants(ButtonVariant.LUMO_SMALL, ButtonVariant.LUMO_TERTIARY_INLINE);
		this.getElement().setProperty("title", StringResourceUtils.getResourceString("deleteHover", this));
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void setClickListener(final RemoveFilterEditor component, final Replaceabel editor)
	{
		this.addClickListener(listener -> component.removeFilterEntryEditor(editor));
	}

}
