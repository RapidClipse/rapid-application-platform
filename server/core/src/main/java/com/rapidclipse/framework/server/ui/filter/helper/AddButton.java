
package com.rapidclipse.framework.server.ui.filter.helper;

import com.rapidclipse.framework.server.resources.StringResourceUtils;
import com.rapidclipse.framework.server.ui.filter.FilterComponent;
import com.rapidclipse.framework.server.ui.filter.helper.interfaces.FilterComponentInterface;
import com.rapidclipse.framework.server.ui.filter.helper.interfaces.Replaceabel;
import com.vaadin.flow.component.Key;
import com.vaadin.flow.component.button.ButtonVariant;
import com.vaadin.flow.component.icon.VaadinIcon;
import com.vaadin.flow.shared.Registration;


/**
 * @author XDEV Software
 *
 */
public class AddButton extends Buttons<FilterComponentInterface>
{
	private Registration addButtonClick;

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
	 * Add a clickListener to this Button. This will add a new {@link Label} to the <b>labelDiv</b> with the
	 * selected data.
	 *
	 * @param component
	 *            -> {@link FilterComponent}
	 * @param editor
	 *            -> {@link ReplaceabelEditor}
	 */
	@Override
	public void setClickListener(final FilterComponentInterface component, final Replaceabel editor)
	{
		removeListener();
		this.addButtonClick =
			this.addClickListener(
				listener -> component.getLabelDiv().addingNewRow(component, editor, new LabelButtons(component)));
	}
	
	private void removeListener()
	{
		if(this.addButtonClick != null)
		{
			this.addButtonClick.remove();
		}

	}
}
