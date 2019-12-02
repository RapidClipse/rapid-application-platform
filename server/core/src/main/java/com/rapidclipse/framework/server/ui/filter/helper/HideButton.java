
package com.rapidclipse.framework.server.ui.filter.helper;

import com.rapidclipse.framework.server.resources.StringResourceUtils;
import com.rapidclipse.framework.server.ui.filter.helper.interfaces.Replaceabel;
import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.button.ButtonVariant;
import com.vaadin.flow.component.icon.Icon;
import com.vaadin.flow.component.icon.VaadinIcon;


/**
 * @author XDEV Software
 *
 */
public class HideButton extends Buttons<Component>
{
	boolean                     open;
	Icon                        icon;
	private static final String TITLE = "title";
	
	/**
	 * Set true or false. Either if something is open or not
	 *
	 * @param b
	 */
	public void setOpen(final boolean b)
	{
		this.open = b;
		
	}
	
	/**
	 * return the boolean <b> open</b> to check if something is open or not
	 *
	 * @return
	 */
	public boolean isOpen()
	{
		return this.open;
	}

	@Override
	public void defineButton()
	{
		this.setIcon(VaadinIcon.ANGLE_DOUBLE_LEFT.create());

		setOpen(false);
		this.getElement().setProperty(TITLE,
			StringResourceUtils.getResourceString("hideHover", this));
		this.addClassName(StringResourceUtils.getResourceString("hideFilterButton", this));
		this.addThemeVariants(ButtonVariant.LUMO_SMALL, ButtonVariant.LUMO_TERTIARY_INLINE);

	}
	
	public void open()
	{
		this.setIcon(VaadinIcon.ANGLE_DOUBLE_DOWN.create());
		this.setOpen(true);
		this.getElement().setProperty(TITLE,
			StringResourceUtils.getResourceString("showHover", this));
	}
	
	public void close()
	{
		this.setIcon(VaadinIcon.ANGLE_DOUBLE_LEFT.create());
		this.setOpen(false);
		this.getElement().setProperty(TITLE,
			StringResourceUtils.getResourceString("hideHover", this));
	}
	
	/**
	 * Defines the ClickEvent which is activated by clicking on the 'Hide-Button'.
	 * This will hide or show the FilterDiv and restyle the Button with:<br>
	 * {@link #open} <br>
	 * and <br>
	 * {@link #close}
	 */
	public void setClickListener(final Component component)
	{
		this.addClickListener(listener -> {
			if(!this.isOpen())
			{
				open();
				component.setVisible(true);
			}
			else if(this.isOpen())
			{
				close();
				component.setVisible(false);
			}
		});

	}

	/*
	 * (non-Javadoc)
	 *
	 * @see
	 * com.rapidclipse.framework.server.ui.filter.helper.Buttons#setClickListener(com.rapidclipse.framework.server.ui.
	 * filter.helper.interfaces.FilterComponentInterface,
	 * com.rapidclipse.framework.server.ui.filter.helper.interfaces.Replaceabel)
	 */
	@Override
	public void setClickListener(final Component first, final Replaceabel second)
	{
		// TODO Auto-generated method stub
		
	}

}
