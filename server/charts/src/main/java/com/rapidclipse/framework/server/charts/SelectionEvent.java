
package com.rapidclipse.framework.server.charts;

import com.vaadin.flow.component.ComponentEvent;


/**
 * @author XDEV Software
 *
 */
public class SelectionEvent<T extends Chart> extends ComponentEvent<T>
{
	private final Selection selection;
	
	SelectionEvent(final T source, final boolean fromClient, final Selection selection)
	{
		super(source, fromClient);

		this.selection = selection;
	}
	
	public Selection getSelection()
	{
		return this.selection;
	}
}