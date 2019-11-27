
package com.rapidclipse.framework.server.ui.filter.helper;

import com.rapidclipse.framework.server.resources.StringResourceUtils;
import com.rapidclipse.framework.server.ui.filter.FilterComponent;
import com.rapidclipse.framework.server.ui.filter.FilterEntryEditor;
import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.HasComponents;
import com.vaadin.flow.component.HasOrderedComponents;
import com.vaadin.flow.component.html.Div;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;
import com.vaadin.flow.dom.Element;


/**
 * @author XDEV Software
 *
 */
public class LabelDiv extends Div
{
	/**
	 * Defining the Div which is used to hold the labels beyond the Comboboxes
	 *
	 * Classname = labelDiv -> getting through
	 * {@link StringResourceUtils #getResourceString(String, java.util.Locale)}
	 */
	public void defineDiv()
	{
		this.setVisible(false);
		this.setWidthFull();
		this.addClassName(StringResourceUtils.getResourceString("labelDiv", this));
	}
	
	public void updateRow(final FilterComponent component, final ReplaceabelEditor editor, final LabelButtons buttons)
	{
		buttons.definingButtons(editor);
		
		final HorizontalLayout entryRow     = createEntryRow(editor);
		final HorizontalLayout buttonLayout =
			component.createButtonLayout(buttons.getCheckbox(), buttons.getEditButton(),
				buttons.getDeleteButton());
		
		final HorizontalLayout finalLayout = component.createFinalLayout(entryRow, buttonLayout);
		
		replaceLabelRow(editor.getLabelLayout(), finalLayout);
		
		editor.setLabelLayout(finalLayout);
		
		component.updateEverything(editor);
	}
	
	/**
	 * Copied from {@link HasOrderedComponents #replace(Component, Component)}
	 *
	 * @param oldComponent
	 *            -> {@link Component}
	 * @param newComponent
	 *            -> {@link Component}
	 * @param wrapper
	 *            -> {@link HasComponents}
	 */
	private void
		replaceLabelRow(final Component oldComponent, final Component newComponent)
	{
		if(oldComponent == null && newComponent == null)
		{
			// NO-OP
			return;
		}
		if(oldComponent == null)
		{
			this.add(newComponent);
		}
		else if(newComponent == null)
		{
			this.remove(oldComponent);
		}
		else
		{
			final Element element  = this.getElement();
			final int     oldIndex = element.indexOfChild(oldComponent.getElement());
			final int     newIndex = element.indexOfChild(newComponent.getElement());
			if(oldIndex >= 0 && newIndex >= 0)
			{
				element.insertChild(oldIndex, newComponent.getElement());
				element.insertChild(newIndex, oldComponent.getElement());
			}
			else if(oldIndex >= 0)
			{
				element.setChild(oldIndex, newComponent.getElement());
			}
			else
			{
				this.add(newComponent);
			}
		}
	}
	
	public void removeData(final ReplaceabelEditor editor)
	{
		this.remove(editor.getLabelLayout());
	}
	
	/**
	 * Creates the Filter Entry Row for the <b> Label Div </b>
	 * <br>
	 * The EntryRow is specified as {@link HorizontalLayout} and holds different {@link Label} foreach:
	 * <br>
	 * {@link FilterEntryEditor#getSelectedProperty()},
	 * <br>
	 * {@link FilterEntryEditor #getSelectedOperator()},
	 * <br>
	 * {@link FilterEntryEditor#getValueEditors()}
	 *
	 * @param editor
	 *            -> {@link FilterEntryEditor}
	 * @return The EntryRow as {@link HorizontalLayout}
	 *         <br>
	 *         Classname = entryRowLabel -> getting through
	 *         {@link StringResourceUtils #getResourceString(String, java.util.Locale)}
	 */
	public HorizontalLayout createEntryRow(final ReplaceabelEditor replace)
	{
		final FilterEntryEditor editor = replace.getOriginal();
		final EntryRowLabel     entry  = new EntryRowLabel(editor);
		replace.setEntryRow(entry);
		
		return entry.getLayout();
	}
	
	/**
	 * Add a new Row to this Div.
	 * Defines the given Components and create the the row with {@link #createFinalLayout(Component, Component)}.
	 *
	 * @param component
	 *            -> {@link FilterComponent}
	 * @param editor
	 *            -> {@link ReplaceabelEditor}
	 * @param buttons
	 *            -> {@link LabelButtons}
	 */
	public void
		addingNewRow(final FilterComponent component, final ReplaceabelEditor editor, final LabelButtons buttons)
	{
		buttons.definingButtons(editor);
		
		component.getFilterEntryEditors().add(editor);
		
		final HorizontalLayout finalLayout = component.createFinalLayout(this.createEntryRow(editor),
			component.createButtonLayout(buttons.getCheckbox(), buttons.getEditButton(), buttons.getDeleteButton()));
		
		editor.setLabelLayout(finalLayout);
		
		this.add(finalLayout);
		this.setVisible(true);
		
		component.updateEverything(editor);
	}
}
