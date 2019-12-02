
package com.rapidclipse.framework.server.ui.filter.helper;

import com.rapidclipse.framework.server.resources.StringResourceUtils;
import com.rapidclipse.framework.server.ui.filter.FilterEntryEditor;
import com.rapidclipse.framework.server.ui.filter.helper.interfaces.Replaceabel;
import com.vaadin.flow.component.formlayout.FormLayout;
import com.vaadin.flow.component.html.Div;
import com.vaadin.flow.component.html.Label;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;


/**
 * @author XDEV Software
 *
 */
public class ComboDiv extends DefinedDiv
{

	/**
	 * Defines the ComboDiv with ClassName, etc.
	 * <br>
	 * Classname = comboBoxDiv -> getting through
	 * {@link StringResourceUtils #getResourceString(String, java.util.Locale)}
	 *
	 */
	public void defineDiv()
	{
		this.setVisible(true);
		this.setWidthFull();
		this.addClassName(StringResourceUtils.getResourceString("comboBoxDiv", this));
	}
	
	/**
	 * Updates the {@link FilterEntryEditor} {@link Div} with the given Data of the Label.
	 * This method is needed to edit an excisting {@link Label}.
	 *
	 * @param editor
	 *            -> {@link ReplaceabelEditor}
	 * @param buttons
	 *            -> {@link ComboBoxButtons}
	 */
	public void
		updateComboBox(final Replaceabel editor, final ComboBoxButtons buttons)
	{
		buttons.definingButtons(editor);

		editor.getOriginal().setVisible(true); // Replaceabel

		final HorizontalLayout entryRow = createEntryRow(editor.getOriginal());// Replaceabel
		
		final HorizontalLayout buttonLayout = createButtonLayout(buttons.getUpdateButton(), buttons.getCancelButton());
		buttonLayout.setClassName("buttonLayoutCombo");
		
		this.add(createFinalLayout(entryRow, buttonLayout));
	}

	/**
	 * Creates the Filter Entry Row for the <b> Combo Div </b>
	 * <br>
	 * The EntryRow holds a single instance of a {@link FilterEntryEditor}. Because of this also extended from a
	 * {@link FormLayout},
	 * there is no more need to specify that object.
	 *
	 * @param editor
	 *            -> {@link FilterEntryEditor}
	 * @return The Entry Row as {@link HorizontalLayout}
	 *         <br>
	 *         Classname = entryRowComboBox -> getting through
	 *         {@link StringResourceUtils #getResourceString(String, java.util.Locale)}
	 */
	public HorizontalLayout createEntryRow(final FilterEntryEditor editor)
	{
		final HorizontalLayout layout = new HorizontalLayout(editor);
		layout.setEnabled(true);
		layout.addClassName(StringResourceUtils.getResourceString("entryRowComboBox", this));
		return layout;
	}
	
}
