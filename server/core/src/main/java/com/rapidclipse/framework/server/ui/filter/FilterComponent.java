/*
 * Copyright (C) 2013-2019 by XDEV Software, All Rights Reserved.
 *
 * This file is part of the RapidClipse Application Platform (RAP).
 *
 * RAP is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * RAP is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with RAP. If not, see <http://www.gnu.org/licenses/>.
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *
 * Contributors:
 *     XDEV Software Corp. - initial API and implementation
 */

package com.rapidclipse.framework.server.ui.filter;

import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.function.BiPredicate;
import java.util.function.Predicate;
import java.util.stream.Collectors;

import com.rapidclipse.framework.server.data.filter.Composite;
import com.rapidclipse.framework.server.data.filter.Composite.Connector;
import com.rapidclipse.framework.server.data.filter.Filter;
import com.rapidclipse.framework.server.data.provider.DataProviderFilterAdapter;
import com.rapidclipse.framework.server.resources.StringResourceUtils;
import com.rapidclipse.framework.server.util.ServiceLoader;
import com.vaadin.flow.component.AbstractCompositeField;
import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.HasSize;
import com.vaadin.flow.component.UI;
import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.checkbox.Checkbox;
import com.vaadin.flow.component.combobox.ComboBox;
import com.vaadin.flow.component.dependency.JavaScript;
import com.vaadin.flow.component.dependency.StyleSheet;
import com.vaadin.flow.component.dialog.Dialog;
import com.vaadin.flow.component.formlayout.FormLayout;
import com.vaadin.flow.component.grid.Grid;
import com.vaadin.flow.component.html.Div;
import com.vaadin.flow.component.html.Label;
import com.vaadin.flow.component.icon.VaadinIcon;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import com.vaadin.flow.component.textfield.TextField;
import com.vaadin.flow.data.provider.ConfigurableFilterDataProvider;
import com.vaadin.flow.data.provider.DataProvider;
import com.vaadin.flow.data.value.ValueChangeMode;
import com.vaadin.flow.shared.Registration;


/**
 * @author XDEV Software
 *
 */
@StyleSheet("FilterComponent.css")
@JavaScript("FilterComponent.js")
public class FilterComponent
	extends AbstractCompositeField<VerticalLayout, FilterComponent, FilterData>
	implements FilterContext, HasSize
{
	private boolean caseSensitive = false;
	private char    wildcard      = '*';

	private Connector searchPropertiesConnector = Connector.OR;
	private Connector searchMultiWordConnector  = Connector.OR;
	private Connector filterPropertiesConnector = Connector.AND;
	private Connector searchAndFilterConnector  = Connector.AND;

	private SearchFilterGenerator searchFilterGenerator = SearchFilterGenerator
		.New();

	private FilterOperatorRegistry            filterOperatorRegistry            = FilterOperatorRegistry
		.Default();
	private SubsetDataProviderFactoryRegistry subsetDataProviderFactoryRegistry = SubsetDataProviderFactoryRegistry
		.Default();

	private FilterSubject filterSubject;

	private TextField                     searchTextField;
	private Button                        addFilterButton;
	private HideButton                    hideFilterButton;
	private Div                           filterDiv;
	private Div                           comboDiv;
	private Div                           labelDiv           = new Div();
	private final List<FilterEntryEditor> filterEntryEditors = new ArrayList<>();
	private int                           rowIndex           = 0;
	private int                           windowWidth;
	private static final int              BREAKPOINT         = 727;
	protected List<String>                filterProperty     = new ArrayList<>();
	protected List<String>                filterOperator     = new ArrayList<>();

	public FilterComponent()
	{
		super(new FilterData("", null));

		/*
		 * Init UI
		 */
		getContent();
	}

	@Override
	protected VerticalLayout initContent()
	{

		UI.getCurrent().getPage()
			.retrieveExtendedClientDetails(detail -> {
				this.windowWidth = detail.getWindowInnerWidth();
				swapDivs();
			});
		UI.getCurrent().getPage().addBrowserWindowResizeListener(listener -> {
			this.windowWidth = listener.getWidth();
			swapDivs();
		});
		this.searchTextField = createSearchTextField();
		this.searchTextField.addValueChangeListener(event -> updateFilterData());
		this.searchTextField.setEnabled(false);

		this.hideFilterButton = createHideButton();
		this.hideFilterButton.addClickListener(listener -> hideButtonClickListener());

		this.comboDiv = createFilterDivComboBox();
		this.labelDiv = createFilterDivLabel();

		this.addFilterButton = createAddButton();
		this.addFilterButton.addClickListener(event -> addFilterEntryEditor(this.rowIndex));
		this.addFilterButton.setEnabled(false);

		final HorizontalLayout searchBar = createSearchBar();

		return createContent(searchBar);
	}

	public Registration connectWith(final Grid<?> grid)
	{
		final Registration registration = connectWith(grid.getDataProvider());

		// set subject after successful registration
		setFilterSubject(GridFilterSubjectFactory.CreateFilterSubject(grid));

		return registration;
	}

	@SuppressWarnings({"unchecked"})
	public Registration connectWith(final DataProvider<?, ?> dataProvider)
	{
		if(dataProvider instanceof ConfigurableFilterDataProvider<?, ?, ?>)
		{
			final ConfigurableFilterDataProvider<?, ?, ?>                            configurableFilterDataProvider =
				(ConfigurableFilterDataProvider<?, ?, ?>)dataProvider;
			final DataProviderFilterAdapter<ConfigurableFilterDataProvider<?, ?, ?>> filterAdapter                  =
				ServiceLoader.forType(DataProviderFilterAdapter.class).servicesStream()
					.filter(adapter -> adapter.supports(configurableFilterDataProvider))
					.findFirst().orElse(null);
			if(filterAdapter != null)
			{
				return addValueChangeListener(
					event -> filterAdapter.updateFilter(configurableFilterDataProvider, getFilter()));
			}
		}

		throw new IllegalArgumentException("Unsupported data provider: " + dataProvider.getClass().getName());
	}

	/*****************************************************************************************************************************************
	 */
	/**************************************
	 ************ Creating Stuff***********
	 **************************************/

	/**
	 * Create the TopContent with the searchbar and the filterDiv
	 *
	 * @param searchBar
	 * @return
	 */
	private VerticalLayout createContent(final HorizontalLayout searchBar)
	{
		final VerticalLayout content = new VerticalLayout(searchBar, this.comboDiv);
		content.setMargin(false);
		content.setPadding(false);
		content.setSpacing(true);
		return content;
	}

	/**
	 * Creating the Searchbar
	 *
	 * @return
	 */
	private HorizontalLayout createSearchBar()
	{
		final HorizontalLayout searchBar = new HorizontalLayout(this.searchTextField, this.hideFilterButton,
			this.addFilterButton);
		searchBar.setMargin(false);
		searchBar.setPadding(false);
		searchBar.expand(this.searchTextField);
		searchBar.setWidth("100%");
		return searchBar;
	}

	/**
	 * Creating the Search Text Field
	 *
	 * @return
	 */
	protected TextField createSearchTextField()
	{
		final TextField textField = new TextField();
		textField.setValueChangeMode(ValueChangeMode.EAGER);
		return textField;
	}

	/**
	 * Creates the Layout which is then seen from the User inside the Div
	 *
	 * @param filterEntryRow
	 *            -> The left part of the Layout with the needed Editor
	 * @param finalButtonLayout
	 *            -> The right part of the Layout with the needed Buttons
	 * @return
	 */
	private HorizontalLayout createFinalLayout(final Component filterEntryRow, final Component finalButtonLayout)
	{
		final HorizontalLayout layout = new HorizontalLayout();
		layout.add(filterEntryRow, finalButtonLayout);
		layout.addClassName("finalLayout");
		return layout;
	}

	private Dialog createPopUpDialog()
	{

		final Dialog dialog = new Dialog(new Label(StringResourceUtils.getResourceString("filterLabel", this)));
		dialog.setCloseOnEsc(false);
		dialog.setCloseOnOutsideClick(false);
		return dialog;
	}

	protected Filter createValueFilter()
	{
		if(this.filterEntryEditors == null || this.filterEntryEditors.isEmpty())
		{
			return null;
		}

		final List<Filter> valueFilters = this.filterEntryEditors.stream()
			.map(editor -> editor.getFilter()).filter(Objects::nonNull)
			.collect(Collectors.toList());
		if(valueFilters.isEmpty())
		{
			return null;
		}

		final int count = valueFilters.size();
		if(count == 1)
		{
			return valueFilters.get(0);
		}

		return Composite.New(getFilterPropertiesConnector(), valueFilters);
	}

	protected Filter createSearchFilter()
	{
		if(this.searchFilterGenerator != null)
		{
			return this.searchFilterGenerator.createSearchFilter(getSearchText(), this);
		}

		return null;
	}

	/**
	 * Creating the Div which is used to hold the labels within the small view
	 *
	 * @return
	 */
	private Div createFilterDivLabel()
	{
		final Div filter = new Div();
		filter.setVisible(false);
		filter.setWidthFull();
		filter.addClassName("filterDivLabel");
		return filter;
	}

	/**
	 * Creating the Div which is used to hold the ComboBoxes within the normal view
	 *
	 * @return
	 */
	private Div createFilterDivComboBox()
	{
		final Div filter = new Div();
		filter.setVisible(true);
		filter.setWidthFull();
		filter.addClassName("filterDivComboBox");
		return filter;
	}

	/**
	 * Creates the Filter Entry Row for the <b> Label Div</b>
	 *
	 * @return
	 */
	@SuppressWarnings("rawtypes")
	private HorizontalLayout createEntryRowLabel(final int index, final FilterEntryEditor editor)
	{
		final HorizontalLayout row = new HorizontalLayout();
		row.addClassName("filterEntryRow");
		row.setEnabled(true);

		if(editor.getSelectedProperty() != null)
		{
			final List<FilterValueEditorComposite> values = editor.getValueEditors();

			row.add(new Label("" + editor.getSelectedProperty().caption()));
			row.add(new Label("\t -> " + editor.getSelectedOperator().name()));

			if(values != null)
			{
				for(final FilterValueEditorComposite<?, ?> value : values)
				{

					row.add(new Label("\t -> " + value.getValue()));
				}
			}
		}

		return row;

	}

	private HorizontalLayout createEntryRowCombo(final FilterEntryEditor editor)
	{
		final HorizontalLayout layout = new HorizontalLayout(editor);
		layout.setEnabled(true);
		layout.addClassName("filterEntryRow");
		return layout;
	}

	/**
	 * Create the ButtonLayout for the LabelDiv
	 *
	 * @param editButton
	 * @param checkbox
	 * @param deleteButton
	 * @return
	 */
	private HorizontalLayout
		createButtonLayoutLabel(final Component editButton, final Component checkbox, final Component deleteButton)
	{
		final HorizontalLayout layout = new HorizontalLayout();
		layout.addClassName("finalButtonLayout");
		layout.add(editButton, checkbox, deleteButton);
		return layout;
	}

	private FormLayout createButtonLayoutCombo(final Component checkbox, final Component deleteButton)
	{
		final FormLayout layout = new FormLayout();
		layout.addClassName("finalButtonLayout");
		layout.add(checkbox, deleteButton);
		return layout;
	}

	/**
	 * Creates the Row which is seen from the User.
	 *
	 *
	 * @param removeFilterButton:
	 *            A Button which will be more defined in this method -> DeleteButton
	 * @param checkbox:
	 *            A Checkbox which will be more defined in this method -> Checkbox to temporarily uncheck a filter
	 * @param filterEntryRow:
	 * @param editor:
	 *            The FilterEntryEditor from which you want to create a row
	 * @param index
	 * @return finalLayout: A Horizontal Layout
	 */
	private Component createFinalRow(
		final FilterEntryEditor editor,
		final int index)
	{

		final Button   deleteButton = createDeleteButton();
		final Checkbox checkbox     = new Checkbox();

		defineComponentsAtIndex(editor, deleteButton, checkbox, null, index);

		final FormLayout       finalButtonLayout = createButtonLayoutCombo(checkbox, deleteButton);
		final HorizontalLayout filterEntryRow    = createEntryRowCombo(editor);

		return createFinalLayout(filterEntryRow, finalButtonLayout);
	}

	/**
	 * Creates the Row which is then seen by the user.
	 * This Row holds the usable Buttons and the Labels which name the filter.
	 *
	 * @param editor
	 *            -> The FilterEntryEditor object
	 * @param index
	 *            -> Index where to attach the Row on the Div
	 * @return
	 */
	private Component createFinalRowLabel(
		final FilterEntryEditor editor,
		final int index)
	{
		final Button   deleteButton = createDeleteButton();
		final Checkbox checkbox     = new Checkbox();
		final Button   editButton   = createEditButton();

		defineComponentsAtIndex(editor, deleteButton, checkbox, editButton, index);

		final HorizontalLayout finalButtonLayout = createButtonLayoutLabel(editButton, checkbox, deleteButton);

		final HorizontalLayout filterEntryRow = createEntryRowLabel(index, editor);

		return createFinalLayout(filterEntryRow, finalButtonLayout);
	}

	/**
	 * Creates the Cancel Button within an PopUp Dialog.
	 *
	 * @param dialog:
	 *            The given PopUp Dialog
	 * @return A Button you can click on
	 */
	private Button createCancelButton(final Dialog dialog)
	{
		final Button button = new Button();

		button.setIcon(VaadinIcon.CLOSE.create());
		button.addClickListener(event -> dialog.close());
		button.addClassName("cancelButton");

		return button;
	}

	/**
	 * Creates the Confirm Button within the Dialog of the small view.
	 * <br>
	 * It is used to confirm the selected filter
	 *
	 * @return
	 */
	private Button createConfirmButton()
	{
		final Button button = new Button();
		button.setIcon(VaadinIcon.CHECK_CIRCLE.create());
		button.addClassName("confirmButton");
		return button;
	}

	/**
	 * Creates the Hide-Button which is used to 'open' or 'close' the current Div
	 *
	 * @return
	 */
	private HideButton createHideButton()
	{
		final HideButton button = new HideButton();
		button.setIcon(VaadinIcon.ANGLE_DOUBLE_LEFT.create());
		button.setOpen(false);
		button.addClassName("hideFilterButton");
		return button;
	}

	/**
	 * Creates the 'Add-Button' which is used to add a new Filter Row
	 *
	 * @return
	 */
	protected Button createAddButton()
	{
		final Button button = new Button();
		button.setIcon(VaadinIcon.PLUS.create());
		return button;
	}

	/**
	 * Creates the 'Remove-Button' which is used to remove the selected Row from the View and the Filter
	 *
	 * @return
	 */
	protected Button createRemoveButton()
	{
		final Button button = new Button();
		button.setIcon(VaadinIcon.MINUS.create());
		return button;
	}

	/**
	 * Creates the Edit Button which is only available in the small View
	 *
	 * @return
	 */
	private Button createEditButton()
	{
		final Button button = new Button();
		button.setIcon(VaadinIcon.EDIT.create());
		button.setClassName("editButton");
		return button;
	}

	/**
	 * Creates the DeleteButton which is used to remove a specific row
	 */
	private Button createDeleteButton()
	{
		final Button button = new Button();
		button.addClassName("deleteButton");
		button.setIcon(VaadinIcon.MINUS.create());
		return button;
	}

	/**
	 * Creates the CheckButton within the Dialog to confirm the task of deleting the row
	 *
	 * @param editor
	 * @param dialog
	 * @param index
	 * @return
	 */
	private Button createCheckButtonDialog(final FilterEntryEditor editor, final Dialog dialog, final int index)
	{
		final Button button = new Button();

		button.setIcon(VaadinIcon.CHECK.create());
		button.addClickListener(event -> {
			removeFilterEntryEditor(editor, index);
			this.rowIndex--;
			dialog.close();
		});
		button.addClassName("checkButton");
		return button;
	}

	/*****************************************************************************************************************************************
	 */
	/**************************************
	 ************ Defining Stuff***********
	 **************************************/

	private void defineComponentsAtIndex(
		final FilterEntryEditor editor,
		final Button deleteButton,
		final Checkbox checkbox,
		final Button editButton,
		final int index)
	{
		if(editButton != null)
		{
			editButton.addClickListener(listener -> editButtonClickListener(editor, index));
		}
		editor.addClassName("editor");

		defineCheckBox(checkbox);
		checkbox
			.addValueChangeListener(listener -> checkboxValueChanceListener(checkbox, index, editor));

		defineDeleteButton(deleteButton);
		deleteButton.addClickListener(listener -> deleteButtonClickListener(editor, index));
	}

	protected void defineCheckBox(final Checkbox checkbox)
	{
		checkbox.setValue(true);
		checkbox.addClassName("checkbox");
	}

	protected void defineDeleteButton(final Button deleteButton)
	{
		deleteButton.setIcon(VaadinIcon.MINUS.create());

	}

	/*****************************************************************************************************************************************
	 */
	/**************************************
	 ************ Listener***********
	 **************************************/

	/**
	 * * The Listener for the confirm Button inside the PopUp-Window of the small view.
	 * It's creating the see Filter Row
	 *
	 * @param deleteButton
	 * @param checkbox
	 * @param filterEntryRow
	 * @param editor
	 * @param index
	 * @param dialog
	 */
	private void createConfirmButtonListener(
		final FilterEntryEditor editor,
		final int index,
		final Dialog dialog)
	{
		// +1 because of search bar at top
		this.comboDiv.addComponentAtIndex(index,
			createFinalRow(editor, index));
		this.labelDiv.addComponentAtIndex(index, createFinalRowLabel(editor, index));

		dialog.close();
		if(this.comboDiv.isVisible())
		{
			closeDiv();
		}
	}

	/**
	 * Create the Click-Event for the Edit Button.
	 * This Button is only available in the small View.
	 *
	 * @param editor
	 *            -> The FilterEntryEditor object
	 */
	private void editButtonClickListener(final FilterEntryEditor editor, final int index)
	{
		final FilterEntryEditor oldEditor     = editor;
		final Button            confirmButton = createConfirmButton();
		final Dialog            dialog        = createPopUpDialog();
		confirmButton.addClickListener(listener -> {
			replaceDivEditor(this.comboDiv, oldEditor, editor, index);
			dialog.close();
		});
		dialog.add(editor, confirmButton);
		dialog.open();
	}

	/**
	 * Creates a saftey Message to check, if the row should really be deleted
	 *
	 * @param editor
	 */
	protected void deleteButtonClickListener(final FilterEntryEditor editor, final int index)
	{
		final Dialog dialog       = new Dialog();
		final Label  label        = new Label(StringResourceUtils.getResourceString("deleteMessage", this));
		final Button deleteButton = createCheckButtonDialog(editor, dialog, index);
		final Button cancelButton = createCancelButton(dialog);

		final HorizontalLayout buttonLayout = new HorizontalLayout();
		buttonLayout.add(deleteButton, cancelButton);

		final VerticalLayout dialogLayout = new VerticalLayout();
		dialogLayout.add(label, buttonLayout);

		dialog.add(dialogLayout);
		dialog.open();
	}

	/**
	 * Defines the ClickEvent which is activated by clicking on the 'Hide-Button'
	 */
	protected void hideButtonClickListener()
	{
		if(!this.hideFilterButton.isOpen())
		{
			openDiv();
		}
		else if(this.hideFilterButton.isOpen())
		{
			closeDiv();
		}
	}

	protected void
		checkboxValueChanceListener(final Checkbox checkbox, final int index, final FilterEntryEditor editor)
	{
		final Boolean check = checkbox.getValue();
		if(Boolean.TRUE.equals(check))
		{
			activateFilterEntryEditor(index, editor);
		}
		else if(Boolean.FALSE.equals(check))
		{
			deactivateFilterEntryEditor(editor);
		}
	}

	/*****************************************************************************************************************************************
	 */
	/**************************************
	 ************ Updating Stuff***********
	 **************************************/

	protected void updateFilterData()
	{
		final String        searchTerm = this.searchTextField.getValue();
		final FilterEntry[] entries    = this.filterEntryEditors.stream()
			.map(FilterEntryEditor::getFilterEntry).filter(Objects::nonNull)
			.toArray(FilterEntry[]::new);
		setModelValue(new FilterData(searchTerm, entries), false);
	}

	public void reset()
	{
		setValue(new FilterData());
	}

	/**
	 * updates the LabelDiv if something's change in a ComboBox
	 * Neverthless where this ComboBox is
	 * -> Big View
	 * -> PopUp
	 *
	 * @param index
	 *            -> The Index of the row who was changed
	 * @param combo
	 *            -> The ComboBox which was updated
	 * @param values
	 *            -> The List of values (only important if an Operator was changed)
	 */
	@SuppressWarnings("rawtypes")
	protected void updateFilterDivLabel(
		final int index,
		final ComboBox<?> combo,
		final List<FilterValueEditorComposite> values)
	{
		
		// Notification.show("Index of Row" + index);
		final Object comboObject = combo.getValue();

		if(comboObject instanceof FilterProperty<?>)
		{
			final FilterProperty<?> property = (FilterProperty<?>)comboObject;
			setFilterLabelDivByProperty(property, index);
		}
		else if(comboObject instanceof FilterOperator)
		{
			final FilterOperator operator = (FilterOperator)comboObject;
			setFilterLabelDivByOperator(operator, values, index);
		}

	}

	/**
	 * Searches for the old Editor inside the filterDiv<b>ComboBox</b>
	 * and replace it with the new one
	 *
	 * @param oldEditor
	 * @param newEditor
	 */
	private void
		replaceDivEditor(
			final Div div,
			final FilterEntryEditor oldEditor,
			final FilterEntryEditor newEditor,
			final int index)
	{
		int count = 0;
		for(final Object obj : div.getChildren().toArray())
		{
			if(count == index)
			{
				final HorizontalLayout filterEntryRow = getFilterEntryRowFromDiv(obj);
				filterEntryRow.replace(oldEditor, newEditor);
			}
			count++;
		}
	}

	/**
	 * Add the Filter to the List.
	 * <br>
	 * This method just select the Filter but not make it visible
	 *
	 * @param editor
	 */
	protected void activateFilterEntryEditor(final int index, final FilterEntryEditor editor)
	{
		this.filterEntryEditors.add(index, editor);
		updateFilterData();
	}

	/**
	 * Removes the Filter from the List. This just deselect the Filter but allows the User to reactivate it.
	 * <br>
	 * The Filter will not be removed from the view
	 *
	 * @param editor
	 */
	protected void deactivateFilterEntryEditor(final FilterEntryEditor editor)
	{
		this.filterEntryEditors.remove(editor);
		updateFilterData();
	}

	/*****************************************************************************************************************************************
	 */
	/**************************************
	 ************ Removing Stuff***********
	 **************************************/

	/**
	 * Removes the selected Filter from the list and the view
	 *
	 * @param editor
	 *            -> the Object to remove
	 * @param index
	 *            -> The index of the row on which the object is appended
	 */
	protected void removeFilterEntryEditor(final FilterEntryEditor editor, final int index)
	{
		removeFilterEntryEditorComponent(editor, index);

		this.filterEntryEditors.remove(editor);

		updateFilterData();
	}

	/**
	 * Remove the FilterEntryEditor Object from the whole Div (index < 0)
	 * or from the specific index
	 *
	 * @param editor
	 *            -> The Object which should be removed
	 * @param index
	 *            -> The index which Child should remove (index < 0 deletes all Childs of the Div)
	 */
	protected void removeFilterEntryEditorComponent(final FilterEntryEditor editor, final int index)
	{
		if(index > -1)
		{
			removeFromDivAtIndex(this.comboDiv, index);
			removeFromDivAtIndex(this.labelDiv, index);
		}
		else
		{
			final Component filterEntryRow = editor.getParent().get();
			final Component finalRow       = filterEntryRow.getParent().get();
			this.comboDiv.remove(finalRow);
		}

	}

	/**
	 * Go through given List and removes the one on the correct index
	 *
	 * @param index
	 */
	private void removeFromDivAtIndex(final Div div, final int index)
	{
		int count = 0;
		for(final Object obj : div.getChildren().toArray())
		{
			if(count == index)
			{
				div.remove((Component)obj);
			}
			count++;
		}
	}

	/*****************************************************************************************************************************************
	 */
	/**************************************
	 ************ Swapping Stuff***********
	 **************************************/

	/**
	 * Switches the Divs depending on the current width.
	 * Breakpoint for a small view -> 727 px
	 */
	private void swapDivs()
	{
		if(this.windowWidth < BREAKPOINT)
		{
			swapToLabel();
		}
		else
		{
			swapToComboBox();
		}
	}

	/**
	 * This Method will switch from the Div with <b>ComboBoxes</b>
	 * to the Div with <b>Labels</b>
	 */
	private void swapToLabel()
	{
		if(this.hideFilterButton.isOpen())
		{
			this.labelDiv.setVisible(true);
		}
		else
		{
			this.labelDiv.setVisible(false);
		}
		this.comboDiv.setVisible(false);
		this.getContent().replace(this.comboDiv, this.labelDiv);
	}

	/**
	 * This Method will switch from the Div with <b>Labels</b>
	 * to the Div with <b>ComboBoxes</b>
	 */
	private void swapToComboBox()
	{

		if(this.hideFilterButton.isOpen())
		{
			this.comboDiv.setVisible(true);
		}
		else
		{
			this.comboDiv.setVisible(false);
		}
		this.labelDiv.setVisible(false);
		this.getContent().replace(this.labelDiv, this.comboDiv);
	}

	/**
	 * Opens the current Div by clicking on the Hide-Button
	 */
	protected void openDiv()
	{
		if(this.windowWidth >= BREAKPOINT)
		{
			this.hideFilterButton.setIcon(VaadinIcon.ANGLE_DOUBLE_DOWN.create());
			this.hideFilterButton.setOpen(true);
			this.comboDiv.setVisible(true);
		}
		else
		{
			this.hideFilterButton.setIcon(VaadinIcon.ANGLE_DOUBLE_DOWN.create());
			this.hideFilterButton.setOpen(true);
			this.labelDiv.setVisible(true);
		}

	}

	/**
	 * Closes the current Div by clicking on the Hide-Button
	 */
	protected void closeDiv()
	{
		if(this.windowWidth >= BREAKPOINT)
		{
			this.comboDiv.setVisible(false);
			this.hideFilterButton.setIcon(VaadinIcon.ANGLE_DOUBLE_LEFT.create());
			this.hideFilterButton.setOpen(false);
		}
		else
		{
			this.labelDiv.setVisible(false);
			this.hideFilterButton.setIcon(VaadinIcon.ANGLE_DOUBLE_LEFT.create());
			this.hideFilterButton.setOpen(false);
		}

	}

	/*****************************************************************************************************************************************
	 */
	/**************************************
	 ************ Getter / Adder / Setter**
	 **************************************/

	/**************************************
	 * More Important***********
	 **************************************/

	/**
	 * Updates the Entry Row inside the LabelDiv with the given FilterProperty
	 *
	 * @param operator
	 */
	private void setFilterLabelDivByProperty(final FilterProperty<?> property, final int index)
	{
		this.filterProperty.add(index, property.caption());
		int counter = 0;
		for(final Object obj : this.labelDiv.getChildren().toArray())
		{
			if(counter == index)
			{
				final HorizontalLayout filterEntryRow = getFilterEntryRowFromDiv(obj);
				filterEntryRow.removeAll();
				filterEntryRow.add(new Label(this.filterProperty.get(index)));
			}
			counter++;
		}
	}

	/**
	 * Updates the Filter Entry Row inside the LabelDiv with the given Operator and
	 * - if present -
	 * the given values
	 *
	 * @param operator
	 *            -> The FilterOperator Object
	 * @param values
	 *            -> The list of values attached to the FilterOperator
	 */
	@SuppressWarnings("rawtypes")
	private void
		setFilterLabelDivByOperator(
			final FilterOperator operator,
			final List<FilterValueEditorComposite> values,
			final int index)
	{
		int counter = 0;
		for(final Object obj : this.labelDiv.getChildren().toArray())
		{
			if(counter == index)
			{
				final HorizontalLayout filterEntryRow = getFilterEntryRowFromDiv(obj);
				filterEntryRow.removeAll();
				filterEntryRow.add(new Label(this.filterProperty.get(index)));
				filterEntryRow.add(new Label(" -> " + operator.name()));
				if(values != null)
				{
					for(final FilterValueEditorComposite<?, ?> value : values)
					{

						filterEntryRow.add(new Label(" -> " + value.getValue()));
					}
				}
			}
			counter++;
		}
	}

	/**
	 * Return the filterEntryRow from an Object,
	 * which is a Child of either the ComboBoxDiv or the LabelDiv
	 *
	 * @param obj
	 *            -> The Child Object
	 * @return -> HorizontalLayout (the filterEntryRow)
	 */
	private HorizontalLayout getFilterEntryRowFromDiv(final Object obj)
	{
		final HorizontalLayout row = (HorizontalLayout)obj;
		return (HorizontalLayout)row.getComponentAt(0);
	}

	@Override
	protected void setPresentationValue(final FilterData filterData)
	{
		for(final FilterEntryEditor editor : this.filterEntryEditors)
		{
			removeFilterEntryEditorComponent(editor, -1);
		}
		this.filterEntryEditors.clear();

		if(filterData != null)
		{
			this.searchTextField.setValue(filterData.getSearchTerm());

			final FilterEntry[] filterEntries = filterData.getEntries();
			if(filterEntries != null)
			{
				for(final FilterEntry filterEntry : filterEntries)
				{
					addFilterEntryEditor(this.filterEntryEditors.size())
						.setFilterEntry(filterEntry);
				}
			}
		}
		else
		{
			this.searchTextField.setValue("");
		}
	}

	public Filter getFilter()
	{
		final Filter searchFilter = createSearchFilter();
		final Filter valueFilter  = createValueFilter();
		if(searchFilter != null && valueFilter != null)
		{
			return Composite.New(getSearchAndFilterConnector(), searchFilter, valueFilter);
		}
		if(searchFilter != null)
		{
			return searchFilter;
		}
		if(valueFilter != null)
		{
			return valueFilter;
		}
		return null;
	}

	public void setFilterSubject(final FilterSubject filterSubject)
	{
		this.filterSubject = filterSubject;

		final boolean hasSubject = this.filterSubject != null;
		this.searchTextField.setEnabled(hasSubject);
		this.addFilterButton.setEnabled(hasSubject);

		if(hasSubject)
		{
			final String res         = StringResourceUtils.getResourceString("searchTextFieldInputPrompt",
				this);
			final String properties  = filterSubject.searchableProperties().stream()
				.map(p -> p.caption()).collect(Collectors.joining(", "));
			final String placeholder = MessageFormat.format(res, properties);
			this.searchTextField.setPlaceholder(placeholder);
		}
		else
		{
			this.searchTextField.setPlaceholder("");
		}

		reset();
	}

	public <T> void addSubsetDataProvider(final Class<T> type, final SubsetDataProvider<T> provider)
	{
		getSubsetDataProviderFactoryRegistry().put(SubsetDataProviderFactory.New(type, provider));
	}

	public void addSubsetDataProvider(
		final BiPredicate<FilterContext, FilterProperty<?>> predicate,
		final SubsetDataProvider<?> provider)
	{
		getSubsetDataProviderFactoryRegistry()
			.put(SubsetDataProviderFactory.New(predicate, provider));
	}

	public void addSubsetDataProvider(
		final Predicate<FilterProperty<?>> predicate,
		final SubsetDataProvider<?> provider)
	{
		getSubsetDataProviderFactoryRegistry()
			.put(SubsetDataProviderFactory.New(predicate, provider));
	}

	/**
	 * Main Method to initialize the Filter Components
	 *
	 * @param index
	 * @return
	 */
	protected FilterEntryEditor addFilterEntryEditor(final int index)
	{
		final FilterEntryEditor editor = new FilterEntryEditor(this, this, this::updateFilterData);
		this.filterEntryEditors.add(index, editor);

		final HorizontalLayout filterEntryRow = new HorizontalLayout(editor);

		if(this.windowWidth < 727)
		{
			final Dialog dialog = createPopUpDialog();

			final Button confirmButton = createConfirmButton();
			confirmButton.addClickListener(
				e -> createConfirmButtonListener(editor, index,
					dialog));

			dialog.add(filterEntryRow, confirmButton);
			dialog.open();
		}
		else if(this.windowWidth >= 727)
		{
			openDiv();

			this.comboDiv.addComponentAtIndex(index,
				createFinalRow(editor, index));
			this.labelDiv.addComponentAtIndex(index,
				createFinalRowLabel(editor, index));

		}
		this.rowIndex++;
		return editor;

	}

	/**************************************
	 * Less Important***********
	 **************************************/

	public void setSearchText(final String searchText)
	{
		this.searchTextField.setValue(searchText != null ? searchText : "");

		updateFilterData();
	}

	@Override
	public boolean isCaseSensitive()
	{
		return this.caseSensitive;
	}

	public void setCaseSensitive(final boolean caseSensitive)
	{
		this.caseSensitive = caseSensitive;
	}

	@Override
	public char getWildcard()
	{
		return this.wildcard;
	}

	public void setWildcard(final char wildcard)
	{
		this.wildcard = wildcard;
	}

	@Override
	public Connector getSearchPropertiesConnector()
	{
		return this.searchPropertiesConnector;
	}

	public void setSearchPropertiesConnector(final Connector searchPropertiesConnector)
	{
		this.searchPropertiesConnector = searchPropertiesConnector;
	}

	@Override
	public Connector getSearchMultiWordConnector()
	{
		return this.searchMultiWordConnector;
	}

	public void setSearchMultiWordConnector(final Connector searchMultiWordConnector)
	{
		this.searchMultiWordConnector = searchMultiWordConnector;
	}

	@Override
	public Connector getFilterPropertiesConnector()
	{
		return this.filterPropertiesConnector;
	}

	public void setFilterPropertiesConnector(final Connector filterPropertiesConnector)
	{
		this.filterPropertiesConnector = filterPropertiesConnector;
	}

	@Override
	public Connector getSearchAndFilterConnector()
	{
		return this.searchAndFilterConnector;
	}

	public void setSearchAndFilterConnector(final Connector searchAndFilterConnector)
	{
		this.searchAndFilterConnector = searchAndFilterConnector;
	}

	public SearchFilterGenerator getSearchFilterGenerator()
	{
		return this.searchFilterGenerator;
	}

	public void setSearchFilterGenerator(final SearchFilterGenerator searchFilterGenerator)
	{
		this.searchFilterGenerator = searchFilterGenerator;
	}

	@Override
	public FilterOperatorRegistry getFilterOperatorRegistry()
	{
		return this.filterOperatorRegistry;
	}

	public void setFilterOperatorRegistry(final FilterOperatorRegistry filterOperatorRegistry)
	{
		this.filterOperatorRegistry = filterOperatorRegistry;
	}

	@Override
	public SubsetDataProviderFactoryRegistry getSubsetDataProviderFactoryRegistry()
	{
		return this.subsetDataProviderFactoryRegistry;
	}

	public void setSubsetDataProviderFactoryRegistry(
		final SubsetDataProviderFactoryRegistry subsetDataProviderFactoryRegistry)
	{
		this.subsetDataProviderFactoryRegistry = subsetDataProviderFactoryRegistry;
	}

	@Override
	public FilterSubject getFilterSubject()
	{
		return this.filterSubject;
	}

	public int getIndex()
	{
		return this.rowIndex;
	}

	public String getSearchText()
	{
		return this.searchTextField != null ? this.searchTextField.getValue() : "";
	}

}
