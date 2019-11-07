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
import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.checkbox.Checkbox;
import com.vaadin.flow.component.combobox.ComboBox;
import com.vaadin.flow.component.dependency.JavaScript;
import com.vaadin.flow.component.dependency.StyleSheet;
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
		this.searchTextField = createSearchTextField();
		this.searchTextField.addValueChangeListener(event -> updateFilterData());
		this.searchTextField.setEnabled(false);
		
		this.hideFilterButton = createHideButton();
		this.hideFilterButton.addClickListener(listener -> hideButtonClickListener());
		
		this.comboDiv = createComboBoxDiv();
		this.labelDiv = createLabelDiv();
		
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
	 * Create the TopContent with the searchbar and the filter{@link Div}
	 *
	 * @param searchBar
	 *            as {@link HorizontalLayout}
	 * @return The content as {@link VerticalLayout}
	 */
	private VerticalLayout createContent(final HorizontalLayout searchBar)
	{
		final VerticalLayout content = new VerticalLayout(searchBar, this.filterDiv);
		content.setMargin(false);
		content.setPadding(false);
		content.setSpacing(true);
		return content;
	}
	
	/**
	 * Creating the Searchbar
	 *
	 * @return The searchBar as {@link HorizontalLayout}
	 */
	private HorizontalLayout createSearchBar()
	{
		final HorizontalLayout searchBar = new HorizontalLayout(this.searchTextField, this.hideFilterButton);
		searchBar.setMargin(false);
		searchBar.setPadding(false);
		searchBar.expand(this.searchTextField);
		searchBar.setWidth("100%");
		return searchBar;
	}
	
	/**
	 * Creating the Search Text Field
	 *
	 * @return The TextField as {@link TextField}
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
	 *            -> The left part of the Layout with the needed Editor as {@link Component}
	 * @param finalButtonLayout
	 *            -> The right part of the Layout with the needed Buttons as {@link Component}
	 * @return The final Layout as {@link HorizontalLayout}
	 *         <br>
	 *         Classname = finalLayout
	 */
	private HorizontalLayout createFinalLayout(final Component filterEntryRow, final Component finalButtonLayout)
	{
		final HorizontalLayout layout = new HorizontalLayout();
		layout.add(filterEntryRow, finalButtonLayout);
		layout.addClassName("finalLayout");
		return layout;
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
	 * Creating the Div which is used to hold the labels beyond the Comboboxes
	 *
	 *
	 * @return The Label Div as {@link Div}
	 *         <br>
	 *         Classname = labelDiv
	 */
	private Div createLabelDiv()
	{
		final Div filter = new Div();
		filter.setVisible(false);
		filter.setWidthFull();
		filter.addClassName("labelDiv");
		return filter;
	}
	
	/**
	 * Creating the Div which is used to hold the ComboBoxes under the Labels
	 *
	 * @return The ComboDiv as {@link Div}
	 *         <br>
	 *         Classname = comboBoxDiv
	 */
	private Div createComboBoxDiv()
	{
		final Div filter = new Div();
		filter.setVisible(true);
		filter.setWidthFull();
		filter.addClassName("comboBoxDiv");
		return filter;
	}
	
	/**
	 * Creates the Filter Entry Row for the <b> Label Div </b>
	 *
	 * @param index
	 *            -> {@link Integer}
	 * @param editor
	 *            -> {@link FilterEntryEditor}
	 * @return The EntryRow as {@link HorizontalLayout}
	 *         <br>
	 *         Classname = entryRowLabel
	 */
	@SuppressWarnings("rawtypes")
	private HorizontalLayout createEntryRowLabel(final int index, final FilterEntryEditor editor)
	{
		final HorizontalLayout row = new HorizontalLayout();
		row.addClassName("entryRowLabel");
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
	
	/**
	 * Creates the Filter Entry Row for the <b> Combo Div </b>
	 *
	 * @param editor
	 *            -> {@link FilterEntryEditor}
	 * @return The Entry Row as {@link HorizontalLayout}
	 *         <br>
	 *         Classname = entryRowComboBox
	 */
	private HorizontalLayout createEntryRowCombo(final FilterEntryEditor editor)
	{
		final HorizontalLayout layout = new HorizontalLayout(editor);
		layout.setEnabled(true);
		layout.addClassName("entryRowComboBox");
		return layout;
	}
	
	/**
	 * Create the ButtonLayout for the LabelDiv
	 *
	 * @param editButton
	 *            -> {@link Component}
	 * @param checkbox
	 *            -> {@link Component}
	 * @param deleteButton
	 *            -> {@link Component}
	 * @return The ButtonLayout as {@link HorizontalLayout}
	 *         <br>
	 *         Classname = buttonLayout
	 */
	private HorizontalLayout
		createButtonLayoutLabel(final Component editButton, final Component checkbox, final Component deleteButton)
	{
		final HorizontalLayout layout = new HorizontalLayout();
		layout.addClassName("buttonLayout");
		layout.add(editButton, checkbox, deleteButton);
		return layout;
	}
	
	/**
	 * Creates the finished ButtonLayout for the <b> ComboBox Div </b>
	 *
	 * @param checkbox
	 *            -> {@link Component}
	 * @param deleteButton
	 *            -> {@link Component}
	 * @return The Button Layout as {@link FormLayout}
	 *         <br>
	 *         Classname = buttonLayout
	 */
	private FormLayout createButtonLayoutCombo(final Component checkbox, final Component deleteButton)
	{
		final FormLayout layout = new FormLayout();
		layout.addClassName("buttonLayout");
		layout.add(checkbox, deleteButton);
		return layout;
	}
	
	/**
	 * Creates the Row which is seen from the User.
	 *
	 * @param editor
	 *            -> {@link FilterEntryEditor}
	 * @param index
	 *            -> {@link Integer}
	 * @return The finished Row as {@link Component}
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
	 *            -> {@link FilterEntryEditor}
	 * @param index
	 *            -> {@link Integer}
	 * @return The final Row for the LabelDiv as {@link Component}
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
	 * Creates the Hide-Button which is used to 'open' or 'close' the current Div
	 *
	 * @return The Hide Button as {@link HideButton}
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
	 * @return The Add-Button as {@link Button}
	 */
	protected Button createAddButton()
	{
		final Button button = new Button();
		button.setIcon(VaadinIcon.PLUS.create());
		return button;
	}
	
	/**
	 * Creates the Edit Button which is used to edit the Filter who are already set
	 *
	 * @return The Edit-Button as {@link Button}
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
	 *
	 * @return The Delete-Button as {@link Button}
	 */
	private Button createDeleteButton()
	{
		final Button button = new Button();

		defineDeleteButton(button);
		return button;
	}
	
	/*****************************************************************************************************************************************
	 */
	/**************************************
	 ************ Defining Stuff***********
	 **************************************/
	
	/**
	 * More specific defining of a Component at an index (Classnames, Listener, etc)
	 *
	 * @param editor
	 *            -> {@link FilterEntryEditor} / Classname = editor
	 * @param deleteButton
	 *            -> {@link Button}
	 * @param checkbox
	 *            -> {@link Checkbox}
	 * @param editButton
	 *            -> {@link Button}
	 * @param index
	 *            -> {@link Integer}
	 */
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
		deleteButtonClickListener(deleteButton, this.labelDiv, index);
	}
	
	/**
	 * Specification of a checkbox
	 *
	 * @param checkbox
	 *            -> {@link Checkbox}
	 *            <br>
	 *            Classname = checkbox
	 */
	protected void defineCheckBox(final Checkbox checkbox)
	{
		checkbox.setValue(true);
		checkbox.addClassName("checkbox");
	}
	
	/**
	 * Specification of the deleteButton
	 *
	 * @param deleteButton
	 *            -> {@link Button}
	 *            <br>
	 *            Classname = deleteButton
	 */
	protected void defineDeleteButton(final Button deleteButton)
	{
		deleteButton.addClassName("deleteButton");
		deleteButton.setIcon(VaadinIcon.MINUS.create());
		
	}
	
	/*****************************************************************************************************************************************
	 */
	/**************************************
	 ************ Listener***********
	 **************************************/
	
	/**
	 * Create the Click-Event for the Edit Button.
	 *
	 * @param editor
	 *            -> {@link FilterEntryEditor}
	 * @param index
	 *            -> {@link Integer}
	 */
	private void editButtonClickListener(final FilterEntryEditor editor, final int index)
	{
		final FilterEntryEditor oldEditor = editor;
		
		/*
		 * TODO:
		 * ClickListener so programmieren, das er die ComboBoxen in der anderen Div mit den Werten füllt
		 */
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
	
	/**
	 * Defines the ValueChanceListener if the Checkbox of a row is selected or deselected
	 *
	 * @param checkbox
	 *            -> {@link Checkbox}
	 * @param index
	 *            -> {@link Integer}
	 * @param editor
	 *            -> {@link FilterEntryEditor}
	 */
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

	/**
	 * Deletes the selected Row from the given {@link Div}
	 * 
	 * @param button
	 *            -> {@link Button}
	 * @param div
	 *            -> {@link Div}
	 * @param index
	 *            -> {@link Integer}
	 */
	private void deleteButtonClickListener(final Button button, final Div div, final int index)
	{
		button.addClickListener(listener -> {
			removeFromDivAtIndex(div, index);
			this.rowIndex--;
		});
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
	 * Updates the LabelDiv if something's change in a ComboBox
	 * No matter where this ComboBox is
	 * -> Big View
	 * -> PopUp
	 *
	 * @param index
	 *            -> {@link Integer} of the changed row
	 * @param combo
	 *            -> {@link ComboBox} which was updated
	 * @param values
	 *            -> {@link List} of {@link FilterValueEditorComposite} which are depending on the
	 *            {@link FilterOperator}
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
	 * TODO: vll wird das noch gebraucht?
	 *
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
	 *            -> {@link FilterEntryEditor}
	 * @param index
	 *            -> {@link Integer}
	 */
	protected void activateFilterEntryEditor(final int index, final FilterEntryEditor editor)
	{
		this.filterEntryEditors.add(index, editor);
		updateFilterData();
	}
	
	/**
	 * Removes the Filter from the List.
	 * <br>
	 * This just deselect the Filter but allows the User to reactivate it.
	 * <br>
	 * The Filter will not be removed from the view
	 *
	 * @param editor
	 *            -> {@link FilterEntryEditor}
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
	 * Removes the selected Filter from the filter list and the view
	 *
	 * @param editor
	 *            -> {@link FilterEntryEditor}
	 * @param index
	 *            -> {@link Integer}
	 */
	protected void removeFilterEntryEditor(final FilterEntryEditor editor, final int index)
	{
		removeFromDivAtIndex(this.labelDiv, index);
		
		this.filterEntryEditors.remove(editor);
		
		updateFilterData();
	}
	
	/**
	 * Go through the childs of given Div and remove object on index
	 *
	 * @param div
	 *            -> {@link Div}
	 * @param index
	 *            -> {@link Integer} (has to be >-1)
	 */
	private void removeFromDivAtIndex(final Div div, final int index)
	{
		if(index > -1)
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
	}
	
	/*****************************************************************************************************************************************
	 */
	/**************************************
	 ************ Swapping Stuff***********
	 **************************************/
	
	/**
	 * Opens the filter Div by clicking on the Hide-Button
	 * <br>
	 * Also setting 'open' in the {@link HideButton} to true
	 */
	protected void openDiv()
	{
		this.hideFilterButton.setIcon(VaadinIcon.ANGLE_DOUBLE_DOWN.create());
		this.hideFilterButton.setOpen(true);
		this.filterDiv.setVisible(true);
	}
	
	/**
	 * Closes the filter Div by clicking on the Hide-Button
	 * <br>
	 * Also setting 'open' in the {@link HideButton} to false
	 */
	protected void closeDiv()
	{
		this.hideFilterButton.setIcon(VaadinIcon.ANGLE_DOUBLE_LEFT.create());
		this.hideFilterButton.setOpen(false);
		this.filterDiv.setVisible(false);
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
	 * Updates the Entry Row inside the LabelDiv with the given FilterProperty at the index
	 *
	 * @param operator
	 *            -> {@link FilterProperty}
	 * @param index
	 *            -> {@link Integer}
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
	 * <br>
	 * - if present - <br>
	 * the given values at the index
	 *
	 * @param operator
	 *            -> {@link FilterOperator}
	 * @param values
	 *            -> {@link List} of {@link FilterValueEditorComposite}
	 * @param index
	 *            -> {@link Integer}
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
	 *            -> {@link Object}
	 * @return The EntryRow as {@link HorizontalLayout}
	 */
	private HorizontalLayout getFilterEntryRowFromDiv(final Object obj)
	{
		final HorizontalLayout row = (HorizontalLayout)obj;
		return (HorizontalLayout)row.getComponentAt(0);
	}
	
	@Override
	protected void setPresentationValue(final FilterData filterData)
	{

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
	 *            -> {@link Integer}
	 * @return The EntryEditor as {@link FilterEntryEditor}
	 */
	protected FilterEntryEditor addFilterEntryEditor(final int index)
	{
		final FilterEntryEditor editor = new FilterEntryEditor(this, this, this::updateFilterData);
		this.filterEntryEditors.add(index, editor);
		
		openDiv();
		
		this.comboDiv.addComponentAtIndex(index,
			createFinalRow(editor, index));
		this.labelDiv.addComponentAtIndex(index,
			createFinalRowLabel(editor, index));
		
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
