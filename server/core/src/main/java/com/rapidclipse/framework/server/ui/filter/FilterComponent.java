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
import com.rapidclipse.framework.server.ui.filter.readabelHelper.AddButton;
import com.rapidclipse.framework.server.ui.filter.readabelHelper.CancelButton;
import com.rapidclipse.framework.server.ui.filter.readabelHelper.DeleteButton;
import com.rapidclipse.framework.server.ui.filter.readabelHelper.EditButton;
import com.rapidclipse.framework.server.ui.filter.readabelHelper.FilterCheckBox;
import com.rapidclipse.framework.server.ui.filter.readabelHelper.HideButton;
import com.rapidclipse.framework.server.ui.filter.readabelHelper.ReplaceabelEditor;
import com.rapidclipse.framework.server.ui.filter.readabelHelper.UpdateButton;
import com.rapidclipse.framework.server.util.ServiceLoader;
import com.vaadin.flow.component.AbstractCompositeField;
import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.HasOrderedComponents;
import com.vaadin.flow.component.HasSize;
import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.checkbox.Checkbox;
import com.vaadin.flow.component.dependency.JavaScript;
import com.vaadin.flow.component.dependency.StyleSheet;
import com.vaadin.flow.component.formlayout.FormLayout;
import com.vaadin.flow.component.grid.Grid;
import com.vaadin.flow.component.html.Div;
import com.vaadin.flow.component.html.Label;
import com.vaadin.flow.component.icon.VaadinIcon;
import com.vaadin.flow.component.notification.Notification;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import com.vaadin.flow.component.textfield.TextField;
import com.vaadin.flow.data.provider.ConfigurableFilterDataProvider;
import com.vaadin.flow.data.provider.DataProvider;
import com.vaadin.flow.data.value.ValueChangeMode;
import com.vaadin.flow.dom.Element;
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
	private AddButton                     addFilterButton;
	private HideButton                    hideFilterButton;
	private Div                           filterDiv;
	private Div                           comboDiv;
	protected Div                         labelDiv           = new Div();
	private final List<ReplaceabelEditor> filterEntryEditors = new ArrayList<>();
	private int                           rowIndex           = 0;                // needed to add filter in the right
																					// place of this.filterEntryEditors
	protected List<String>                filterProperty     = new ArrayList<>();
	protected List<String>                filterOperator     = new ArrayList<>();
	private HorizontalLayout              searchBar;
	private Registration                  addButtonClick;
	
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
		
		this.hideFilterButton = new HideButton();
		this.hideFilterButton.defineButton();
		this.hideFilterButton.addClickListener(listener -> hideButtonClickListener());
		
		this.comboDiv = createComboBoxDiv();
		this.labelDiv = createLabelDiv();
		
		this.addFilterButton = new AddButton();
		this.addFilterButton.defineButton();
		this.addButtonClick = this.addFilterButton.addClickListener(event -> {
			this.searchBar.remove(this.addFilterButton);
			addFilterEntryEditor(new FilterEntryEditor(this, this, this::updateFilterData),
				this.rowIndex);
		});
		this.addFilterButton.setEnabled(false);
		this.filterDiv = createFilterDiv();
		
		this.searchBar = createSearchBar();
		
		return createContent(this.searchBar);
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
	 *            -> {@link HorizontalLayout}
	 * @return The content -> {@link VerticalLayout}
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
		final HorizontalLayout searchBar =
			new HorizontalLayout(this.searchTextField, this.hideFilterButton, this.addFilterButton);
		searchBar.setMargin(false);
		searchBar.setPadding(false);
		searchBar.expand(this.searchTextField);
		searchBar.setWidth("100%");
		return searchBar;
	}
	
	/**
	 * Creating the Search Text Field where the user can search for anything
	 * <br>
	 * Input will be compared to every column
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
	 *            <br>
	 *            Can be created with {@link #createEntryRowLabel(FilterEntryEditor)} or
	 *            {@link #createEntryRowCombo(FilterEntryEditor)}
	 * @param finalButtonLayout
	 *            -> The right part of the Layout with the needed Buttons as {@link Component}
	 *            <br>
	 *            Can be created with {@link #createButtonLayoutLabel(Component, Component, Component)} or
	 *            {@link #createButtonLayoutCombo(Component, Component)}
	 * @return The final Layout as {@link HorizontalLayout}
	 *         <br>
	 *         Classname = finalLayout
	 */
	private HorizontalLayout createFinalLayout(
		final Component filterEntryRow,
		final Component finalButtonLayout)
	{
		final HorizontalLayout layout = new HorizontalLayout();
		layout.add(filterEntryRow, finalButtonLayout);
		layout.addClassName(StringResourceUtils.getResourceString("finalLayout", this));
		return layout;
	}
	
	protected Filter createValueFilter()
	{
		if(this.filterEntryEditors == null || this.filterEntryEditors.isEmpty())
		{
			return null;
		}
		
		final List<Filter> valueFilters = this.filterEntryEditors.stream()
			.map(editor -> editor.getOriginal().getFilter()).filter(Objects::nonNull)
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
	 *         Classname = labelDiv -> getting through
	 *         {@link StringResourceUtils #getResourceString(String, java.util.Locale)}
	 */
	private Div createLabelDiv()
	{
		final Div filter = new Div();
		filter.setVisible(false);
		filter.setWidthFull();
		filter.addClassName(StringResourceUtils.getResourceString("labelDiv", this));
		return filter;
	}
	
	/**
	 * Creating the Div which is used to hold the ComboBoxes under the Labels
	 *
	 * @return The ComboDiv as {@link Div}
	 *         <br>
	 *         Classname = comboBoxDiv -> getting through
	 *         {@link StringResourceUtils #getResourceString(String, java.util.Locale)}
	 */
	private Div createComboBoxDiv()
	{
		final Div div = new Div();
		defineComboBoxDiv(div);
		return div;
	}
	
	/**
	 * Creates the whole Div which holds the <b>labelDiv</b> and the <b>comboDiv</b>.
	 * <br>
	 * Both can be created through:
	 * <br>
	 * {@link #createLabelDiv()}
	 * <br>
	 * {@link #createComboBoxDiv()}
	 *
	 * @return -> The filterDiv as {@link Div}
	 */
	private Div createFilterDiv()
	{
		final Div div = new Div();
		div.add(this.labelDiv, this.comboDiv);
		defineFilterDiv(div);
		return div;
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
	 *         Classname = entryRowLabel
	 */
	@SuppressWarnings("rawtypes")
	private HorizontalLayout createEntryRowLabel(final FilterEntryEditor editor)
	{
		final HorizontalLayout row = new HorizontalLayout();
		row.addClassName(StringResourceUtils.getResourceString("entryRowLabel", this));
		row.setEnabled(true);
		
		final List<FilterValueEditorComposite> values = editor.getValueEditors();
		
		row.add(new Label("" + editor.getSelectedProperty().caption()));
		this.filterProperty.add(editor.getSelectedProperty().caption());
		row.add(new Label("\t -> " + editor.getSelectedOperator().name()));
		this.filterOperator.add(editor.getSelectedOperator().name());
		if(values != null)
		{
			for(final FilterValueEditorComposite<?, ?> value : values)
			{
				
				row.add(new Label("\t -> " + value.getValue()));
				
			}
		}
		
		return row;
		
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
	 *         Classname = entryRowComboBox
	 */
	private HorizontalLayout createEntryRowCombo(final FilterEntryEditor editor)
	{
		final HorizontalLayout layout = new HorizontalLayout(editor);
		layout.setEnabled(true);
		layout.addClassName(StringResourceUtils.getResourceString("entryRowComboBox", this));
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
		layout.addClassName(StringResourceUtils.getResourceString("buttonLayout", this));
		layout.add(editButton, checkbox, deleteButton);
		return layout;
	}
	
	/**
	 * Creates the finished ButtonLayout for the <b> ComboBox Div </b>.
	 * Also it adds an ClassName.
	 *
	 * @param checkbox
	 *            -> {@link Component}
	 * @param deleteButton
	 *            -> {@link Component}
	 * @return The Button Layout as {@link FormLayout}
	 *         <br>
	 *         Classname = buttonLayout
	 */
	private HorizontalLayout createButtonLayoutCombo(final Component checkbox, final Component deleteButton)
	{
		final HorizontalLayout layout = new HorizontalLayout();
		layout.addClassName(StringResourceUtils.getResourceString("buttonLayout", this));
		layout.add(checkbox, deleteButton);
		return layout;
	}
	
	/*****************************************************************************************************************************************
	 */
	/**************************************
	 ************ Defining Stuff***********
	 **************************************/
	
	/**
	 * Defines the ComboDiv with ClassName, etc.
	 *
	 * @param div
	 *            -> {@link Div}
	 */
	private void defineComboBoxDiv(final Div div)
	{
		div.setVisible(true);
		div.setWidthFull();
		div.addClassName(StringResourceUtils.getResourceString("comboBoxDiv", this));
	}
	
	/**
	 * Defines the FilterDiv with ClassName, etc.
	 *
	 * @param div
	 *            -> {@link Div}
	 */
	private void defineFilterDiv(final Div div)
	{
		div.setClassName(StringResourceUtils.getResourceString("filterDiv", this));
		div.setVisible(false);
		div.setEnabled(true);
		div.setSizeFull();
	}
	
	/**
	 * Defines the Buttons inside the Label div. Those are needed to check the Filter, Edit or remove it.
	 *
	 * @param checkbox
	 *            -> {@link FilterCheckBox}
	 * @param editButton
	 *            -> {@link EditButton}
	 * @param deleteButton
	 *            -> {@link DeleteButton}
	 * @param editor
	 *            -> {@link ReplaceabelEditor}
	 */
	private void definingButtons(
		final FilterCheckBox checkbox,
		final EditButton editButton,
		final DeleteButton deleteButton,
		final ReplaceabelEditor editor)
	{
		checkbox.defineCheckBox();
		checkboxValueChangeListener(checkbox, editor);
		
		editButton.defineButton();
		editButtonClickListener(editButton, editor);
		
		deleteButton.defineButton();
		deleteButtonClickListener(deleteButton, editor);
	}
	
	/**
	 * Defines the Buttons which are needed to edit a filter
	 *
	 * @param updateButton
	 *            -> {@link UpdateButton}
	 * @param cancelButton
	 *            -> {@link CancelButton}
	 * @param editor
	 *            -> {@link ReplaceabelEditor}
	 */
	private void definingUpdateButtons(
		final UpdateButton updateButton,
		final CancelButton cancelButton,
		final ReplaceabelEditor editor)
	{
		updateButton.defineButton();
		updateButtonClickListener(updateButton, editor);
		cancelButton.defineButton();
		cancelButtonUpdateClickListener(cancelButton, editor);
	}
	
	/*****************************************************************************************************************************************
	 */
	/**************************************
	 ************ Listener***********
	 **************************************/
	
	/**
	 * Defines the ClickEvent which is activated by clicking on the 'Hide-Button'.
	 * This will hide or show the FilterDiv
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
		checkboxValueChangeListener(final Checkbox checkbox, final ReplaceabelEditor editor)
	{
		
		checkbox.addValueChangeListener(listener -> {
			final Boolean check = checkbox.getValue();
			if(Boolean.TRUE.equals(check))
			{
				activateFilterEntryEditor(editor);
			}
			else if(Boolean.FALSE.equals(check))
			{
				deactivateFilterEntryEditor(editor);
			}
		});
		
	}
	
	/**
	 * Add a click listener to a {@link Button}.
	 * This will remove the selected Filter from the filterList and the labelDiv
	 *
	 * @param button
	 *            -> {@link Button}
	 * @param editor
	 *            -> {@link ReplaceabelEditor}
	 */
	private void deleteButtonClickListener(final Button button, final ReplaceabelEditor editor)
	{
		button.addClickListener(listener -> {
			removeFilterEntryEditor(editor);
		});
	}
	
	/**
	 * Add a clickListener to a {@link Button}. This will add a new Label to the labelDiv with the selected data.
	 *
	 * @param button
	 *            -> {@link Button}
	 * @param index
	 *            -> {@link Integer} current row Index
	 * @param editor
	 *            -> {@link ReplaceabelEditor}
	 * @param checkbox
	 *            -> {@link FilterCheckBox}
	 * @param editButton
	 *            -> {@link EditButton}
	 * @param deleteButton
	 *            -> {@link DeleteButton}
	 */
	private void addButtonClickListener(
		final Button button,
		final int index,
		final ReplaceabelEditor editor,
		final FilterCheckBox checkbox,
		final EditButton editButton,
		final DeleteButton deleteButton)
	{
		
		removeClickListener(this.addButtonClick);
		this.addButtonClick = button.addClickListener(listener -> {
			final FilterEntryEditor original = editor.getOriginal();
			if(original.getSelectedProperty() != null &&
				original.getSelectedOperator() != null &&
				original.getValueEditors() != null)
			{
				addingNewLabelRow(editor, checkbox, editButton, deleteButton);
			}
			else
			{
				Notification.show(StringResourceUtils.getResourceString("addWarning", this));
			}
			
		});
	}
	
	/**
	 * Creates the ClickListener for a given {@link Button}. This method just removes the current Filter inside the
	 * comboDiv
	 * and add a new one.
	 * This method is only used if the current Filter is a new one and not added to the filterList yet.
	 *
	 * @param button
	 *            -> {@link Button}
	 * @param index
	 *            -> {@link Integer} current row Index
	 * @param editor
	 *            -> {@link ReplaceabelEditor}
	 */
	private void cancelButtonClickListener(final Button button, final int index, final ReplaceabelEditor editor)
	{
		button.addClickListener(listener -> {
			this.comboDiv.removeAll();
			addFilterEntryEditor(new FilterEntryEditor(this, this, this::updateFilterData), index);
		});
	}
	
	/**
	 * Add a clickListener to the given {@link Button}. It will remove the current filter inside the comboDiv
	 * and creates a new Layout with the selected data inside the selected label.
	 *
	 * @param button
	 *            -> {@link Button}
	 * @param editor
	 *            -> {@link ReplaceabelEditor}
	 */
	private void editButtonClickListener(final Button button, final ReplaceabelEditor editor)
	{
		button.addClickListener(listener -> {
			this.comboDiv.removeAll();
			updateComboBox(editor, new UpdateButton(), new CancelButton());
		});
	}
	
	/**
	 * Add a clicklistener to the given {@link Button}
	 * This Listener creates a new {@link HorizontalLayout}, swap the old one inside the labelDiv with the new one
	 * and create a new FilterEntry to let the User select another Filter.
	 *
	 * @param updateButton
	 *            -> {@link Button}
	 * @param editor
	 *            -> {@link ReplaceabelEditor}
	 */
	private void updateButtonClickListener(final Button updateButton, final ReplaceabelEditor editor)
	{
		updateButton.addClickListener(listener -> {
			updateLabelRow(editor, new FilterCheckBox(), new EditButton(), new DeleteButton());
			newFilterEntry(this.rowIndex);
			updateFilterData();
		});
	}
	
	/**
	 * Creates the ClickListener for the {@link Button} inside the ComboDiv
	 * <br>
	 * This method is explicit needed if the data inside the Div is used to update something.
	 * <br>
	 * It just reset the Data within the orignial editor and creates a new FilterEntry,
	 * so the user can add a new one.
	 *
	 * @param cancelButton
	 *            -> {@link Button}
	 * @param editor
	 *            -> {@link ReplaceabelEditor}
	 */
	private void cancelButtonUpdateClickListener(final Button cancelButton, final ReplaceabelEditor editor)
	{
		cancelButton.addClickListener(listener -> {
			updateReplaceableOriginal(editor, editor.getCopy());
			newFilterEntry(this.rowIndex);
			updateFilterData();
		});
	}
	
	/*****************************************************************************************************************************************
	 */
	/**************************************
	 ************ Updating Stuff***********
	 **************************************/
	
	/**
	 * Updates the Data inside the grid, by checking the Filter inside the Searchbar and the filterList
	 */
	protected void updateFilterData()
	{
		final String        searchTerm = this.searchTextField.getValue();
		final FilterEntry[] entries    = this.filterEntryEditors.stream()
			.map(editor -> editor.getOriginal().getFilterEntry()).filter(Objects::nonNull)
			.toArray(FilterEntry[]::new);
		setModelValue(new FilterData(searchTerm, entries), false);
	}
	
	public void reset()
	{
		setValue(new FilterData());
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
	protected void activateFilterEntryEditor(final ReplaceabelEditor editor)
	{
		this.filterEntryEditors.add(editor);
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
	protected void deactivateFilterEntryEditor(final ReplaceabelEditor editor)
	{
		this.filterEntryEditors.remove(editor);
		
		updateFilterData();
	}
	
	/**
	 * Removes the current {@link FilterEntryEditor} inside the ComboDiv and add a new one instead.
	 * With this method a complete new Filter can be selected
	 *
	 * @param index
	 *            -> {@link Integer} (current rowIndex)
	 */
	private void newFilterEntry(final int index)
	{
		this.comboDiv.removeAll();
		addFilterEntryEditor(new FilterEntryEditor(this, this, this::updateFilterData), index);
	}
	
	/**
	 * Updates the {@link FilterEntryEditor} {@link Div} with the given Data of the Label.
	 * This method is needed to edit an excisting {@link Label}.
	 *
	 * @param editor
	 *            -> {@link ReplaceabelEditor}
	 * @param updateButton
	 *            -> {@link UpdateButton}
	 * @param cancelButton
	 *            -> {@link CancelButton}
	 */
	private void updateComboBox(
		final ReplaceabelEditor editor,
		final UpdateButton updateButton,
		final CancelButton cancelButton)
	{
		definingUpdateButtons(updateButton, cancelButton, editor);
		
		editor.getOriginal().setVisible(true);
		
		this.comboDiv.add(
			createFinalLayout(
				createEntryRowCombo(editor.getOriginal()),
				createButtonLayoutCombo(updateButton, cancelButton)));
	}
	
	/**
	 * Updates the copy of the {@link FilterEntryEditor} to be a new deep copy of the original.
	 *
	 * @param editor
	 *            -> {@link ReplaceabelEditor}
	 */
	private void updateReplaceabelCopy(final ReplaceabelEditor editor)
	{
		editor.updateCopy();
	}
	
	/**
	 * Replaces the original {@link FilterEntryEditor} with the given editor.
	 * Thas can be the old one{@link ReplaceabelEdiotr #getCopy}, to reset the data, but also a new one to update the
	 * filter entry.
	 * Also the methods updates the copy, so both Filter (old and new) are the same.
	 *
	 * @param replace
	 *            -> {@link ReplaceabelEditor}
	 * @param editor
	 *            -> {@link FilterEntryEditor}
	 */
	private void
		updateReplaceableOriginal(final ReplaceabelEditor replace, final FilterEntryEditor editor)
	{
		replace.setOriginal(editor);
		replace.updateCopy();
	}
	
	/**
	 * Creates a new {@link HorizontalLayout} which will then replaces the old HorizontalLayout inside the
	 * label{@link Div}.
	 *
	 * @param editor
	 * @param checkbox
	 * @param editButton
	 * @param deleteButton
	 */
	private void updateLabelRow(
		final ReplaceabelEditor editor,
		final FilterCheckBox checkbox,
		final EditButton editButton,
		final DeleteButton deleteButton)
	{
		definingButtons(checkbox, editButton, deleteButton, editor);
		
		final HorizontalLayout finalLayout = createFinalLayout(createEntryRowLabel(editor.getOriginal()),
			createButtonLayoutLabel(checkbox, editButton, deleteButton));
		
		replaceLabelRow(editor.getLabelLayout(), finalLayout);
		
		editor.setLabelLayout(finalLayout);
		
		updateReplaceabelCopy(editor);
		updateFilterData();
		newFilterEntry(this.rowIndex);
	}
	
	/**
	 * Copied from {@link HasOrderedComponents #replace(Component, Component)}
	 *
	 * @param oldComponent
	 * @param newComponent
	 */
	private void replaceLabelRow(final Component oldComponent, final Component newComponent)
	{
		if(oldComponent == null && newComponent == null)
		{
			// NO-OP
			return;
		}
		if(oldComponent == null)
		{
			this.labelDiv.add(newComponent);
		}
		else if(newComponent == null)
		{
			this.labelDiv.remove(oldComponent);
		}
		else
		{
			final Element element  = this.labelDiv.getElement();
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
				this.labelDiv.add(newComponent);
			}
		}
	}
	
	/*****************************************************************************************************************************************
	 */
	/**************************************
	 ************ Removing Stuff***********
	 **************************************/
	
	/**
	 * Removes the selected Filter from the <b>filterEntryEditors List</b> and <b> labelDiv View </b>
	 *
	 * @param editor
	 *            -> {@link FilterEntryEditor}
	 * @param index
	 *            -> {@link Integer}
	 */
	protected void removeFilterEntryEditor(final ReplaceabelEditor editor)
	{
		removeDataFromDiv(editor);
		
		deactivateFilterEntryEditor(editor);
		
		updateFilterData();
	}
	
	private void removeDataFromDiv(final ReplaceabelEditor editor)
	{
		
		this.labelDiv.remove(editor.getLabelLayout());
	}
	
	private void removeClickListener(final Registration listener)
	{
		listener.remove();
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
	
	/*
	 * Main Method to initialize the Filter Components
	 *
	 * @param index
	 * -> {@link Integer}
	 *
	 * @return The EntryEditor as {@link FilterEntryEditor}
	 */
	protected FilterEntryEditor addFilterEntryEditor(final FilterEntryEditor editor, final int index)
	{
		final ReplaceabelEditor replace      = new ReplaceabelEditor(editor);
		final CancelButton      cancelButton = new CancelButton();
		cancelButton.defineButton();
		
		addButtonClickListener(this.addFilterButton, index, replace, new FilterCheckBox(),
			new EditButton(),
			new DeleteButton());
		cancelButtonClickListener(cancelButton, index, replace);
		this.comboDiv.add(
			createFinalLayout(
				createEntryRowCombo(editor),
				createButtonLayoutCombo(this.addFilterButton, cancelButton)));
		this.rowIndex++;
		
		openDiv();
		return editor;
		
	}
	
	/**************************************
	 * More Important***********
	 **************************************/
	
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
					addFilterEntryEditor(new FilterEntryEditor(this, this, this::updateFilterData),
						this.filterEntryEditors.size())
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
	 * Add a new Row to the label Div.
	 * Defines the given Components and create the the row with {@link #createFinalLayout(Component, Component)}.
	 *
	 * @param editor
	 *            -> {@link ReplaceabelEditor}
	 * @param checkbox
	 *            -> {@link FilterCheckBox}
	 * @param editButton
	 *            -> {@link EditButton}
	 * @param deleteButton
	 *            -> {@link DeleteButton}
	 */
	private void addingNewLabelRow(
		final ReplaceabelEditor editor,
		final FilterCheckBox checkbox,
		final EditButton editButton,
		final DeleteButton deleteButton)
	{
		
		definingButtons(checkbox, editButton, deleteButton, editor);
		
		this.filterEntryEditors.add(editor);
		
		final HorizontalLayout finalLayout = createFinalLayout(createEntryRowLabel(editor.getOriginal()),
			createButtonLayoutLabel(checkbox, editButton, deleteButton));
		
		editor.setLabelLayout(finalLayout);
		
		this.labelDiv.add(finalLayout);
		this.labelDiv.setVisible(true);
		
		updateReplaceabelCopy(editor);
		updateFilterData();
		newFilterEntry(this.rowIndex);
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
