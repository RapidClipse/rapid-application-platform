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
	private AddButton                     addFilterButton;
	private HideButton                    hideFilterButton;
	private Div                           filterDiv;
	private Div                           comboDiv;
	private Div                           labelDiv           = new Div();
	private final List<ReplaceabelEditor> filterEntryEditors = new ArrayList<>();
	private int                           rowIndex           = 0;
	protected List<String>                filterProperty     = new ArrayList<>();
	protected List<String>                filterOperator     = new ArrayList<>();
	private HorizontalLayout              searchBar;
	private Registration                  addButtonClick;
	private Registration                  cancelButtonClick;
	private final List<HorizontalLayout>  labelLayouts       = new ArrayList<>();

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
	 * @param finalButtonLayout
	 *            -> The right part of the Layout with the needed Buttons as {@link Component}
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
		final Div div = new Div();
		defineComboBoxDiv(div);
		return div;
	}

	private Div createFilterDiv()
	{
		final Div div = new Div();
		div.add(this.labelDiv, this.comboDiv);
		defineFilterDiv(div);
		return div;
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
	private HorizontalLayout createButtonLayoutCombo(final Component checkbox, final Component deleteButton)
	{
		final HorizontalLayout layout = new HorizontalLayout();
		layout.addClassName("buttonLayout");
		layout.add(checkbox, deleteButton);
		return layout;
	}

	/*****************************************************************************************************************************************
	 */
	/**************************************
	 ************ Defining Stuff***********
	 **************************************/

	private void defineComboBoxDiv(final Div div)
	{
		div.setVisible(true);
		div.setWidthFull();
		div.addClassName("comboBoxDiv");
	}

	private void defineFilterDiv(final Div div)
	{
		div.setClassName("filterDiv");
		div.setVisible(false);
		div.setEnabled(true);
		div.setSizeFull();
	}

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
	 * Deletes the selected Row from the given {@link Div}
	 *
	 * @param button
	 *            -> {@link Button}
	 * @param div
	 *            -> {@link Div}
	 * @param index
	 *            -> {@link Integer}
	 */
	private void deleteButtonClickListener(final Button button, final ReplaceabelEditor editor)
	{
		button.addClickListener(listener -> {
			removeFilterEntryEditor(editor);
		});
	}

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
			addingNewLabelRow(index, editor, checkbox, editButton, deleteButton);

		});
	}

	/**
	 * Creates the ClickListener for a cancelButton
	 *
	 * @param button
	 * @param index
	 * @param editor
	 */
	private void cancelButtonClickListener(final Button button, final int index, final ReplaceabelEditor editor)
	{
		this.cancelButtonClick = button.addClickListener(listener -> {
			deactivateFilterEntryEditor(editor);
			this.comboDiv.removeAll();
			// I yet don't know why I have to do this
			this.rowIndex--;
			addFilterEntryEditor(new FilterEntryEditor(this, this, this::updateFilterData), index);
		});
	}

	private void editButtonClickListener(final EditButton button, final ReplaceabelEditor editor)
	{
		button.addClickListener(listener -> {
			this.comboDiv.removeAll();
			updateFilterEntryEditor(editor, new UpdateButton(), new CancelButton());
		});
	}

	private void updateButtonClickListener(final UpdateButton updateButton, final ReplaceabelEditor editor)
	{
		updateButton.addClickListener(listener -> {
			// updateFilterDivLabel(editor.getOriginal().getRowIndex(), editor.getOriginal());
			updateComboBox(this.rowIndex);
			updateFilterData();
		});
	}

	private void cancelButtonUpdateClickListener(final CancelButton cancelButton, final ReplaceabelEditor editor)
	{
		cancelButton.addClickListener(listener -> {

			removeFilterEntryEditor(editor);
			/*
			 * TODO: Nach remove an selber stelle neue Row hinzufügen
			 *
			 * Not Adding like ->
			 * addingNewLabelRow(editor.getOriginal().getRowIndex(), editor, new FilterCheckBox(), new EditButton(),
			 * new DeleteButton());
			 */
			updateFilterView(editor);

		});
	}

	/*****************************************************************************************************************************************
	 */
	/**************************************
	 ************ Updating Stuff***********
	 **************************************/

	private void updateFilterView(final ReplaceabelEditor editor)
	{
		updateReplaceableOriginal(editor, editor.getCopy());
		updateComboBox(this.rowIndex);
		updateFilterData();
	}

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

	private void updateComboBox(final int index)
	{
		this.comboDiv.removeAll();
		addFilterEntryEditor(new FilterEntryEditor(this, this, this::updateFilterData), index);
	}

	private void updateFilterEntryEditor(
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

	private void updateReplaceabelCopy(final ReplaceabelEditor editor)
	{
		editor.updateCopy();
	}

	private void
		updateReplaceableOriginal(final ReplaceabelEditor replace, final FilterEntryEditor editor)
	{
		replace.setOriginal(editor);
		replace.updateCopy();
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
	 * Main Method to initialize the Filter Components
	 *
	 * @param index
	 *            -> {@link Integer}
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

	private void addingNewLabelRow(
		final int index,
		final ReplaceabelEditor editor,
		final FilterCheckBox checkbox,
		final EditButton editButton,
		final DeleteButton deleteButton)
	{

		definingButtons(checkbox, editButton, deleteButton, editor);

		this.filterEntryEditors.add(editor);

		final HorizontalLayout finalLayout = createFinalLayout(createEntryRowLabel(index, editor.getOriginal()),
			createButtonLayoutLabel(checkbox, editButton, deleteButton));

		editor.setLabelLayout(finalLayout);

		this.labelDiv.add(finalLayout);
		this.labelDiv.setVisible(true);

		updateReplaceabelCopy(editor);
		updateFilterData();
		updateComboBox(this.rowIndex);
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
