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
import java.util.stream.Collectors;

import com.rapidclipse.framework.server.data.filter.Composite;
import com.rapidclipse.framework.server.data.filter.Composite.Connector;
import com.rapidclipse.framework.server.data.filter.Filter;
import com.rapidclipse.framework.server.data.provider.DataProviderFilterAdapter;
import com.rapidclipse.framework.server.resources.StringResourceUtils;
import com.rapidclipse.framework.server.ui.filter.helper.AddButton;
import com.rapidclipse.framework.server.ui.filter.helper.CancelButton;
import com.rapidclipse.framework.server.ui.filter.helper.ComboDiv;
import com.rapidclipse.framework.server.ui.filter.helper.FilterDiv;
import com.rapidclipse.framework.server.ui.filter.helper.HideButton;
import com.rapidclipse.framework.server.ui.filter.helper.LabelDiv;
import com.rapidclipse.framework.server.ui.filter.helper.ReplaceabelEditor;
import com.rapidclipse.framework.server.ui.filter.helper.Searchbar;
import com.rapidclipse.framework.server.util.ServiceLoader;
import com.vaadin.flow.component.AbstractCompositeField;
import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.HasSize;
import com.vaadin.flow.component.dependency.StyleSheet;
import com.vaadin.flow.component.grid.Grid;
import com.vaadin.flow.component.html.Div;
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
public class FilterComponent
	extends AbstractCompositeField<VerticalLayout, FilterComponent, FilterData>
	implements FilterContext, HasSize
{
	private final boolean caseSensitive = false;
	private final char    wildcard      = '*';
	
	private final Connector searchPropertiesConnector = Connector.OR;
	private final Connector searchMultiWordConnector  = Connector.OR;
	private final Connector filterPropertiesConnector = Connector.AND;
	private final Connector searchAndFilterConnector  = Connector.AND;
	
	private final SearchFilterGenerator searchFilterGenerator = SearchFilterGenerator
		.New();
	
	private final FilterOperatorRegistry            filterOperatorRegistry            = FilterOperatorRegistry
		.Default();
	private final SubsetDataProviderFactoryRegistry subsetDataProviderFactoryRegistry =
		SubsetDataProviderFactoryRegistry
			.Default();
	
	private FilterSubject filterSubject;
	
	private TextField                     searchTextField;
	public final AddButton                addFilterButton    = new AddButton();
	private final HideButton              hideFilterButton   = new HideButton();
	public final FilterDiv                filterDiv          = new FilterDiv();
	public final ComboDiv                 comboDiv           = new ComboDiv();
	private final LabelDiv                labelDiv           = new LabelDiv();
	private final List<ReplaceabelEditor> filterEntryEditors = new ArrayList<>();
	private final Searchbar               searchBar          = new Searchbar();
	private Registration                  hideButtonClick;
	
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
		
		this.hideFilterButton.defineButton();
		this.hideFilterButton.setClickListener(this, null);
		
		this.hideButtonClick = this.hideFilterButton.addClickListener(
			listener -> {
				// I don't know how else you can remove the Listener to not constantly create a new Editor
				// furthermore you have to add the filter AFTER the content was returned
				// Else -> Nullpointer
				// Reason -> The Method calls theirself as other value. If it's not finished yet, there is null
				this.hideButtonClick.remove();
				addFilterEntryEditor(new FilterEntryEditor(this, this, this::updateFilterData));
			});
		
		this.comboDiv.defineDiv();
		this.labelDiv.defineDiv();
		
		this.addFilterButton.defineButton();
		this.addFilterButton.setEnabled(false);
		
		this.filterDiv.defineDiv();
		this.filterDiv.add(this.labelDiv, this.comboDiv);
		
		this.searchBar.defineSearchbar();
		this.searchBar.createSearchBar(this.searchTextField, this.hideFilterButton);
		
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
	 * Creates the Layout which is then seen from the User inside a Div
	 *
	 * @param filterEntryRow
	 *            -> The left part of the Layout with the needed Editor as {@link Component}
	 *            <br>
	 *            {@link #createEntryRowCombo(FilterEntryEditor)}
	 * @param finalButtonLayout
	 *            -> The right part of the Layout with the needed Buttons as {@link Component}
	 *            <br>
	 *            Can be created with {@link #createButtonLayout(Component, Component, Component)} or
	 *            {@link #createButtonLayout(Component, Component)}
	 * @return The final Layout as {@link HorizontalLayout}
	 *         <br>
	 *         Classname = finalLayout
	 */
	public HorizontalLayout createFinalLayout(
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
		
		final List<Filter> valueFilters = getFilterFromEntries();
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
	 * Creates the the Button layout for the given {@link Component}s
	 *
	 * @param components
	 *            -> As many {@link Component} as necessary
	 * @return -> {@link HorizontalLayout}
	 */
	public HorizontalLayout createButtonLayout(final Component... components)
	{
		final HorizontalLayout layout = new HorizontalLayout();
		layout.addClassName(StringResourceUtils.getResourceString("buttonLayout", this));
		for(final Component c : components)
		{
			layout.add(c);
		}
		return layout;
	}
	
	/*****************************************************************************************************************************************
	 */
	/**************************************
	 ************ Updating Stuff***********
	 **************************************/
	
	/**
	 * Updates the Data inside the grid, by checking the Filter inside the <b>Searchbar</b> and the <b>filterEntryEditor
	 * List</b>
	 */
	public void updateFilterData()
	{
		final String        searchTerm = this.searchTextField.getValue();
		final FilterEntry[] entries    = getEntriesFromEditor();
		setModelValue(new FilterData(searchTerm, entries), false);
	}

	public void resetValue()
	{
		setValue(new FilterData());
	}
	
	/**
	 * Add the Filter to the filter list.
	 * <br>
	 * This method just select the Filter but not make it visible
	 *
	 * @param editor
	 *            -> {@link ReplaceabelEditor}
	 */
	public void activateFilter(final ReplaceabelEditor editor)
	{
		this.filterEntryEditors.add(editor);
		updateFilterData();
	}
	
	/**
	 * Removes the Filter from the filter list.
	 * <br>
	 * This just deselect the Filter but allows the User to reactivate it.
	 * <br>
	 * The Filter will not be removed from the view
	 *
	 * @param editor
	 *            -> {@link ReplaceabelEditor}
	 */
	public void deactivateFilter(final ReplaceabelEditor editor)
	{
		this.filterEntryEditors.remove(editor);
		updateFilterData();
	}
	
	/**
	 * Removes the current {@link FilterEntryEditor} inside the ComboDiv and add a new one instead.
	 * With this method a complete new Filter can be selected
	 */
	public void newFilterEntry()
	{
		this.comboDiv.removeAll();
		addFilterEntryEditor(new FilterEntryEditor(this, this, this::updateFilterData));
	}

	public void updateEverything(final ReplaceabelEditor editor)
	{
		editor.updateCopy();
		updateFilterData();
		newFilterEntry();
	}
	
	/*****************************************************************************************************************************************
	 */
	/**************************************
	 ************ Removing Stuff***********
	 **************************************/
	
	/**
	 * Removes the selected Filter from the <b>filterEntryEditors List</b> and <b> labelDiv View </b>. <br>
	 * Than updates the FilterData inside the Grid.
	 *
	 * @param editor
	 *            -> {@link FilterEntryEditor}
	 */
	public void removeFilterEntryEditor(final ReplaceabelEditor editor)
	{
		this.labelDiv.removeData(editor);
		
		deactivateFilter(editor);
		
		updateFilterData();
	}

	/*****************************************************************************************************************************************
	 */
	/**************************************
	 ************ Getter / Adder / Setter**
	 **************************************/
	
	/**
	 * Main Method to initialize the Filterdata.<br>
	 * Creates a new instance of {@link ReplaceabelEditor} and add all needed parts to the <b> comoDiv</b>
	 *
	 * @param editor
	 *            -> {@link FilterEntryEditor}
	 * @return -> {@link FilterEntryEditor}
	 */
	public FilterEntryEditor addFilterEntryEditor(final FilterEntryEditor editor)
	{
		final ReplaceabelEditor replace      = new ReplaceabelEditor(editor);
		final CancelButton      cancelButton = new CancelButton();
		cancelButton.defineButton();
		
		this.addFilterButton.setClickListener(this, replace);
		
		cancelButton.setClickListener(this);
		
		final HorizontalLayout buttonLayout = createButtonLayout(this.addFilterButton, cancelButton);
		buttonLayout.setClassName("buttonLayoutCombo");
		
		this.comboDiv.add(
			createFinalLayout(
				this.comboDiv.createEntryRow(editor),
				buttonLayout));
		
		this.filterDiv.openDiv(this.hideFilterButton);
		
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
				addFilterEntryEditor(new FilterEntryEditor(this, this, this::updateFilterData));
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
			setTextFieldPlaceholderFromSubject(filterSubject);
			
		}
		else
		{
			this.searchTextField.setPlaceholder("");
		}
		
		resetValue();
	}
	
	private void setTextFieldPlaceholderFromSubject(final FilterSubject filterSubject)
	{
		final String res         = StringResourceUtils.getResourceString("searchTextFieldInputPrompt",
			this);
		final String properties  = getPropertiesFromSubject(filterSubject);
		final String placeholder = MessageFormat.format(res, properties);
		this.searchTextField.setPlaceholder(placeholder);
	}
	
	private String getPropertiesFromSubject(final FilterSubject filterSubject)
	{
		return filterSubject.searchableProperties().stream()
			.map(p -> p.caption()).collect(Collectors.joining(", "));
	}

	/*
	 * Less Important***********
	 **************************************/

	private FilterEntry[] getEntriesFromEditor()
	{
		return this.filterEntryEditors.stream()
			.map(editor -> editor.getOriginal().getFilterEntry()).filter(Objects::nonNull)
			.toArray(FilterEntry[]::new);
	}
	
	private List<Filter> getFilterFromEntries()
	{
		return this.filterEntryEditors.stream()
			.map(editor -> editor.getOriginal().getFilter()).filter(Objects::nonNull)
			.collect(Collectors.toList());
	}

	@Override
	public boolean isCaseSensitive()
	{
		return this.caseSensitive;
	}
	
	@Override
	public char getWildcard()
	{
		return this.wildcard;
	}
	
	@Override
	public Connector getSearchPropertiesConnector()
	{
		return this.searchPropertiesConnector;
	}
	
	@Override
	public Connector getSearchMultiWordConnector()
	{
		return this.searchMultiWordConnector;
	}
	
	@Override
	public Connector getFilterPropertiesConnector()
	{
		return this.filterPropertiesConnector;
	}
	
	@Override
	public Connector getSearchAndFilterConnector()
	{
		return this.searchAndFilterConnector;
	}
	
	@Override
	public FilterOperatorRegistry getFilterOperatorRegistry()
	{
		return this.filterOperatorRegistry;
	}
	
	@Override
	public SubsetDataProviderFactoryRegistry getSubsetDataProviderFactoryRegistry()
	{
		return this.subsetDataProviderFactoryRegistry;
	}
	
	@Override
	public FilterSubject getFilterSubject()
	{
		return this.filterSubject;
	}
	
	public String getSearchText()
	{
		return this.searchTextField != null ? this.searchTextField.getValue() : "";
	}
	
	/**
	 *
	 * @return the FilterEntryEditors -> {@link List} of {@link ReplaceabelEditor}
	 */
	public List<ReplaceabelEditor> getFilterEntryEditors()
	{
		return this.filterEntryEditors;
	}
	
	/**
	 * @return the labelDiv -> {@link LabelDiv}
	 */
	public LabelDiv getLabelDiv()
	{
		return this.labelDiv;
	}
	
}
