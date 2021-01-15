/*
 * Copyright (C) 2013-2021 by XDEV Software, All Rights Reserved.
 *
 * This file is part of the RapidClipse Application Platform (RAP).
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
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
import com.rapidclipse.framework.server.ui.UIUtils;
import com.rapidclipse.framework.server.util.ServiceLoader;
import com.vaadin.flow.component.AbstractCompositeField;
import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.HasSize;
import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.grid.Grid;
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
	private final List<FilterEntryEditor> filterEntryEditors = new ArrayList<>();

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

		this.addFilterButton = createAddFilterButton();
		this.addFilterButton.addClickListener(event -> addFilterEntryEditor(0));
		this.addFilterButton.setEnabled(false);

		final HorizontalLayout searchBar = new HorizontalLayout(this.searchTextField,
			this.addFilterButton);
		searchBar.setMargin(false);
		searchBar.setPadding(false);
		searchBar.expand(this.searchTextField);
		searchBar.setWidth("100%");

		final VerticalLayout content = new VerticalLayout(searchBar);
		content.setMargin(false);
		content.setPadding(false);
		content.setSpacing(true);
		return content;
	}

	protected TextField createSearchTextField()
	{
		final TextField textField = new TextField();
		textField.setValueChangeMode(ValueChangeMode.EAGER);
		return textField;
	}

	protected Button createAddFilterButton()
	{
		final Button button = new Button();
		button.setIcon(VaadinIcon.PLUS.create());
		return button;
	}

	protected Button createRemoveFilterButton()
	{
		final Button button = new Button();
		button.setIcon(VaadinIcon.MINUS.create());
		return button;
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

	@Override
	public FilterSubject getFilterSubject()
	{
		return this.filterSubject;
	}

	public Registration connectWith(final Grid<?> grid)
	{
		final Registration registration = connectWith(grid.getDataProvider());

		// set subject after successful registration
		setFilterSubject(GridFilterSubjectFactory.CreateFilterSubject(grid));

		return registration;
	}

	@SuppressWarnings({"unchecked", "rawtypes"})
	public Registration connectWith(final DataProvider<?, ?> dataProvider)
	{
		if(dataProvider instanceof ConfigurableFilterDataProvider<?, ?, ?>)
		{
			final ConfigurableFilterDataProvider configurableFilterDataProvider =
				(ConfigurableFilterDataProvider)dataProvider;
			final DataProviderFilterAdapter      filterAdapter                  =
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

	protected FilterEntryEditor addFilterEntryEditor(final int index)
	{
		final FilterEntryEditor editor = new FilterEntryEditor(this, this::updateFilterData);
		editor.setWidth("100%");
		this.filterEntryEditors.add(index, editor);

		final Button addFilterButton = createAddFilterButton();
		addFilterButton.addClickListener(event -> addFilterEntryEditor(index + 1));

		final Button removeFilterButton = createRemoveFilterButton();
		removeFilterButton.addClickListener(event -> removeFilterEntryEditor(editor));

		final HorizontalLayout filterEntryRow = new HorizontalLayout(editor, removeFilterButton,
			addFilterButton);
		filterEntryRow.setPadding(false);
		filterEntryRow.setMargin(false);
		filterEntryRow.expand(editor);
		filterEntryRow.setWidth("100%");

		// +1 because of search bar at top
		getContent().addComponentAtIndex(index + 1, filterEntryRow);

		return editor;
	}

	protected void removeFilterEntryEditor(final FilterEntryEditor editor)
	{
		removeFilterEntryEditorComponent(editor);

		this.filterEntryEditors.remove(editor);

		updateFilterData();
	}

	protected void removeFilterEntryEditorComponent(final FilterEntryEditor editor)
	{
		final VerticalLayout content           = getContent();
		final Component      componentToRemove = UIUtils.getNextParent(editor,
			component -> component.getParent().get() == content);
		content.remove(componentToRemove);
	}

	public String getSearchText()
	{
		return this.searchTextField != null ? this.searchTextField.getValue() : "";
	}

	public void setSearchText(final String searchText)
	{
		this.searchTextField.setValue(searchText != null ? searchText : "");

		updateFilterData();
	}

	protected void updateFilterData()
	{
		final String        searchTerm = this.searchTextField.getValue();
		final FilterEntry[] entries    = this.filterEntryEditors.stream()
			.map(FilterEntryEditor::getFilterEntry).filter(Objects::nonNull)
			.toArray(FilterEntry[]::new);
		setModelValue(new FilterData(searchTerm, entries), false);
	}

	@Override
	protected void setPresentationValue(final FilterData filterData)
	{
		this.filterEntryEditors.forEach(this::removeFilterEntryEditorComponent);
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

	public void reset()
	{
		setValue(new FilterData());
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

	protected Filter createSearchFilter()
	{
		if(this.searchFilterGenerator != null)
		{
			return this.searchFilterGenerator.createSearchFilter(getSearchText(), this);
		}

		return null;
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
}
