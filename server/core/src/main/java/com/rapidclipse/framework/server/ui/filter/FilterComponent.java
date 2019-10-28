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
import com.vaadin.flow.component.dependency.JavaScript;
import com.vaadin.flow.component.dependency.StyleSheet;
import com.vaadin.flow.component.dialog.Dialog;
import com.vaadin.flow.component.formlayout.FormLayout;
import com.vaadin.flow.component.grid.Grid;
import com.vaadin.flow.component.html.Div;
import com.vaadin.flow.component.html.Label;
import com.vaadin.flow.component.html.NativeButton;
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
	private final Div                     filterDiv          = new Div();
	private final List<FilterEntryEditor> filterEntryEditors = new ArrayList<>();
	private int                           index              = 0;

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

		this.hideFilterButton = createHideFilterButton();
		this.hideFilterButton.addClassName("hideFilterButton");
		this.hideFilterButton.addClickListener(listener -> hideButtonClickListener());

		this.addFilterButton = createAddFilterButton();
		this.addFilterButton.addClickListener(event -> addFilterEntryEditor(this.index));
		this.addFilterButton.setEnabled(false);

		this.filterDiv.setVisible(true);
		this.filterDiv.setWidthFull();

		final HorizontalLayout searchBar = new HorizontalLayout(this.searchTextField, this.hideFilterButton,
			this.addFilterButton);
		searchBar.setMargin(false);
		searchBar.setPadding(false);
		searchBar.expand(this.searchTextField);
		searchBar.setWidth("100%");

		final VerticalLayout content = new VerticalLayout(searchBar, this.filterDiv);
		content.setMargin(false);
		content.setPadding(false);
		content.setSpacing(true);
		return content;
	}

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

	protected void openDiv()
	{
		this.hideFilterButton.setIcon(VaadinIcon.ANGLE_DOUBLE_DOWN.create());
		this.hideFilterButton.setOpen(true);
		this.filterDiv.setVisible(true);
	}

	protected void closeDiv()
	{
		this.filterDiv.setVisible(false);
		this.hideFilterButton.setIcon(VaadinIcon.ANGLE_DOUBLE_LEFT.create());
		this.hideFilterButton.setOpen(false);
	}

	private HideButton createHideFilterButton()
	{
		final HideButton button = new HideButton();
		button.setIcon(VaadinIcon.ANGLE_DOUBLE_LEFT.create());
		button.setOpen(false);
		return button;
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
		editor.addClassName("editor");
		final Checkbox checkbox = new Checkbox();
		checkbox.addClassName("checkbox");
		final Button deleteButton = new Button();
		deleteButton.addClassName("deleteButton");
		deleteButton.setIcon(VaadinIcon.MINUS.create());
		UI.getCurrent().getPage().retrieveExtendedClientDetails(detail -> {
			if(detail.getBodyClientWidth() <= 800)
			{
				final Dialog dialog = new Dialog(new Label("Test"));
				dialog.setCloseOnEsc(false);
				dialog.setCloseOnOutsideClick(true);

				this.filterEntryEditors.add(index, editor);

				final HorizontalLayout filterEntryRow = new HorizontalLayout(editor);

				final NativeButton confirmButton = new NativeButton("Übernehmen", event -> {
					// +1 because of search bar at top
					this.filterDiv.addComponentAtIndex(index,
						createFinalRow(deleteButton, checkbox, filterEntryRow, editor, index));
					dialog.close();
					if(this.filterDiv.isVisible())
					{
						closeDiv();
					}
				});

				confirmButton.addClassName("confirmButton");
				dialog.add(filterEntryRow, confirmButton);
				dialog.open();
			}
			else if(detail.getBodyClientWidth() > 800)
			{

				openDiv();
				this.filterEntryEditors.add(index, editor);

				final Button removeFilterButton = createRemoveFilterButton();

				final HorizontalLayout filterEntryRow = new HorizontalLayout(editor);
				
				// +1 because of search bar at top
				this.filterDiv.addComponentAtIndex(index,
					createFinalRow(removeFilterButton, checkbox, filterEntryRow, editor, index));

			}

		});
		return editor;

	}

	/**
	 * @param removeFilterButton
	 * @param checkbox
	 * @param filterEntryRow
	 * @param editor
	 * @param index
	 * @return
	 */
	private Component createFinalRow(
		final Button deleteButton,
		final Checkbox checkbox,
		final HorizontalLayout filterEntryRow,
		final FilterEntryEditor editor,
		final int index)
	{
		defineCheckBox(checkbox);
		checkbox
			.addValueChangeListener(listener -> checkboxValueChanceForEntryEditor(checkbox, index, editor));

		defineDeleteButton(deleteButton);
		deleteButton.addClickListener(listener -> deleteButtonClickListener(editor));

		final FormLayout finalButtonLayout = new FormLayout();
		finalButtonLayout.addClassName("finalButtonLayout");
		finalButtonLayout.add(checkbox, deleteButton);

		filterEntryRow.setEnabled(true);
		filterEntryRow.addClassName("filterEntryRow");

		final HorizontalLayout finalLayout = new HorizontalLayout();
		finalLayout.add(filterEntryRow, finalButtonLayout);
		finalLayout.addClassName("finalLayout");
		this.index++;
		return finalLayout;
	}

	protected void defineCheckBox(final Checkbox checkbox)
	{
		checkbox.setValue(true);
	}

	protected void defineDeleteButton(final Button deleteButton)
	{
		deleteButton.setIcon(VaadinIcon.MINUS.create());

	}

	protected void deleteButtonClickListener(final FilterEntryEditor editor)
	{
		final Dialog       dialog       = new Dialog(new Label("Wollen Sie diese Zeile wirklich lösen?"));
		final NativeButton deleteButton = new NativeButton("Ja", event -> {
											removeFilterEntryEditor(editor);
											this.index--;
											dialog.close();
										});
		final NativeButton cancelButton = new NativeButton("Nein", event -> dialog.close());
		dialog.add(deleteButton, cancelButton);
		dialog.open();
	}

	protected void
		checkboxValueChanceForEntryEditor(final Checkbox checkbox, final int index, final FilterEntryEditor editor)
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

	protected void activateFilterEntryEditor(final int index, final FilterEntryEditor editor)
	{
		this.filterEntryEditors.add(index, editor);
		updateFilterData();
	}

	protected void deactivateFilterEntryEditor(final FilterEntryEditor editor)
	{
		this.filterEntryEditors.remove(editor);
		updateFilterData();
	}

	protected void removeFilterEntryEditor(final FilterEntryEditor editor)
	{
		removeFilterEntryEditorComponent(editor);

		this.filterEntryEditors.remove(editor);

		updateFilterData();
	}

	protected void removeFilterEntryEditorComponent(final FilterEntryEditor editor)
	{

		final Component filterEntryRow = editor.getParent().get();
		final Component finalRow       = filterEntryRow.getParent().get();
		this.filterDiv.remove(finalRow);

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
