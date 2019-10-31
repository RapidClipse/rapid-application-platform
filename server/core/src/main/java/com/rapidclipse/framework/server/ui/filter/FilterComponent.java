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
import java.util.HashMap;
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
	private Div                           filterDivComboBox;
	private Div                           filterDivLabel     = new Div();
	private final List<FilterEntryEditor> filterEntryEditors = new ArrayList<>();
	private int                           index              = 0;
	private int                           width;
	
	protected String                    filterProperty = "";
	protected String                    filterOperator = "";
	protected final String              thirdBox       = "";
	protected final String              fourthBox      = "";
	private final int                   comboIndex     = 0;
	HashMap<Integer, FilterEntryEditor> editors        = new HashMap<>();
	private final List<Label>           labels         = new ArrayList<>();
	
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
			.retrieveExtendedClientDetails(detail -> this.width = detail.getWindowInnerWidth());
		UI.getCurrent().getPage().addBrowserWindowResizeListener(listener -> {
			this.width = listener.getWidth();
			if(this.width < 727)
			{
				swapToLabel();
			}
			else if(this.width >= 727)
			{
				swapToComboBox();
			}
		});
		this.searchTextField = createSearchTextField();
		this.searchTextField.addValueChangeListener(event -> updateFilterData());
		this.searchTextField.setEnabled(false);
		
		this.hideFilterButton = createHideFilterButton();
		this.hideFilterButton.addClickListener(listener -> hideButtonClickListener());
		
		this.filterDivComboBox = createFilterDivComboBox();
		this.filterDivLabel    = createFilterDivLabel();
		
		this.addFilterButton = createAddFilterButton();
		this.addFilterButton.addClickListener(event -> addFilterEntryEditor(this.index));
		this.addFilterButton.setEnabled(false);
		
		final HorizontalLayout searchBar = createSearchBar();
		
		final VerticalLayout content = createContent(searchBar);
		
		return content;
	}
	
	private VerticalLayout createContent(final HorizontalLayout searchBar)
	{
		final VerticalLayout content = new VerticalLayout(searchBar, this.filterDivComboBox);
		content.setMargin(false);
		content.setPadding(false);
		content.setSpacing(true);
		return content;
	}
	
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
	
	private Div createFilterDivLabel()
	{
		final Div filter = new Div();
		filter.setVisible(false);
		filter.setWidthFull();
		filter.addClassName("filterDivLabel");
		return filter;
	}
	
	private Div createFilterDivComboBox()
	{
		final Div filter = new Div();
		filter.setVisible(true);
		filter.setWidthFull();
		filter.addClassName("filterDivComboBox");
		return filter;
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
		if(this.width >= 727)
		{
			this.hideFilterButton.setIcon(VaadinIcon.ANGLE_DOUBLE_DOWN.create());
			this.hideFilterButton.setOpen(true);
			this.filterDivComboBox.setVisible(true);
		}
		else if(this.width < 727)
		{
			this.hideFilterButton.setIcon(VaadinIcon.ANGLE_DOUBLE_DOWN.create());
			this.hideFilterButton.setOpen(true);
			this.filterDivLabel.setVisible(true);
		}
		
	}
	
	protected void closeDiv()
	{
		if(this.width >= 727)
		{
			this.filterDivComboBox.setVisible(false);
			this.hideFilterButton.setIcon(VaadinIcon.ANGLE_DOUBLE_LEFT.create());
			this.hideFilterButton.setOpen(false);
		}
		else if(this.width < 727)
		{
			this.filterDivLabel.setVisible(false);
			this.hideFilterButton.setIcon(VaadinIcon.ANGLE_DOUBLE_LEFT.create());
			this.hideFilterButton.setOpen(false);
		}
		
	}
	
	private HideButton createHideFilterButton()
	{
		final HideButton button = new HideButton();
		button.setIcon(VaadinIcon.ANGLE_DOUBLE_LEFT.create());
		button.setOpen(false);
		button.addClassName("hideFilterButton");
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
	
	protected FilterEntryEditor addFilterEntryEditor(final int index)
	{
		final Checkbox          checkbox     = new Checkbox();
		final Button            deleteButton = createDeleteButton();
		final FilterEntryEditor editor       = new FilterEntryEditor(this, this, this::updateFilterData);
		this.filterEntryEditors.add(index, editor);
		
		final HorizontalLayout filterEntryRow = new HorizontalLayout(editor);
		
		if(this.width < 727)
		{
			final Dialog dialog = createPopUpDialog();
			
			final Button confirmButton = createConfirmButton();
			confirmButton.addClickListener(
				e -> createConfirmButtonListener(deleteButton, checkbox, filterEntryRow, editor, index,
					dialog));
			
			dialog.add(filterEntryRow, confirmButton);
			dialog.open();
		}
		else if(this.width >= 727)
		{
			openDiv();
			
			// +1 because of search bar at top
			this.filterDivComboBox.addComponentAtIndex(index,
				createFinalRow(deleteButton, checkbox, filterEntryRow, editor, index));
			this.filterDivLabel.addComponentAtIndex(index,
				createFinalRowLabel(editor, index));
			
		}
		// this.filterDivLabel.addComponentAtIndex(index,);
		this.index++;
		return editor;
		
	}
	
	private Button createDeleteButton()
	{
		final Button button = new Button();
		button.addClassName("deleteButton");
		button.setIcon(VaadinIcon.MINUS.create());
		return button;
	}
	
	private Dialog createPopUpDialog()
	{
		
		final Dialog dialog = new Dialog(new Label(StringResourceUtils.getResourceString("filterLabel", this)));
		dialog.setCloseOnEsc(false);
		dialog.setCloseOnOutsideClick(true);
		dialog.setId("ownIdForDialg");
		
		return dialog;
	}
	
	private Button createConfirmButton()
	{
		final Button button = new Button();
		button.setIcon(VaadinIcon.CHECK_CIRCLE.create());
		button.addClassName("confirmButton");
		return button;
	}
	
	private void createConfirmButtonListener(
		final Button deleteButton,
		final Checkbox checkbox,
		final HorizontalLayout filterEntryRow,
		final FilterEntryEditor editor,
		final int index,
		final Dialog dialog)
	{
		// +1 because of search bar at top
		this.filterDivComboBox.addComponentAtIndex(index,
			createFinalRow(deleteButton, checkbox, filterEntryRow, editor, index));
		
		dialog.close();
		if(this.filterDivComboBox.isVisible())
		{
			closeDiv();
		}
	}
	
	private String getSelectedOperator(final FilterEntryEditor editor)
	{
		if(editor.getSelectedOperator().name().equals(null))
		{
			return "";
		}
		return editor.getSelectedOperator().name();
	}
	
	private String getSelectedProperty(final FilterEntryEditor editor)
	{
		if(editor.getSelectedProperty().caption() == null)
		{
			return "";
		}
		return editor.getSelectedProperty().caption();
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
		final Button deleteButton,
		final Checkbox checkbox,
		final HorizontalLayout filterEntryRow,
		final FilterEntryEditor editor,
		final int index)
	{
		editor.addClassName("editor");
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
		return finalLayout;
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
	
	protected void deleteButtonClickListener(final FilterEntryEditor editor)
	{
		final Dialog dialog       = new Dialog();
		final Label  label        = new Label(StringResourceUtils.getResourceString("deleteMessage", this));
		final Button deleteButton = createDeleteButton(editor, dialog);
		
		final Button cancelButton = createCancelButton(dialog);
		
		final HorizontalLayout buttonLayout = new HorizontalLayout();
		buttonLayout.add(deleteButton, cancelButton);
		
		final VerticalLayout dialogLayout = new VerticalLayout();
		dialogLayout.add(label, buttonLayout);
		
		dialog.add(dialogLayout);
		dialog.open();
	}
	
	private Button createDeleteButton(final FilterEntryEditor editor, final Dialog dialog)
	{
		final Button button = new Button();
		
		button.setIcon(VaadinIcon.CHECK.create());
		button.addClickListener(event -> {
			removeFilterEntryEditor(editor);
			this.index--;
			dialog.close();
		});
		button.addClassName("deleteButton");
		return button;
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
		this.filterDivComboBox.remove(finalRow);
		
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
	
	/**
	 * This Method will switch from the Div with <b>ComboBoxes</b>
	 * to the Div with <b>Labels</b>
	 */
	private void swapToLabel()
	{
		this.filterDivLabel.setVisible(true);
		this.filterDivComboBox.setVisible(false);
		this.getContent().replace(this.filterDivComboBox, this.filterDivLabel);
	}
	
	/**
	 * This Method will switch from the Div with <b>Labels</b>
	 * to the Div with <b>ComboBoxes</b>
	 */
	private void swapToComboBox()
	{
		this.filterDivLabel.setVisible(false);
		this.filterDivComboBox.setVisible(true);
		this.getContent().replace(this.filterDivLabel, this.filterDivComboBox);
	}
	
	protected void updateFilterDivLabel(
		final int index,
		final FilterEntryEditor fee,
		final ComboBox<?> combo)
	{
		
		/*
		 * TODO:
		 * Irgendwie funktioniert es doch es muss noch angepasst werden, dass die Labels entsprechen leer werden
		 * wenn die Propertie geändert werden und somit der opertator null wird.
		 * Außerdem sind die weiteren comboboxen nicht mit drin
		 * 
		 *
		 */
		
		final Object comboObject = combo.getValue();
		
		if(comboObject instanceof FilterProperty<?>)
		{
			final FilterProperty<?> property = (FilterProperty<?>)comboObject;
			this.filterProperty = property.caption();
			int counter = 1;
			for(final Object c : this.filterDivLabel.getChildren().toArray())
			{
				if(counter == index)
				{
					final HorizontalLayout finalLayout    = (HorizontalLayout)c;
					final HorizontalLayout filterEntryRow = (HorizontalLayout)finalLayout.getComponentAt(0);
					filterEntryRow.removeAll();
					filterEntryRow.add(new Label(this.filterProperty + " -> " + this.filterOperator));
				}
				counter++;
			}
		}
		else if(comboObject instanceof FilterOperator)
		{
			final FilterOperator operator = (FilterOperator)comboObject;
			this.filterOperator = operator.name();
			int counter = 1;
			for(final Object c : this.filterDivLabel.getChildren().toArray())
			{
				if(counter == index)
				{
					final HorizontalLayout finalLayout    = (HorizontalLayout)c;
					final HorizontalLayout filterEntryRow = (HorizontalLayout)finalLayout.getComponentAt(0);
					filterEntryRow.removeAll();
					filterEntryRow.add(new Label(this.filterProperty + " -> " + this.filterOperator));
				}
				counter++;
			}
		}

	}

	private Component getComponentFromEditorAtIndex(final int index, final FilterEntryEditor editor)
	{
		
		Component component = null;
		final int count     = 0;
		for(final Object c : editor.getChildren().toArray())
		{
			if(count == index)
			{
				component = (Component)c;
			}
		}
		return component;
	}
	
	private FormLayout getEditorFromDiv(final int index, final Div filter)
	{
		FormLayout editor = null;
		int        count  = 1;
		for(final Object component : filter.getChildren().toArray())
		{
			if(count == index)
			{
				final HorizontalLayout finalLayout    = (HorizontalLayout)component;
				final HorizontalLayout filterEntryRow = (HorizontalLayout)finalLayout.getComponentAt(0);
				editor = (FormLayout)filterEntryRow.getComponentAt(0);
			}
			else
			{
				count++;
			}
		}
		return editor;
	}
	
	private Component createFinalRowLabel(
		final FilterEntryEditor editor,
		final int index)
	{
		
		final Button   deleteButton = createDeleteButton();
		final Checkbox checkbox     = new Checkbox();
		editor.addClassName("editor");
		defineCheckBox(checkbox);
		checkbox
			.addValueChangeListener(listener -> checkboxValueChanceForEntryEditor(checkbox, index, editor));
		
		defineDeleteButton(deleteButton);
		deleteButton.addClickListener(listener -> deleteButtonClickListener(editor));
		
		final FormLayout finalButtonLayout = new FormLayout();
		finalButtonLayout.addClassName("finalButtonLayout");
		finalButtonLayout.add(checkbox, deleteButton);
		
		final HorizontalLayout filterEntryRow = new HorizontalLayout();
		filterEntryRow.addClassName("filterEntryRow");
		filterEntryRow.setEnabled(true);
		filterEntryRow.add(new Label(this.filterProperty));
		filterEntryRow.add(new Label(this.filterOperator));
		
		final HorizontalLayout finalLayout = new HorizontalLayout();
		finalLayout.add(filterEntryRow, finalButtonLayout);
		finalLayout.addClassName("finalLayout");
		return finalLayout;
	}
	
	public int getIndex()
	{
		return this.index;
	}
	
}
