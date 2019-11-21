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

import javax.validation.constraints.Null;

import com.rapidclipse.framework.server.data.filter.Composite;
import com.rapidclipse.framework.server.data.filter.Composite.Connector;
import com.rapidclipse.framework.server.data.filter.Filter;
import com.rapidclipse.framework.server.data.provider.DataProviderFilterAdapter;
import com.rapidclipse.framework.server.resources.StringResourceUtils;
import com.rapidclipse.framework.server.ui.filter.helper.AddButton;
import com.rapidclipse.framework.server.ui.filter.helper.Buttons;
import com.rapidclipse.framework.server.ui.filter.helper.CancelButton;
import com.rapidclipse.framework.server.ui.filter.helper.ComboDiv;
import com.rapidclipse.framework.server.ui.filter.helper.DeleteButton;
import com.rapidclipse.framework.server.ui.filter.helper.EditButton;
import com.rapidclipse.framework.server.ui.filter.helper.EntryRowLabel;
import com.rapidclipse.framework.server.ui.filter.helper.FilterCheckBox;
import com.rapidclipse.framework.server.ui.filter.helper.FilterDiv;
import com.rapidclipse.framework.server.ui.filter.helper.HideButton;
import com.rapidclipse.framework.server.ui.filter.helper.LabelDiv;
import com.rapidclipse.framework.server.ui.filter.helper.ReplaceabelEditor;
import com.rapidclipse.framework.server.ui.filter.helper.Searchbar;
import com.rapidclipse.framework.server.ui.filter.helper.UpdateButton;
import com.rapidclipse.framework.server.util.ServiceLoader;
import com.vaadin.flow.component.AbstractCompositeField;
import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.HasComponents;
import com.vaadin.flow.component.HasOrderedComponents;
import com.vaadin.flow.component.HasSize;
import com.vaadin.flow.component.UI;
import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.dependency.JavaScript;
import com.vaadin.flow.component.dependency.StyleSheet;
import com.vaadin.flow.component.formlayout.FormLayout;
import com.vaadin.flow.component.grid.Grid;
import com.vaadin.flow.component.html.Div;
import com.vaadin.flow.component.html.Label;
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
	private static final int BREAKPOINT    = 800;
	private boolean          caseSensitive = false;
	private char             wildcard      = '*';

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
	private final AddButton               addFilterButton    = new AddButton();
	private final HideButton              hideFilterButton   = new HideButton();
	public final FilterDiv                filterDiv          = new FilterDiv();
	public final ComboDiv                 comboDiv           = new ComboDiv();
	protected LabelDiv                    labelDiv           = new LabelDiv();
	private final List<ReplaceabelEditor> filterEntryEditors = new ArrayList<>();
	public int                            rowIndex           = 0;                // needed to add filter in the right
																					// place of this.filterEntryEditors
	private final Searchbar               searchBar          = new Searchbar();
	private Registration                  addButtonClick;
	private int                           width              = 0;

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
			resizeLabelRow();
		});

		this.searchTextField = createSearchTextField();
		this.searchTextField.addValueChangeListener(event -> updateFilterData());
		this.searchTextField.setEnabled(false);

		this.hideFilterButton.defineButton();
		this.hideFilterButton.setClickListener(this, null);

		this.comboDiv.defineDiv();
		this.labelDiv.defineDiv();

		this.addFilterButton.defineButton();
		this.addButtonClick = this.addFilterButton.addClickListener(event -> {
			this.searchBar.remove(this.addFilterButton);
			addFilterEntryEditor(new FilterEntryEditor(this, this, this::updateFilterData),
				this.rowIndex);
		});
		this.addFilterButton.setEnabled(false);
		this.filterDiv.defineDiv();
		this.filterDiv.add(this.labelDiv, this.comboDiv);

		this.searchBar.defineSearchbar();
		this.searchBar.createSearchBar(this.searchTextField, this.hideFilterButton, this.addFilterButton);

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
	 * Creates the Layout which is then seen from the User inside the Div
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
	private HorizontalLayout createEntryRowLabel(final ReplaceabelEditor replace)
	{

		final FilterEntryEditor editor = replace.getOriginal();
		final EntryRowLabel     entry  = new EntryRowLabel(editor);
		replace.setEntryRow(entry);

		if(this.width <= BREAKPOINT)
		{
			return entry.getShortLayout();
		}
		else
		{
			return entry.getLongLayout();
		}
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
	private HorizontalLayout createEntryRowCombo(final FilterEntryEditor editor)
	{
		final HorizontalLayout layout = new HorizontalLayout(editor);
		layout.setEnabled(true);
		layout.addClassName(StringResourceUtils.getResourceString("entryRowComboBox", this));
		return layout;
	}

	/**
	 * Creates the the Button layout for the given {@link Component}s
	 *
	 * @param components
	 *            -> As many {@link Component} as necessary
	 * @return -> {@link HorizontalLayout}
	 */
	private HorizontalLayout createButtonLayout(final Component... components)
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
	 ************ Defining Stuff***********
	 **************************************/

	/**
	 * Define all Buttons and (if given) the checkbox. Also add the right Listener to them, which can be seen in their
	 * classes.
	 *
	 * @param editor
	 *            -> {@link ReplaceabelEditor}
	 * @param checkbox
	 *            -> {@link FilterCheckBox} or {@link Null}
	 * @param buttons
	 *            -> {@link Buttons}
	 */
	private void
		definingButtons(final ReplaceabelEditor editor, final FilterCheckBox checkbox, final Buttons... buttons)
	{
		if(checkbox != null)
		{
			checkbox.defineCheckBox();
			checkbox.setValueChangeListener(this, editor);
		}
		for(final Buttons b : buttons)
		{
			b.defineButton();
			b.setClickListener(this, editor);
		}

	}

	/*****************************************************************************************************************************************
	 */
	/**************************************
	 ************ Listener***********
	 **************************************/

	/**
	 * Add a clickListener to a {@link Button}. This will add a new {@link Label} to the <b>labelDiv</b> with the
	 * selected data.
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
		final ReplaceabelEditor editor,
		final FilterCheckBox checkbox,
		final EditButton editButton,
		final DeleteButton deleteButton)
	{
		removeList(this.addButtonClick);
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
	 *            -> {@link ReplaceabelEditor}
	 */
	public void activateFilterEntryEditor(final ReplaceabelEditor editor)
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
	public void deactivateFilterEntryEditor(final ReplaceabelEditor editor)
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
	public void newFilterEntry(final int index)
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
	public void updateComboBox(
		final ReplaceabelEditor editor,
		final UpdateButton updateButton,
		final CancelButton cancelButton)
	{
		definingButtons(editor, null, updateButton, cancelButton);

		editor.getOriginal().setVisible(true);

		this.comboDiv.add(
			createFinalLayout(
				createEntryRowCombo(editor.getOriginal()),
				createButtonLayout(updateButton, cancelButton)));
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

	public void
		updateReplaceableOriginal(final ReplaceabelEditor replace, final FilterEntryEditor editor)
	{
		replace.setOriginal(editor);
		replace.updateCopy();
	}

	/**
	 * Creates a new {@link HorizontalLayout} which will then replace the old {@link HorizontalLayout} inside the
	 * label{@link Div}. This will update the labelRow which can be seen by the user, inside the <b> labelDiv</b>
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
	public void updateLabelRow(
		final ReplaceabelEditor editor,
		final FilterCheckBox checkbox,
		final EditButton editButton,
		final DeleteButton deleteButton)
	{
		definingButtons(editor, checkbox, editButton, deleteButton);

		final HorizontalLayout finalLayout = createFinalLayout(createEntryRowLabel(editor),
			createButtonLayout(checkbox, editButton, deleteButton));
		
		replaceLabelRow(editor.getLabelLayout(), finalLayout, this.labelDiv);

		editor.setLabelLayout(finalLayout);

		updateReplaceabelCopy(editor);
		updateFilterData();
		newFilterEntry(this.rowIndex);
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
		replaceLabelRow(final Component oldComponent, final Component newComponent, final HasComponents wrapper)
	{
		if(oldComponent == null && newComponent == null)
		{
			// NO-OP
			return;
		}
		if(oldComponent == null)
		{
			wrapper.add(newComponent);
		}
		else if(newComponent == null)
		{
			wrapper.remove(oldComponent);
		}
		else
		{
			final Element element  = wrapper.getElement();
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
				wrapper.add(newComponent);
			}
		}
	}

	/**
	 * Method used to make the {@link Label}s resizeabel.<br>
	 * It looks for the current width and compare it with the {@link FilterComponent #BREAKPOINT} <br>
	 * Depending on the width the finalLayout will be changed with the Short- or Long one
	 */
	private void resizeLabelRow()
	{
		for(final ReplaceabelEditor editor : this.filterEntryEditors)
		{
			final HorizontalLayout finalLayout = editor.getLabelLayout();
			final HorizontalLayout longLayout  = editor.getEntryRow().getLongLayout();
			final HorizontalLayout shortLayout = editor.getEntryRow().getShortLayout();

			if(this.width < BREAKPOINT)
			{
				if(finalLayout.getComponentAt(0) == longLayout)
				{
					finalLayout.replace(longLayout, shortLayout);
				}
			}
			else
			{
				if(finalLayout.getComponentAt(0) == shortLayout)
				{
					finalLayout.replace(shortLayout, longLayout);
				}
			}
		}
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
		removeDataFromDiv(editor);

		deactivateFilterEntryEditor(editor);

		updateFilterData();
	}

	/**
	 * Removes the Layout from the labelDiv, depending on the given editor
	 *
	 * @param editor
	 *            -> {@link ReplaceabelEditor}
	 */
	private void removeDataFromDiv(final ReplaceabelEditor editor)
	{

		this.labelDiv.remove(editor.getLabelLayout());
	}

	/**
	 * Removes the current Listener from the given one.
	 *
	 * @param listener
	 *            -> {@link Registration}
	 */
	private void removeList(final Registration listener)
	{
		listener.remove();
	}

	/*****************************************************************************************************************************************
	 */
	/**************************************
	 ************ Swapping Stuff***********
	 **************************************/
	
	/**
	 * Opens the <b>filterDiv</b> by clicking on the {@link HideButton}
	 * <br>
	 * Also setting 'open' in the {@link HideButton} to true
	 */
	public void openDiv()
	{
		this.hideFilterButton.open();
		this.filterDiv.setVisible(true);
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
	 * @param index
	 *            -> {@link Integer}
	 * @return -> {@link FilterEntryEditor}
	 */
	public FilterEntryEditor addFilterEntryEditor(final FilterEntryEditor editor, final int index)
	{
		final ReplaceabelEditor replace      = new ReplaceabelEditor(editor);
		final CancelButton      cancelButton = new CancelButton();
		cancelButton.defineButton();

		addButtonClickListener(this.addFilterButton, replace, new FilterCheckBox(),
			new EditButton(),
			new DeleteButton());

		cancelButton.setClickListener(this, index);

		this.comboDiv.add(
			createFinalLayout(
				createEntryRowCombo(editor),
				createButtonLayout(this.addFilterButton, cancelButton)));

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

		definingButtons(editor, checkbox, editButton, deleteButton);

		this.filterEntryEditors.add(editor);

		final HorizontalLayout finalLayout = createFinalLayout(createEntryRowLabel(editor),
			createButtonLayout(checkbox, editButton, deleteButton));
		// createButtonLayout(checkbox, editButton, deleteButton)
		editor.setLabelLayout(finalLayout);

		this.labelDiv.add(finalLayout);
		this.labelDiv.setVisible(true);

		updateReplaceabelCopy(editor);
		updateFilterData();
		newFilterEntry(this.rowIndex);
	}

	/*
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
