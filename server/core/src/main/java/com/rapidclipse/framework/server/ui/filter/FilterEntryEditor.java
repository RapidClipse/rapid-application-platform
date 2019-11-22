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

import static java.util.stream.Collectors.toList;

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Objects;

import com.rapidclipse.framework.server.data.filter.Filter;
import com.rapidclipse.framework.server.resources.StringResourceUtils;
import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.HasValue.ValueChangeEvent;
import com.vaadin.flow.component.HasValue.ValueChangeListener;
import com.vaadin.flow.component.HasValueAndElement;
import com.vaadin.flow.component.combobox.ComboBox;
import com.vaadin.flow.component.datepicker.DatePicker;
import com.vaadin.flow.component.formlayout.FormLayout;
import com.vaadin.flow.shared.Registration;


/**
 * @author XDEV Software
 *
 */
@SuppressWarnings("rawtypes")
public class FilterEntryEditor extends FormLayout
{
	private final FilterContext context;

	private final ComboBox<FilterProperty<?>> propertyComboBox;
	private final ComboBox<FilterOperator>    operatorComboBox;

	private final ValueChangeListener<ValueChangeEvent<?>> filterValueChangeListener;
	private FilterProperty<?>                              selectedProperty;
	private FilterOperator                                 selectedOperator;
	private List<FilterValueEditorComposite>               valueEditors;
	private List<Registration>                             valueEditorRegistrations;
	private final int                                      rowIndex;
	private final Runnable                                 filterChangeHandler;
	private final FilterComponent                          component;

	/**
	 * The Main Constructor to create a new FilterEntryEditor object.
	 *
	 * @param filterComponent
	 *            -> {@link FilterComponent}
	 * @param context
	 *            -> {@link FilterContext}
	 * @param filterChangeHandler
	 *            -> {@link Runnable}
	 */
	public FilterEntryEditor(
		final FilterComponent filterComponent,
		final FilterContext context,
		final Runnable filterChangeHandler)
	{
		super();
		this.rowIndex            = filterComponent.getIndex();
		this.context             = context;
		this.filterChangeHandler = filterChangeHandler;
		this.setWidthFull();
		this.propertyComboBox = createPropertyComboBox();
		this.component        = filterComponent;
		this.component.addFilterButton.setEnabled(false);

		this.propertyComboBox.setItems(context.getFilterSubject().filterableProperties());

		this.propertyComboBox.addValueChangeListener(event -> propertySelectionChanged());

		this.operatorComboBox = createOperatorComboBox();
		/*
		 * Workaround for
		 * https://github.com/vaadin/vaadin-combo-box-flow/issues/169
		 */
		this.operatorComboBox.setItems(Collections.emptyList());
		this.operatorComboBox.addValueChangeListener(event -> operatorSelectionChanged());

		this.filterValueChangeListener = event -> filterChangeHandler.run();
		this.selectedOperator          = this.operatorComboBox.getValue();
		this.selectedProperty          = this.propertyComboBox.getValue();

		// add the ComboBoxes to the FormLayout
		add(this.propertyComboBox, this.operatorComboBox);
	}

	/**
	 * Creates the ComboBox which then holds all properties
	 *
	 * @return The ComboBox -> {@link ComboBox} type {@link FilterProperty}
	 */
	protected ComboBox<FilterProperty<?>> createPropertyComboBox()
	{
		final ComboBox<FilterProperty<?>> combo = new ComboBox<>();
		combo.setItemLabelGenerator(FilterProperty::caption);
		combo.setAllowCustomValue(false);
		combo.setPlaceholder(getComboBoxPlaceholder());
		combo.addClassName("propertyComboBox");
		return combo;
	}

	/**
	 * Creates the ComboBox which will than hold all operators.
	 *
	 * @return The ComboBox -> {@link ComboBox} type {@link FilterOperator}
	 */
	protected ComboBox<FilterOperator> createOperatorComboBox()
	{
		final ComboBox<FilterOperator> combo = new ComboBox<>();
		combo.setItemLabelGenerator(FilterOperator::name);
		combo.setAllowCustomValue(false);
		combo.setPlaceholder(getComboBoxPlaceholder());
		combo.setVisible(false);
		combo.addClassName("operatorComboBox");
		return combo;
	}

	/**
	 * Set the Values of the Filter given by the data.
	 * Sets Property, Operator and all Values.
	 *
	 * @param data
	 *            -> {@link FilterEntry}
	 */
	@SuppressWarnings("unchecked")
	public void setFilterEntry(final FilterEntry data)
	{
		final FilterProperty<?> property = this.context.getFilterSubject()
			.filterableProperty(data.getPropertyIdentifier());
		this.propertyComboBox.setValue(property);
		propertySelectionChanged();

		if(property != null)
		{
			final FilterOperator operator = this.context.getFilterOperatorRegistry()
				.get(data.getOperatorKey());
			this.operatorComboBox.setValue(operator);
			operatorSelectionChanged();

			if(operator != null)
			{
				final Object[] values = data.getValues();
				if(values != null && values.length == this.valueEditors.size())
				{
					int i = 0;
					for(final Object value : values)
					{
						this.valueEditors.get(i++).setValue(value);

					}
				}
			}
		}
	}

	/**
	 * Gets the setted Filters given by the ComboBoxes and ValueFields
	 *
	 * @return -> new {@link FilterEntry} with setted Property, Operator and Values
	 */
	public FilterEntry getFilterEntry()
	{
		if(this.selectedProperty == null || this.selectedOperator == null
			|| this.valueEditors == null)
		{
			return null;
		}

		final Object[] values = this.valueEditors.stream().map(FilterValueEditorComposite::getValue)
			.filter(Objects::nonNull).toArray();
		if(values.length != this.valueEditors.size())
		{
			return null;
		}

		return new FilterEntry(this.selectedProperty.identifier(), this.selectedOperator.key(),
			values);
	}

	/**
	 *
	 * @return The ComboBox Placeholder -> {@link String} getting trough
	 *         {@link StringResourceUtils#getResourceString(String, java.util.Locale)}
	 */
	protected String getComboBoxPlaceholder()
	{
		return StringResourceUtils.getResourceString("selectOption", this);
	}

	/**
	 * Changes the selectedProperty to the new Value of the ComboBox.
	 * Also, if the Property != null
	 * -> Operator ComboBox will be Visible and is filled with the needed Objects
	 */
	protected void propertySelectionChanged()
	{
		removeValueEditors();

		this.selectedProperty = this.propertyComboBox.getValue();
		if(this.selectedProperty != null)
		{
			this.operatorComboBox.setVisible(true);
			final List<FilterOperator> operators = this.context.getFilterOperatorRegistry().getAll()
				.stream().filter(op -> op.isSupported(this.selectedProperty)).collect(toList());
			this.operatorComboBox.setItems(operators);

		}
		else
		{
			this.operatorComboBox.setVisible(false);
		}
	}

	/**
	 * 'Event' which will be executed if the {@link FilterOperator} inside the operatorComboBox is chanced
	 */
	@SuppressWarnings("unchecked")
	protected void operatorSelectionChanged()
	{
		final List<Object> lastValues = removeValueEditors();

		this.selectedOperator = this.operatorComboBox.getValue();

		if(this.selectedOperator != null)
		{
			this.valueEditors = this.selectedOperator.createComposites(this.context,
				this.selectedProperty);

			for(int i = 0, c = Math.min(lastValues.size(), this.valueEditors.size()); i < c; i++)
			{
				this.valueEditors.get(i).setValue(lastValues.get(i));

			}
			this.component.addFilterButton.setEnabled(true);
			for(final FilterValueEditorComposite obj : this.valueEditors)
			{

				final HasValueAndElement<?, ?> component = obj.component();
				try
				{
					final DatePicker date = (DatePicker)component;
					date.setValue(LocalDate.now());
					add(date);
				}
				catch(final ClassCastException e)
				{
					add((Component)component);
				}

			}
		}
	}

	protected List<Object> removeValueEditors()
	{
		final List<Object> lastValues = new ArrayList<>();

		if(this.valueEditors != null)
		{
			for(final FilterValueEditorComposite valueEditor : this.valueEditors)
			{
				lastValues.add(valueEditor.getValue());
				remove((Component)valueEditor.component());
			}
			this.valueEditors = null;
		}

		if(this.valueEditorRegistrations != null)
		{
			this.valueEditorRegistrations.forEach(Registration::remove);
			this.valueEditorRegistrations = null;
		}

		return lastValues;
	}

	/**
	 *
	 * @return The Filter -> {@link Filter}
	 */
	public Filter getFilter()
	{
		if(this.selectedProperty == null || this.selectedOperator == null
			|| this.valueEditors == null)
		{
			return null;
		}

		return this.selectedOperator.createFilter(this.context, this.selectedProperty,
			this.valueEditors);
	}

	/**
	 *
	 * @return The selectedProperty -> {@link FilterProperty}
	 */
	public FilterProperty<?> getSelectedProperty()
	{
		return this.selectedProperty;
	}

	/**
	 *
	 * @return The selectedOperator -> {@link FilterOperator}
	 */
	public FilterOperator getSelectedOperator()
	{
		return this.selectedOperator;
	}

	/**
	 *
	 * @return List of valueEditors -> {@link List} type {@link FilterValueEditorComposite}
	 */
	public List<FilterValueEditorComposite> getValueEditors()
	{
		return this.valueEditors;
	}

	/**
	 *
	 * @param valueEditors
	 *            -> {@link List} type {@link FilterValueEditorComposite}
	 */
	public void setValueEditors(final List<FilterValueEditorComposite> valueEditors)
	{
		this.valueEditors = valueEditors;
	}

	/**
	 * @return List of valueEditorRegistrations -> {@link List} type {@link Registration}
	 */
	public List<Registration> getValueEditorRegistrations()
	{
		return this.valueEditorRegistrations;
	}

	/**
	 * @param valueEditorRegistrations
	 *            -> {@link List} type {@link Registration}
	 */
	public void setValueEditorRegistrations(final List<Registration> valueEditorRegistrations)
	{
		this.valueEditorRegistrations = valueEditorRegistrations;
	}

	/**
	 * @return The context -> {@link FilterContext}
	 */
	public FilterContext getContext()
	{
		return this.context;
	}

	/**
	 * @return The propertyComboBox -> {@link ComboBox} type {@link FilterProperty}
	 */
	public ComboBox<FilterProperty<?>> getPropertyComboBox()
	{
		return this.propertyComboBox;
	}

	/**
	 * @return The operatorComboBox -> {@link ComboBox} type {@link FilterOperator}
	 */
	public ComboBox<FilterOperator> getOperatorComboBox()
	{
		return this.operatorComboBox;
	}

	/**
	 * @return List of filterValueChangeListener -> {@link ValueChangeListener} type {@link ValueChangeEvent}
	 */
	public ValueChangeListener<ValueChangeEvent<?>> getFilterValueChangeListener()
	{
		return this.filterValueChangeListener;
	}

	/**
	 * @return The rowIndes -> {@link Integer}
	 */
	public int getRowIndex()
	{
		return this.rowIndex;
	}

	/**
	 * @return The filterChangeHandler -> {@link Runnable}
	 */
	public Runnable getFilterChangeHandler()
	{
		return this.filterChangeHandler;
	}

	/**
	 * @param selectedProperty
	 *            -> {@link FilterProperty}
	 */
	public void setSelectedProperty(final FilterProperty<?> selectedProperty)
	{
		this.selectedProperty = selectedProperty;
	}

	/**
	 * @param selectedOperator
	 *            -> {@link FilterOperator}
	 */
	public void setSelectedOperator(final FilterOperator selectedOperator)
	{
		this.selectedOperator = selectedOperator;
	}
	
	/**
	 * @return the component
	 */
	public FilterComponent getComponent()
	{
		return this.component;
	}
	
	/**
	 * Method to Copy the given Object into a new {@link FilterEntryEditor} <br>
	 * Calls the private Constructor
	 * {@link #FilterEntryEditor(FilterContext, ComboBox, ComboBox, ValueChangeListener, FilterProperty, FilterOperator, List, List, int, Runnable, FilterEntryEditor)}
	 *
	 * @param original
	 *            -> {@link FilterEntryEditor}
	 * @return -> The Copy of the given Object as {@link FilterEntryEditor}
	 */
	public static FilterEntryEditor copyEditor(final FilterEntryEditor original)
	{
		return new FilterEntryEditor(original.getContext(), original.getPropertyComboBox(),
			original.getOperatorComboBox(),
			original.getRowIndex(), original.getFilterChangeHandler(), original.getComponent(), original);

	}

	/**
	 * Constructor to copy an object
	 *
	 * All param are given by the original object.
	 * Call this <b> only</b> with <b>copyEditor(editor);</b>
	 *
	 * @param context
	 *            -> {@link FilterContext}
	 * @param propertyComboBox
	 *            -> {@link ComboBox} type {@link FilterProperty}
	 * @param operatorComboBox
	 *            -> {@link ComboBox} type {@link FilterOperator}
	 * @param filterValueChangeListener
	 *            -> {@link ValueChangeListener} type {@link ValueChangeEvent}
	 * @param selectedProperty
	 *            -> {@link FilterProperty}
	 * @param selectedOperator
	 *            -> {@link FilterOperator}
	 * @param valueEditors
	 *            -> {@link List} type {@link FilterValueEditorComposite}
	 * @param valueEditorRegistrations
	 *            -> {@link List} type {@link Registration}
	 * @param filterComponent
	 *            -> {@link FilterComponent}
	 * @param rowIndex
	 *            -> {@link Integer}
	 * @param filterChangeHandler
	 *            -> {@link Runnable}
	 */
	private FilterEntryEditor(
		final FilterContext context,
		final ComboBox<FilterProperty<?>> propertyComboBox,
		final ComboBox<FilterOperator> operatorComboBox,
		final int rowIndex,
		final Runnable filterChangeHandler,
		final FilterComponent component,
		final FilterEntryEditor original)
	{
		this.setWidthFull();
		this.context             = context;
		this.filterChangeHandler = filterChangeHandler;
		this.rowIndex            = rowIndex;
		this.component           = component;
		/*
		 * Property Comob
		 */

		this.propertyComboBox = createPropertyComboBox();
		this.propertyComboBox.setItems(this.context.getFilterSubject().filterableProperties());
		this.propertyComboBox.setValue(propertyComboBox.getValue());
		this.propertyComboBox.addValueChangeListener(event -> propertySelectionChanged());

		/*
		 * OperatorCombo
		 */
		this.operatorComboBox = createOperatorComboBox();
		this.operatorComboBox.setVisible(true);
		this.operatorComboBox.setItems(Collections.emptyList());
		this.operatorComboBox.setValue(operatorComboBox.getValue());
		this.operatorComboBox.addValueChangeListener(event -> operatorSelectionChanged());

		this.filterValueChangeListener = event -> this.filterChangeHandler.run();
		this.selectedProperty          = this.propertyComboBox.getValue();
		this.selectedOperator          = this.operatorComboBox.getValue();

		add(this.propertyComboBox, this.operatorComboBox);
		this.setFilterEntry(original.getFilterEntry());

	}

}
