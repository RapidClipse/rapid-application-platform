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

	private FilterProperty<?>                selectedProperty;
	private FilterOperator                   selectedOperator;
	private List<FilterValueEditorComposite> valueEditors;
	private List<Registration>               valueEditorRegistrations;
	private final Runnable                   filterChangeHandler;
	private final FilterComponent            component;

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

		this.selectedOperator = this.operatorComboBox.getValue();
		this.selectedProperty = this.propertyComboBox.getValue();

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

	public void setFilterEntry(final FilterEntry data)
	{
		final FilterProperty<?> property = getPropertyFromContext(data);
		this.propertyComboBox.setValue(property);
		propertySelectionChanged();

		if(property != null)
		{
			declareOperator(data);

		}
	}

	/**
	 *
	 * @param data
	 *            -> {@link FilterEntry}
	 */
	private void declareOperator(final FilterEntry data)
	{
		final FilterOperator operator = getOperatorFromContext(data);
		this.operatorComboBox.setValue(operator);
		operatorSelectionChanged();

		if(operator != null)
		{
			declareValues(data);
		}
	}

	@SuppressWarnings("unchecked")
	private void declareValues(final FilterEntry data)
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

		final Object[] values = getValuesFromComposite();
		if(values.length != this.valueEditors.size())
		{
			return null;
		}

		return new FilterEntry(this.selectedProperty.identifier(), this.selectedOperator.key(),
			values);
	}

	private Object[] getValuesFromComposite()
	{
		return this.valueEditors.stream().map(FilterValueEditorComposite::getValue)
			.filter(Objects::nonNull).toArray();
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

			enableOperatorComboBox();
		}
		else
		{
			this.operatorComboBox.setVisible(false);
		}
	}

	private void enableOperatorComboBox()
	{
		this.operatorComboBox.setVisible(true);
		final List<FilterOperator> operators = this.context.getFilterOperatorRegistry().getAll()
			.stream().filter(op -> op.isSupported(this.selectedProperty)).collect(toList());
		this.operatorComboBox.setItems(operators);
	}

	/**
	 * 'Event' which will be executed if the {@link FilterOperator} inside the operatorComboBox is chanced
	 */
	protected void operatorSelectionChanged()
	{
		final List<Object> lastValues = removeValueEditors();

		this.selectedOperator = this.operatorComboBox.getValue();

		if(this.selectedOperator != null)
		{
			fillValueEditors(lastValues);
			enableValues();
		}
	}

	/**
	 * Deklare <b> valueEditors </b> with the Composite of <b>selectedOperator</b>
	 *
	 * @param lastValues
	 *            {@link List} of type {@link Object}
	 */
	@SuppressWarnings("unchecked")
	private void fillValueEditors(final List<Object> lastValues)
	{
		this.valueEditors = this.selectedOperator.createComposites(this.context,
			this.selectedProperty);

		for(int i = 0, c = Math.min(lastValues.size(), this.valueEditors.size()); i < c; i++)
		{
			this.valueEditors.get(i).setValue(lastValues.get(i));

		}
	}

	/**
	 * enable the 'addFilterButton' of <b> this.component </b><br>
	 * and iterate through <b>this.valueEditors</b> to add them to <b>this</b> layout
	 */
	private void enableValues()
	{
		this.component.addFilterButton.setEnabled(true);
		for(final FilterValueEditorComposite obj : this.valueEditors)
		{
			final HasValueAndElement<?, ?> hasValueComponent = obj.component();
			tryIfDatePicker(hasValueComponent);
		}
	}

	/**
	 * Checks if the given Component is a DatePicker than adding the Component to <b>this</b>
	 *
	 * @param hasValueComponent
	 *            {@link HasValueAndElement}
	 */
	private void tryIfDatePicker(final HasValueAndElement<?, ?> hasValueComponent)
	{
		try
		{
			final DatePicker date = (DatePicker)hasValueComponent;
			date.setValue(LocalDate.now());
			add(date);
		}
		catch(final ClassCastException e)
		{
			add((Component)hasValueComponent);
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

	private FilterProperty<?> getPropertyFromContext(final FilterEntry data)
	{
		return this.context.getFilterSubject()
			.filterableProperty(data.getPropertyIdentifier());
	}

	private FilterOperator getOperatorFromContext(final FilterEntry data)
	{
		return this.context.getFilterOperatorRegistry()
			.get(data.getOperatorKey());
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
	 * @return The filterChangeHandler -> {@link Runnable}
	 */
	public Runnable getFilterChangeHandler()
	{
		return this.filterChangeHandler;
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
	 * {@link #FilterEntryEditor(FilterContext, ComboBox, ComboBox, Runnable, FilterComponent, FilterEntryEditor)}
	 *
	 * @param original
	 *            -> {@link FilterEntryEditor}
	 * @return -> The Copy of the given Object as {@link FilterEntryEditor}
	 */
	public static FilterEntryEditor copyEditor(final FilterEntryEditor original)
	{
		return new FilterEntryEditor(original.getContext(), original.getPropertyComboBox(),
			original.getOperatorComboBox(), original.getFilterChangeHandler(),
			original.getComponent(), original);

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
	 * @param filterChangeHandler
	 *            -> {@link Runnable}
	 * @param component
	 *            -> {@link FilterComponent}
	 * @param original
	 *            -> {@link FilterEntryEditor}
	 */
	private FilterEntryEditor(
		final FilterContext context,
		final ComboBox<FilterProperty<?>> propertyComboBox,
		final ComboBox<FilterOperator> operatorComboBox,
		final Runnable filterChangeHandler,
		final FilterComponent component,
		final FilterEntryEditor original)
	{
		this.setWidthFull();
		this.context             = context;
		this.filterChangeHandler = filterChangeHandler;
		this.component           = component;
		/*
		 * Property Comob
		 */

		this.propertyComboBox = createPropertyComboBox();
		defineCopyPropertyCombobox(propertyComboBox);

		/*
		 * OperatorCombo
		 */
		this.operatorComboBox = createOperatorComboBox();
		defineCopyOperatorCombobox(operatorComboBox);

		this.selectedProperty = this.propertyComboBox.getValue();
		this.selectedOperator = this.operatorComboBox.getValue();

		add(this.propertyComboBox, this.operatorComboBox);
		this.setFilterEntry(original.getFilterEntry());

	}

	private void defineCopyPropertyCombobox(final ComboBox<FilterProperty<?>> comboOriginal)
	{
		this.propertyComboBox.setItems(this.context.getFilterSubject().filterableProperties());
		this.propertyComboBox.setValue(comboOriginal.getValue());
		this.propertyComboBox.addValueChangeListener(event -> propertySelectionChanged());
	}

	private void defineCopyOperatorCombobox(final ComboBox<FilterOperator> comboOriginal)
	{
		this.operatorComboBox.setVisible(true);
		this.operatorComboBox.setItems(Collections.emptyList());
		this.operatorComboBox.setValue(comboOriginal.getValue());
		this.operatorComboBox.addValueChangeListener(event -> operatorSelectionChanged());
	}

}
