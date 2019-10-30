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
import com.vaadin.flow.component.formlayout.FormLayout;
import com.vaadin.flow.shared.Registration;


/**
 * @author XDEV Software
 *
 */
@SuppressWarnings("rawtypes")
public class FilterEntryEditorV2 extends FormLayout
{
	private final FilterContext context;

	private final ComboBox<FilterProperty<?>> propertyComboBox;
	private final ComboBox<FilterOperator>    operatorComboBox;

	private final ValueChangeListener<ValueChangeEvent<?>> filterValueChangeListener;
	private FilterProperty<?>                              selectedProperty;
	private FilterOperator                                 selectedOperator;
	private List<FilterValueEditorComposite>               valueEditors;
	private List<Registration>                             valueEditorRegistrations;

	/*
	 * Constructor
	 *
	 * @context =
	 *
	 * @filterChangeHandler =
	 *
	 * @propertyComboBox = First ComboBox to select from
	 *
	 * @operatorComboBox = Second ComboBox to define the specific task
	 *
	 * @filterValueChangeListener = Listener if a value inside the filter has changed
	 */
	public FilterEntryEditorV2(final FilterContext context, final Runnable filterChangeHandler)
	{
		super();
		this.context = context;
		this.setWidthFull();

		this.propertyComboBox = createPropertyComboBox();
		this.propertyComboBox.setItems(context.getFilterSubject().filterableProperties());
		this.propertyComboBox.addValueChangeListener(event -> {
			propertySelectionChanged();
			filterChangeHandler.run();
		});

		this.operatorComboBox = createOperatorComboBox();
		/*
		 * Workaround for
		 * https://github.com/vaadin/vaadin-combo-box-flow/issues/169
		 */
		this.operatorComboBox.setItems(Collections.emptyList());
		this.operatorComboBox.addValueChangeListener(event -> {
			operatorSelectionChanged();
			filterChangeHandler.run();
		});
		
		this.filterValueChangeListener = event -> filterChangeHandler.run();

		add(this.propertyComboBox, this.operatorComboBox);

	}

	/*
	 * Creates the Property ComboBox
	 *
	 *
	 * @setItemLabelGenerator = give the Items a correct name instead of package-name
	 *
	 * @setAllowCustomValue = allows a custom value or not
	 *
	 * @setPlaceholder = sets a Placeholder
	 *
	 * @addClassName = give the object a classname
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
	
	/*
	 * Creates the Operator ComboBox
	 *
	 * @setItemLabelGenerator = give the Items a correct name instead of package-name
	 *
	 * @setAllowCustomValue = allows a custom value or not
	 *
	 * @setPlaceholder = sets a Placeholder
	 *
	 * @setVisible = set if the object is visible or not
	 *
	 * @addClassName = give the object a classname
	 *
	 */
	protected ComboBox<FilterOperator> createOperatorComboBox()
	{
		final ComboBox<FilterOperator> combo = new ComboBox<>();
		combo.setItemLabelGenerator(FilterOperator::name);
		combo.setAllowCustomValue(false);
		combo.setPlaceholder(getComboBoxPlaceholder());
		combo.setVisible(true);
		combo.addClassName("operatorComboBox");
		return combo;
	}
	
	/*
	 * Sets sth
	 *
	 * @data = a FilterEntry object
	 *
	 * @property = List of FilterPropertys stored in the @context identified by the @data
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

	protected String getComboBoxPlaceholder()
	{
		return StringResourceUtils.getResourceString("selectOption", this);
	}

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

	@SuppressWarnings("unchecked")
	protected void operatorSelectionChanged()
	{
		final List<Object> values = valueEditorsToList();

		this.selectedOperator = this.operatorComboBox.getValue();
		if(this.selectedOperator != null)
		{
			this.valueEditors = this.selectedOperator.createComposites(this.context,
				this.selectedProperty);

			for(int i = 0, c = Math.min(values.size(), this.valueEditors.size()); i < c; i++)
			{
				this.valueEditors.get(i).setValue(values.get(i));
			}
			this.valueEditorRegistrations = this.valueEditors.stream().map(editor -> {
				final HasValueAndElement<?, ?> component = editor.component();
				add((Component)component);
				return component.addValueChangeListener(this.filterValueChangeListener);
			}).collect(toList());
		}
	}

	protected List<Object> valueEditorsToList()
	{
		final List<Object> list = new ArrayList<>();

		if(this.valueEditors != null)
		{
			for(final FilterValueEditorComposite valueEditor : this.valueEditors)
			{
				list.add(valueEditor.getValue());
			}
			removeValueEditors();
		}

		return list;
	}
	
	/*
	 * Removes all entries from the valueEditor
	 */
	protected void removeValueEditors()
	{

		if(this.valueEditors != null)
		{
			for(final FilterValueEditorComposite valueEditor : this.valueEditors)
			{
				remove((Component)valueEditor.component());
			}
			this.valueEditors = null;
		}

		if(this.valueEditorRegistrations != null)
		{
			this.valueEditorRegistrations.forEach(Registration::remove);
			this.valueEditorRegistrations = null;
		}

	}

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
}
