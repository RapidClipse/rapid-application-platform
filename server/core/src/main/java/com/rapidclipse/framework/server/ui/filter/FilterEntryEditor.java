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
	private final FilterComponent                          filterComponent;
	private final int                                      rowIndex;
	
	public FilterEntryEditor(
		final FilterComponent filterComponent,
		final FilterContext context,
		final Runnable filterChangeHandler)
	{
		super();
		this.rowIndex        = filterComponent.getIndex();
		this.filterComponent = filterComponent;
		this.context         = context;
		this.setWidthFull();
		this.propertyComboBox = createPropertyComboBox();
		this.propertyComboBox.setItems(context.getFilterSubject().filterableProperties());
		this.propertyComboBox.addValueChangeListener(event -> {
			this.filterComponent.updateFilterDivLabel(this.rowIndex, this.propertyComboBox,
				this.valueEditors);
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
			this.filterComponent.updateFilterDivLabel(this.rowIndex, this.operatorComboBox,
				this.valueEditors);
			filterChangeHandler.run();
		});

		this.filterValueChangeListener = event -> filterChangeHandler.run();
		this.selectedOperator          = this.operatorComboBox.getValue();
		this.selectedProperty          = this.propertyComboBox.getValue();

		// add the ComboBoxes to the FormLayout
		add(this.propertyComboBox, this.operatorComboBox);
	}
	
	protected ComboBox<FilterProperty<?>> createPropertyComboBox()
	{
		final ComboBox<FilterProperty<?>> combo = new ComboBox<>();
		combo.setItemLabelGenerator(FilterProperty::caption);
		combo.setAllowCustomValue(false);
		combo.setPlaceholder(getComboBoxPlaceholder());
		combo.addClassName("propertyComboBox");
		return combo;
	}

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

			/*
			 * Mal sehen ob das hier klappt
			 */
			for(final FilterValueEditorComposite obj : this.valueEditors)
			{
				obj.component().addValueChangeListener(listener -> {
					this.filterComponent.updateFilterDivLabel(this.rowIndex, this.operatorComboBox,
						this.valueEditors);
				});
			}
			
			this.valueEditorRegistrations = this.valueEditors.stream().map(editor -> {
				final HasValueAndElement<?, ?> component = editor.component();
				add((Component)component);
				return component.addValueChangeListener(this.filterValueChangeListener);
			}).collect(toList());
		}
	}
	
	/**
	 * Removes the last ValueEditors and returns a list of the removed Objects
	 *
	 * @return <b> lastValues </b> -> List of the removed Objects
	 */
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

	public FilterProperty<?> getSelectedProperty()
	{
		return this.selectedProperty;
	}

	public FilterOperator getSelectedOperator()
	{
		return this.selectedOperator;
	}

	public List<FilterValueEditorComposite> getValueEditors()
	{
		return this.valueEditors;
	}

	public void setValueEditors(final List<FilterValueEditorComposite> valueEditors)
	{
		this.valueEditors = valueEditors;
	}

}
