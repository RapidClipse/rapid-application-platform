/*
 * Copyright (C) 2013-2018 by XDEV Software, All Rights Reserved.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 *
 * For further information see
 * <http://www.rapidclipse.com/en/legal/license/license.html>.
 */

package software.xdev.rap.server.ui.filter;


import static java.util.stream.Collectors.toList;

import java.util.ArrayList;
import java.util.List;

import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.HasValue.ValueChangeEvent;
import com.vaadin.flow.component.HasValue.ValueChangeListener;
import com.vaadin.flow.component.combobox.ComboBox;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;
import com.vaadin.flow.shared.Registration;

import software.xdev.rap.server.resources.StringResourceUtils;


/**
 * @author XDEV Software
 *
 */
@SuppressWarnings("rawtypes")
public class FilterEntryEditor extends HorizontalLayout
{
	private final FilterContext								context;

	private final ComboBox<FilterProperty>					propertyComboBox;
	private final ComboBox<FilterOperator>					operatorComboBox;

	private final ValueChangeListener<ValueChangeEvent<?>>	filterValueChangeListener;
	private FilterProperty									selectedProperty;
	private FilterOperator									selectedOperator;
	private List<FilterValueEditorComposite>				valueEditors;
	private List<Registration>								valueEditorRegistrations;


	public FilterEntryEditor(final FilterContext context, final Runnable filterChangeHandler)
	{
		super();

		this.context = context;

		this.propertyComboBox = createPropertyComboBox();
		this.propertyComboBox.setItems();
		this.propertyComboBox.addValueChangeListener(event -> propertySelectionChanged());

		this.operatorComboBox = createOperatorComboBox();
		this.operatorComboBox.addValueChangeListener(event -> operatorSelectionChanged());
		this.operatorComboBox.setVisible(false);

		this.filterValueChangeListener = event -> filterChangeHandler.run();

		add(this.propertyComboBox,this.operatorComboBox);
	}


	public FilterEntryEditor(final FilterContext context, final Runnable filterChangeHandler,
			final FilterEntry filterData)
	{
		this(context,filterChangeHandler);

		setFilterData(filterData);
	}


	@SuppressWarnings("unchecked")
	public void setFilterData(final FilterEntry data)
	{
		final FilterProperty property = this.context.getFilterSubject()
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
					final int i = 0;
					for(final Object value : values)
					{
						this.valueEditors.get(i).setValue(value);
					}
				}
			}
		}
	}


	public FilterEntry getFilterData()
	{
		if(this.selectedProperty == null || this.selectedOperator == null
				|| this.valueEditors == null)
		{
			return null;
		}

		final Object[] values = this.valueEditors.stream().map(FilterValueEditorComposite::getValue)
				.toArray();
		return new FilterEntry(this.selectedProperty.identifier(),this.selectedOperator.key(),
				values);
	}


	protected ComboBox<FilterProperty> createPropertyComboBox()
	{
		final ComboBox<FilterProperty> combo = new ComboBox<>();
		combo.setItemLabelGenerator(FilterProperty::caption);
		combo.setAllowCustomValue(false);
		combo.setPlaceholder(getComboBoxPlaceholder());
		return combo;
	}


	protected ComboBox<FilterOperator> createOperatorComboBox()
	{
		final ComboBox<FilterOperator> combo = new ComboBox<>();
		combo.setItemLabelGenerator(FilterOperator::name);
		combo.setAllowCustomValue(false);
		combo.setPlaceholder(getComboBoxPlaceholder());
		return combo;
	}


	protected String getComboBoxPlaceholder()
	{
		return StringResourceUtils.getResourceString("selectOption",this);
	}


	protected void propertySelectionChanged()
	{
		removeValueEditors();

		this.selectedProperty = this.propertyComboBox.getValue();
		if(this.selectedProperty != null)
		{
			this.operatorComboBox.setItems(this.context.getFilterOperatorRegistry().getAll()
					.stream().filter(op -> op.isSupported(this.selectedProperty)));
			this.operatorComboBox.setVisible(true);
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

			for(int i = 0, c = Math.min(lastValues.size(),this.valueEditors.size()); i < c; i++)
			{
				this.valueEditors.get(i).setValue(lastValues.get(i));
				add((Component)this.valueEditors.get(i).component());
			}

			this.valueEditorRegistrations = this.valueEditors.stream()
					.map(editor -> editor.component()
							.addValueChangeListener(this.filterValueChangeListener))
					.collect(toList());
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
}
