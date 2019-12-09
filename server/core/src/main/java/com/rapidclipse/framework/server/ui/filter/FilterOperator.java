/*
 * Copyright (C) 2013-2019 by XDEV Software, All Rights Reserved.
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

import java.io.Serializable;
import java.time.LocalDate;
import java.time.temporal.Temporal;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Objects;

import com.rapidclipse.framework.server.Rap;
import com.rapidclipse.framework.server.data.converter.ConverterBuilder;
import com.rapidclipse.framework.server.data.converter.LocalDateToDateConverter;
import com.rapidclipse.framework.server.data.converter.LocalDateToTemporalConverter;
import com.rapidclipse.framework.server.data.filter.Filter;
import com.rapidclipse.framework.server.resources.StringResourceUtils;
import com.rapidclipse.framework.server.ui.TextFieldWithNull;
import com.vaadin.flow.component.checkbox.Checkbox;
import com.vaadin.flow.component.combobox.ComboBox;
import com.vaadin.flow.component.datepicker.DatePicker;
import com.vaadin.flow.component.textfield.TextField;
import com.vaadin.flow.data.value.ValueChangeMode;


/**
 * @author XDEV Software
 *
 */
@SuppressWarnings("rawtypes")
public interface FilterOperator extends Serializable
{
	public String key();

	public String name();

	public boolean isSupported(final FilterProperty property);

	public List<FilterValueEditorComposite> createComposites(
		FilterContext context,
		FilterProperty property);

	public Filter createFilter(
		FilterContext context,
		FilterProperty property,
		List<FilterValueEditorComposite> composites);

	public static abstract class Abstract implements FilterOperator
	{
		protected final String key;

		public Abstract(final String key)
		{
			this.key = key;
		}

		@Override
		public String key()
		{
			return this.key;
		}

		@Override
		public String name()
		{
			return StringResourceUtils.getResourceString("Operator." + key, FilterOperator.class);
		}

		protected boolean isNumber(final Class<?> type)
		{
			return Number.class.isAssignableFrom(type) || type == int.class || type == double.class
				|| type == float.class || type == long.class || type == short.class
				|| type == byte.class;
		}

		protected boolean isTemporal(final Class<?> type)
		{
			return Temporal.class.isAssignableFrom(type);
		}

		protected boolean isDate(final Class<?> type)
		{
			return Date.class.isAssignableFrom(type);
		}

		protected boolean isBoolean(final Class<?> type)
		{
			return type == Boolean.class || type == boolean.class;
		}

		protected FilterValueEditorComposite<String, String> createStringField()
		{
			final TextField textField = new TextFieldWithNull();
			textField.setValueChangeMode(ValueChangeMode.EAGER);
			return FilterValueEditorComposite.New(textField);
		}

		protected <MODEL extends Number> FilterValueEditorComposite<String, MODEL> createNumberField(
			final Class<MODEL> numberType)
		{
			final TextField textField = new TextFieldWithNull();
			textField.setValueChangeMode(ValueChangeMode.EAGER);
			return FilterValueEditorComposite.New(textField,
				ConverterBuilder.StringToNumber(numberType).build());
		}

		/**
		 * XXX There is only {@link DatePicker} at the moment, we have to wait
		 * for Vaaadin 14 for more controls.
		 */
		protected <MODEL extends Temporal> FilterValueEditorComposite<LocalDate, MODEL> createTemporalField(
			final Class<MODEL> dateType)
		{
			final DatePicker datePicker = new DatePicker();
			return FilterValueEditorComposite.New(datePicker,
				LocalDateToTemporalConverter.New(dateType));
		}

		protected <MODEL extends Date> FilterValueEditorComposite<LocalDate, MODEL> createDateField(
			final Class<MODEL> dateType)
		{
			final DatePicker datePicker = new DatePicker();
			return FilterValueEditorComposite.New(datePicker,
				LocalDateToDateConverter.New(dateType));
		}

		protected FilterValueEditorComposite<Boolean, Boolean> createBooleanField()
		{
			final Checkbox checkbox = new Checkbox();
			return FilterValueEditorComposite.New(checkbox);
		}

		@SuppressWarnings("unchecked")
		protected <T> FilterValueEditorComposite createChoiceField(
			final FilterContext context,
			final FilterProperty property)
		{
			final SubsetDataProvider<T> subsetDataProvider = (SubsetDataProvider<T>)context
				.getSubsetDataProviderFactoryRegistry().getAll().stream()
				.map(factory -> factory.createFor(context, property)).filter(Objects::nonNull)
				.findFirst().orElse(SubsetDataProvider.Empty());

			final ComboBox<T> combo = new ComboBox<>();
			subsetDataProvider.configure(combo, context, property);
			return FilterValueEditorComposite.New(combo);
		}
	}

	public static abstract class AbstractString extends Abstract
	{
		public AbstractString(final String key)
		{
			super(key);
		}

		@Override
		public boolean isSupported(final FilterProperty property)
		{
			return property.type() == String.class;
		}

		@Override
		public List<FilterValueEditorComposite> createComposites(
			final FilterContext context,
			final FilterProperty property)
		{
			return Arrays.asList(createStringField());
		}

		@Override
		public Filter createFilter(
			final FilterContext context,
			final FilterProperty property,
			final List<FilterValueEditorComposite> composites)
		{
			String value = (String)composites.get(0).getValue();
			if(value != null && (value = value.trim()).length() > 0)
			{
				return createStringFilter(value, context, property);
			}

			return null;
		}

		protected abstract Filter createStringFilter(
			String value,
			FilterContext context,
			FilterProperty property);
	}

	public static class Equals extends AbstractString
	{
		public final static String KEY = "EQUALS";

		public Equals()
		{
			super(KEY);
		}

		@Override
		protected Filter createStringFilter(
			final String value,
			final FilterContext context,
			final FilterProperty property)
		{
			final char    wildcard      = context.getWildcard();
			final boolean caseSensitive = context.isCaseSensitive();

			if(value.indexOf(wildcard) != -1 || !caseSensitive)
			{
				return Filter.StringComparison(property.identifier(), value, caseSensitive,
					Arrays.asList(wildcard));
			}

			return Filter.Equals(property.identifier(), value);
		}
	}

	public static class StartsWith extends AbstractString
	{
		public final static String KEY = "STARTS_WITH";

		public StartsWith()
		{
			super(KEY);
		}

		@Override
		protected Filter createStringFilter(
			String value,
			final FilterContext context,
			final FilterProperty property)
		{
			final char wildcard = context.getWildcard();

			if(value.length() > 0 && value.charAt(value.length() - 1) != wildcard)
			{
				value += wildcard;
			}

			return Filter.StringComparison(property.identifier(), value, context.isCaseSensitive(),
				Arrays.asList(wildcard));
		}
	}

	public static class Contains extends AbstractString
	{
		public final static String KEY = "CONTAINS";

		public Contains()
		{
			super(KEY);
		}

		@Override
		protected Filter createStringFilter(
			String value,
			final FilterContext context,
			final FilterProperty property)
		{
			final char wildcard = context.getWildcard();

			if(value.length() > 0)
			{
				if(value.charAt(0) != wildcard)
				{
					value = wildcard + value;
				}
				if(value.charAt(value.length() - 1) != wildcard)
				{
					value += wildcard;
				}
			}

			return Filter.StringComparison(property.identifier(), value, context.isCaseSensitive(),
				Arrays.asList(wildcard));
		}
	}

	public static class Is extends Abstract
	{
		public final static String KEY = "IS";

		public Is()
		{
			this(KEY);
		}

		protected Is(final String key)
		{
			super(key);
		}

		@Override
		public boolean isSupported(final FilterProperty property)
		{
			return property.type() != String.class;
		}

		@Override
		public List<FilterValueEditorComposite> createComposites(
			final FilterContext context,
			final FilterProperty property)
		{
			final Class<?> propertyType = property.type();

			final FilterValueEditorComposite composite;

			if(isNumber(propertyType))
			{
				composite = createNumberField(
					Rap.wrapperTypeIfPrimitive(propertyType).asSubclass(Number.class));
			}
			else if(isTemporal(propertyType))
			{
				composite = createTemporalField(propertyType.asSubclass(Temporal.class));
			}
			else if(isDate(propertyType))
			{
				composite = createDateField(propertyType.asSubclass(Date.class));
			}
			else if(isBoolean(propertyType))
			{
				composite = createBooleanField();
			}
			else
			{
				composite = createChoiceField(context, property);
			}

			return Arrays.asList(composite);
		}

		@Override
		public Filter createFilter(
			final FilterContext context,
			final FilterProperty property,
			final List<FilterValueEditorComposite> composites)
		{
			final Object value = composites.get(0).getValue();
			if(value != null)
			{
				return Filter.Equals(property.identifier(), value);
			}

			return null;
		}
	}

	public static class IsNot extends Is
	{
		public final static String KEY = "IS_NOT";

		public IsNot()
		{
			super(KEY);
		}

		@Override
		public Filter createFilter(
			final FilterContext context,
			final FilterProperty property,
			final List<FilterValueEditorComposite> composites)
		{
			final Filter filter = super.createFilter(context, property, composites);
			return filter != null ? Filter.Not(filter) : null;
		}
	}

	public static abstract class AbstractSizeComparing extends Abstract
	{
		public AbstractSizeComparing(final String key)
		{
			super(key);
		}

		@Override
		public boolean isSupported(final FilterProperty property)
		{
			final Class<?> type = property.type();
			return isNumber(type) || isTemporal(type) || isDate(type);
		}

		@Override
		public List<FilterValueEditorComposite> createComposites(
			final FilterContext context,
			final FilterProperty property)
		{
			final Class<?> propertyType = property.type();

			final FilterValueEditorComposite composite;

			if(isNumber(propertyType))
			{
				composite = createNumberField(
					Rap.wrapperTypeIfPrimitive(propertyType).asSubclass(Number.class));
			}
			else if(isTemporal(propertyType))
			{
				composite = createTemporalField(propertyType.asSubclass(Temporal.class));
			}
			else
			{
				composite = createDateField(propertyType.asSubclass(Date.class));
			}

			return Arrays.asList(composite);
		}

		@Override
		public Filter createFilter(
			final FilterContext context,
			final FilterProperty property,
			final List<FilterValueEditorComposite> composites)
		{
			final Comparable<?> value = (Comparable<?>)composites.get(0).getValue();
			if(value != null)
			{
				return createFilter(property, value);
			}

			return null;
		}

		protected abstract Filter createFilter(final FilterProperty property, Comparable<?> value);
	}

	public static class Greater extends AbstractSizeComparing
	{
		public final static String KEY = "GREATER";

		public Greater()
		{
			super(KEY);
		}

		@Override
		protected Filter createFilter(final FilterProperty property, final Comparable<?> value)
		{
			return Filter.Greater(property.identifier(), value);
		}
	}

	public static class Less extends AbstractSizeComparing
	{
		public final static String KEY = "LESS";

		public Less()
		{
			super(KEY);
		}

		@Override
		protected Filter createFilter(final FilterProperty property, final Comparable<?> value)
		{
			return Filter.Less(property.identifier(), value);
		}
	}

	public static class GreaterEqual extends AbstractSizeComparing
	{
		public final static String KEY = "GREATER_EQUAL";

		public GreaterEqual()
		{
			super(KEY);
		}

		@Override
		protected Filter createFilter(final FilterProperty property, final Comparable<?> value)
		{
			return Filter.GreaterEquals(property.identifier(), value);
		}
	}

	public static class LessEqual extends AbstractSizeComparing
	{
		public final static String KEY = "LESS_EQUAL";

		public LessEqual()
		{
			super(KEY);
		}

		@Override
		protected Filter createFilter(final FilterProperty property, final Comparable<?> value)
		{
			return Filter.LessEquals(property.identifier(), value);
		}
	}

	public static class Between extends Abstract
	{
		public final static String KEY = "BETWEEN";

		public Between()
		{
			super(KEY);
		}

		@Override
		public boolean isSupported(final FilterProperty property)
		{
			final Class<?> type = property.type();
			return isNumber(type) || isTemporal(type) || isDate(type);
		}

		@Override
		public List<FilterValueEditorComposite> createComposites(
			final FilterContext context,
			final FilterProperty property)
		{
			final FilterValueEditorComposite start = createComposite(property.type());
			final FilterValueEditorComposite end   = createComposite(property.type());
			return Arrays.asList(start, end);
		}

		protected FilterValueEditorComposite createComposite(final Class<?> propertyType)
		{
			final FilterValueEditorComposite composite;

			if(isNumber(propertyType))
			{
				composite = createNumberField(
					Rap.wrapperTypeIfPrimitive(propertyType).asSubclass(Number.class));
			}
			else if(isTemporal(propertyType))
			{
				composite = createTemporalField(propertyType.asSubclass(Temporal.class));
			}
			else
			{
				composite = createDateField(propertyType.asSubclass(Date.class));
			}

			return composite;
		}

		@Override
		public Filter createFilter(
			final FilterContext context,
			final FilterProperty property,
			final List<FilterValueEditorComposite> composites)
		{
			final Comparable<?> start = (Comparable<?>)composites.get(0).getValue();
			final Comparable<?> end   = (Comparable<?>)composites.get(1).getValue();
			if(start != null && end != null)
			{
				return Filter.Between(property.identifier(), start, end);
			}

			return null;
		}
	}

	public static class IsEmpty extends Abstract
	{
		public final static String KEY = "IS_EMPTY";

		public IsEmpty()
		{
			this(KEY);
		}

		protected IsEmpty(final String key)
		{
			super(key);
		}

		@Override
		public boolean isSupported(final FilterProperty property)
		{
			return true;
		}

		@Override
		public List<FilterValueEditorComposite> createComposites(
			final FilterContext context,
			final FilterProperty property)
		{
			return Collections.emptyList();
		}

		@Override
		public Filter createFilter(
			final FilterContext context,
			final FilterProperty property,
			final List<FilterValueEditorComposite> composites)
		{
			Filter filter = Filter.Equals(property.identifier(), null);

			if(property.type() == String.class)
			{
				filter = Filter.Or(filter, Filter.Equals(property.identifier(), ""));
			}

			return filter;
		}
	}

	public static class IsNotEmpty extends IsEmpty
	{
		public final static String KEY = "IS_NOT_EMPTY";

		public IsNotEmpty()
		{
			super(KEY);
		}

		@Override
		public Filter createFilter(
			final FilterContext context,
			final FilterProperty property,
			final List<FilterValueEditorComposite> composites)
		{
			return Filter.Not(super.createFilter(context, property, composites));
		}
	}
}
