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
import java.util.Collection;
import java.util.Collections;

import com.vaadin.flow.component.ItemLabelGenerator;
import com.vaadin.flow.component.combobox.ComboBox;
import com.vaadin.flow.component.combobox.ComboBox.FetchItemsCallback;
import com.vaadin.flow.component.combobox.ComboBox.ItemFilter;
import com.vaadin.flow.data.provider.DataProvider;
import com.vaadin.flow.data.provider.ListDataProvider;
import com.vaadin.flow.function.SerializableFunction;


/**
 * @author XDEV Software
 *
 */
public interface SubsetDataProvider<T> extends Serializable
{
	public void configure(
		ComboBox<T> comboBox,
		final FilterContext context,
		final FilterProperty<T> property);
	
	public static <T> SubsetDataProvider<T> Empty()
	{
		return (comboBox, context, property) -> comboBox.setItems(Collections.emptyList());
	}
	
	public static <T> SubsetDataProvider<T> New(final Collection<T> items)
	{
		return (comboBox, context, property) -> comboBox.setItems(items);
	}
	
	public static <T> SubsetDataProvider<T> New(
		final Collection<T> items,
		final ItemLabelGenerator<T> itemLabelGenerator)
	{
		return (comboBox, context, property) -> {
			comboBox.setItemLabelGenerator(itemLabelGenerator);
			comboBox.setItems(items);
		};
	}
	
	public static <T> SubsetDataProvider<T> New(final ListDataProvider<T> listDataProvider)
	{
		return (comboBox, context, property) -> comboBox.setDataProvider(listDataProvider);
	}
	
	public static <T> SubsetDataProvider<T> New(
		final ListDataProvider<T> listDataProvider,
		final ItemLabelGenerator<T> itemLabelGenerator)
	{
		return (comboBox, context, property) -> {
			comboBox.setItemLabelGenerator(itemLabelGenerator);
			comboBox.setDataProvider(listDataProvider);
		};
	}
	
	public static <T> SubsetDataProvider<T> New(
		final FetchItemsCallback<T> fetchItems,
		final SerializableFunction<String, Integer> sizeCallback)
	{
		return (comboBox, context, property) -> comboBox.setDataProvider(fetchItems, sizeCallback);
	}
	
	public static <T> SubsetDataProvider<T> New(
		final FetchItemsCallback<T> fetchItems,
		final SerializableFunction<String, Integer> sizeCallback,
		final ItemLabelGenerator<T> itemLabelGenerator)
	{
		return (comboBox, context, property) -> {
			comboBox.setItemLabelGenerator(itemLabelGenerator);
			comboBox.setDataProvider(fetchItems, sizeCallback);
		};
	}
	
	public static <T> SubsetDataProvider<T> New(
		final ItemFilter<T> itemFilter,
		final Collection<T> items)
	{
		return (comboBox, context, property) -> comboBox.setItems(itemFilter, items);
	}
	
	public static <T> SubsetDataProvider<T> New(
		final ItemFilter<T> itemFilter,
		final Collection<T> items,
		final ItemLabelGenerator<T> itemLabelGenerator)
	{
		return (comboBox, context, property) -> {
			comboBox.setItemLabelGenerator(itemLabelGenerator);
			comboBox.setItems(itemFilter, items);
		};
	}
	
	public static <T> SubsetDataProvider<T> New(
		final ItemFilter<T> itemFilter,
		final ListDataProvider<T> listDataProvider)
	{
		return (comboBox, context, property) -> comboBox.setDataProvider(itemFilter,
			listDataProvider);
	}
	
	public static <T> SubsetDataProvider<T> New(
		final ItemFilter<T> itemFilter,
		final ListDataProvider<T> listDataProvider,
		final ItemLabelGenerator<T> itemLabelGenerator)
	{
		return (comboBox, context, property) -> {
			comboBox.setItemLabelGenerator(itemLabelGenerator);
			comboBox.setDataProvider(itemFilter, listDataProvider);
		};
	}
	
	public static <T, C> SubsetDataProvider<T> New(
		final DataProvider<T, C> dataProvider,
		final SerializableFunction<String, C> filterConverter)
	{
		return (comboBox, context, property) -> comboBox.setDataProvider(dataProvider,
			filterConverter);
	}
	
	public static <T, C> SubsetDataProvider<T> New(
		final DataProvider<T, C> dataProvider,
		final SerializableFunction<String, C> filterConverter,
		final ItemLabelGenerator<T> itemLabelGenerator)
	{
		return (comboBox, context, property) -> {
			comboBox.setItemLabelGenerator(itemLabelGenerator);
			comboBox.setDataProvider(dataProvider, filterConverter);
		};
	}
	
	public static <T, C> SubsetDataProvider<T> New(final DataProvider<T, String> dataProvider)
	{
		return (comboBox, context, property) -> comboBox.setDataProvider(dataProvider);
	}
	
	public static <T, C> SubsetDataProvider<T> New(
		final DataProvider<T, String> dataProvider,
		final ItemLabelGenerator<T> itemLabelGenerator)
	{
		return (comboBox, context, property) -> {
			comboBox.setItemLabelGenerator(itemLabelGenerator);
			comboBox.setDataProvider(dataProvider);
		};
	}
}
