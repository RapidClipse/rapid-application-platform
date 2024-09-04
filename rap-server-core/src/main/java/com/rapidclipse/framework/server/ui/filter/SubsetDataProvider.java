/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
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
		return (comboBox, context, property) -> comboBox.setItems(listDataProvider);
	}
	
	public static <T> SubsetDataProvider<T> New(
		final ListDataProvider<T> listDataProvider,
		final ItemLabelGenerator<T> itemLabelGenerator)
	{
		return (comboBox, context, property) -> {
			comboBox.setItemLabelGenerator(itemLabelGenerator);
			comboBox.setItems(listDataProvider);
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
		return (comboBox, context, property) -> comboBox.setItems(itemFilter, listDataProvider);
	}
	
	public static <T> SubsetDataProvider<T> New(
		final ItemFilter<T> itemFilter,
		final ListDataProvider<T> listDataProvider,
		final ItemLabelGenerator<T> itemLabelGenerator)
	{
		return (comboBox, context, property) -> {
			comboBox.setItemLabelGenerator(itemLabelGenerator);
			comboBox.setItems(itemFilter, listDataProvider);
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
		return (comboBox, context, property) -> comboBox.setItems(dataProvider);
	}
	
	public static <T, C> SubsetDataProvider<T> New(
		final DataProvider<T, String> dataProvider,
		final ItemLabelGenerator<T> itemLabelGenerator)
	{
		return (comboBox, context, property) -> {
			comboBox.setItemLabelGenerator(itemLabelGenerator);
			comboBox.setItems(dataProvider);
		};
	}
}
