/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
 */
package com.rapidclipse.framework.server.ui;

import java.util.Objects;
import java.util.function.Supplier;

import org.apache.commons.lang3.StringUtils;

import com.vaadin.flow.component.ItemLabelGenerator;
import com.vaadin.flow.component.combobox.ComboBox.ItemFilter;


/**
 * @author XDEV Software
 *
 */
public interface StartsWithIgnoreCaseItemFilter<T> extends ItemFilter<T>
{
	public static <T> StartsWithIgnoreCaseItemFilter<T>
		New(final Supplier<ItemLabelGenerator<T>> itemLabelGeneratorSupplier)
	{
		return new Default<>(itemLabelGeneratorSupplier);
	}
	
	public static class Default<T> implements StartsWithIgnoreCaseItemFilter<T>
	{
		private final Supplier<ItemLabelGenerator<T>> itemLabelGeneratorSupplier;

		protected Default(final Supplier<ItemLabelGenerator<T>> itemLabelGeneratorSupplier)
		{
			super();
			
			this.itemLabelGeneratorSupplier = Objects.requireNonNull(itemLabelGeneratorSupplier);
		}
		
		@Override
		public boolean test(final T item, final String filterText)
		{
			if(item == null || StringUtils.isBlank(filterText))
			{
				return true;
			}

			return StringUtils.startsWithIgnoreCase(this.itemLabelGeneratorSupplier.get().apply(item), filterText);
		}
	}
}
