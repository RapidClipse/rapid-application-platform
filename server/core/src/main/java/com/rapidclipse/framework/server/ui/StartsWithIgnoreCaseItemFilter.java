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
