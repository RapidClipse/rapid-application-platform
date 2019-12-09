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
package com.rapidclipse.framework.server.ui;

import java.util.Objects;

import com.vaadin.flow.component.ItemLabelGenerator;


/**
 * @author XDEV Software
 *
 */
public final class ItemLabelGeneratorFactory
{
	public static <T> ItemLabelGenerator<T> NonNull(final ItemLabelGenerator<T> delegate)
	{
		return WithDefaultValue(delegate, "");
	}

	public static <T> ItemLabelGenerator<T>
		WithDefaultValue(final ItemLabelGenerator<T> delegate, final String defaultValue)
	{
		Objects.requireNonNull(defaultValue);
		
		return value -> {
			final String label = delegate.apply(value);
			return label != null ? label : defaultValue;
		};
	}
	
	private ItemLabelGeneratorFactory()
	{
		throw new Error();
	}
}
