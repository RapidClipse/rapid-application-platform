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
			if(value == null)
			{
				return defaultValue;
			}
			final String label = delegate.apply(value);
			return label != null ? label : defaultValue;
		};
	}

	private ItemLabelGeneratorFactory()
	{
		throw new Error();
	}
}
