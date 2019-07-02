/*
 * Copyright (C) 2013-2019 by XDEV Software, All Rights Reserved.
 *
 * This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License 2.0
 * which accompanies this distribution, and is available at
 * https://www.eclipse.org/legal/epl-2.0/
 *
 * SPDX-License-Identifier: EPL-2.0
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
