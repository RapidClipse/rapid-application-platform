/*
 * Copyright (C) 2013-2020 by XDEV Software, All Rights Reserved.
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
package com.rapidclipse.framework.server.data.renderer;

import java.util.function.Function;

import com.rapidclipse.framework.server.resources.CaptionUtils;
import com.rapidclipse.framework.server.ui.ItemLabelGeneratorFactory;
import com.vaadin.flow.data.renderer.TextRenderer;


/**
 * @author XDEV Software
 * @since 10.02.00
 */
public class CaptionRenderer<ITEM> extends TextRenderer<ITEM>
{
	public <P> CaptionRenderer(final Function<ITEM, P> valueProvider)
	{
		super(ItemLabelGeneratorFactory.NonNull(
			item -> CaptionUtils.resolveCaption(valueProvider.apply(item))));
	}

	public <P> CaptionRenderer(final Function<ITEM, P> valueProvider, final String defaultValue)
	{
		super(ItemLabelGeneratorFactory.WithDefaultValue(
			item -> CaptionUtils.resolveCaption(valueProvider.apply(item)),
			defaultValue));
	}
}
