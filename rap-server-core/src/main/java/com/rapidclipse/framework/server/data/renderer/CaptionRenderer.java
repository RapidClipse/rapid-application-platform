/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
 */
package com.rapidclipse.framework.server.data.renderer;

import com.rapidclipse.framework.server.resources.CaptionUtils;
import com.rapidclipse.framework.server.ui.ItemLabelGeneratorFactory;
import com.vaadin.flow.data.renderer.TextRenderer;
import com.vaadin.flow.function.SerializableFunction;


/**
 * @author XDEV Software
 * @since 10.02.00
 */
public class CaptionRenderer<ITEM> extends TextRenderer<ITEM>
{
	public <P> CaptionRenderer(final SerializableFunction<ITEM, P> valueProvider)
	{
		super(ItemLabelGeneratorFactory.NonNull(
			item -> CaptionUtils.resolveCaption(valueProvider.apply(item))));
	}

	public <P> CaptionRenderer(final SerializableFunction<ITEM, P> valueProvider, final String defaultValue)
	{
		super(ItemLabelGeneratorFactory.WithDefaultValue(
			item -> CaptionUtils.resolveCaption(valueProvider.apply(item)),
			defaultValue));
	}
}
