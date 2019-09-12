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
package com.rapidclipse.framework.server.data.renderer;

import java.io.Serializable;

import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.HasElement;
import com.vaadin.flow.data.renderer.ComponentRenderer;
import com.vaadin.flow.data.renderer.Renderer;
import com.vaadin.flow.function.SerializableSupplier;


/**
 * @author XDEV Software
 *
 */
public interface RenderedComponent<T> extends HasElement, Serializable
{
	/*
	 * Unable to use intersection type due to java compiler bug, see
	 * https://stackoverflow.com/a/34160332
	 */
	// public static <COMPONENT extends Component & RenderedComponent<SOURCE>, SOURCE> Renderer<SOURCE>
	// Renderer(final SerializableSupplier<COMPONENT> componentSupplier)
	// {
	// return new ComponentRenderer<>(componentSupplier, RenderedComponent::renderComponent);
	// }

	@SuppressWarnings("unchecked")
	public static <COMPONENT extends Component, SOURCE> Renderer<SOURCE>
		Renderer(final SerializableSupplier<COMPONENT> componentSupplier)
	{
		return new ComponentRenderer<>(componentSupplier,
			(component, value) -> ((RenderedComponent<SOURCE>)component).renderComponent(value));
	}

	public void renderComponent(T value);
}
