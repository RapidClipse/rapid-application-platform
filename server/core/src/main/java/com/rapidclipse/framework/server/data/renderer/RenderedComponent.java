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
