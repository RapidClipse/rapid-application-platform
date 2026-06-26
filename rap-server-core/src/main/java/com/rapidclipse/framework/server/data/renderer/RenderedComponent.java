/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
 */
package com.rapidclipse.framework.server.data.renderer;

import java.io.Serializable;

import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.HasElement;
import com.vaadin.flow.data.renderer.ComponentRenderer;
import com.vaadin.flow.data.renderer.Renderer;
import com.vaadin.flow.function.SerializableBiConsumer;
import com.vaadin.flow.function.SerializableBiFunction;
import com.vaadin.flow.function.SerializableFunction;
import com.vaadin.flow.function.SerializableSupplier;


/**
 * @author XDEV Software
 *
 */
public interface RenderedComponent<T> extends HasElement, Serializable
{
	/*
	 * 
	 * Unable to use intersection type due to java compiler bug, see
	 * https://stackoverflow.com/a/34160332
	 * TODO: 2024-03-12 Check & rework, this may be fixed with java17+ 
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
		return new RenderedComponentRenderer<>(componentSupplier,
			(component, value) -> ((RenderedComponent<SOURCE>)component).renderComponent(value));
	}
	
	public void renderComponent(T value);
	
	/**
	 *
	 * @author XDEV Software
	 * @since 10.02.00
	 *
	 * @param <COMPONENT>
	 * @param <SOURCE>
	 */
	public static class RenderedComponentRenderer<COMPONENT extends Component, SOURCE>
		extends ComponentRenderer<COMPONENT, SOURCE>
	{
		public RenderedComponentRenderer(
			final SerializableFunction<SOURCE, COMPONENT> componentFunction,
			final SerializableBiFunction<Component, SOURCE, Component> componentUpdateFunction)
		{
			super(componentFunction, componentUpdateFunction);
		}
		
		public RenderedComponentRenderer(final SerializableFunction<SOURCE, COMPONENT> componentFunction)
		{
			super(componentFunction);
		}
		
		public RenderedComponentRenderer(
			final SerializableSupplier<COMPONENT> componentSupplier,
			final SerializableBiConsumer<COMPONENT, SOURCE> itemConsumer)
		{
			super(componentSupplier, itemConsumer);
		}
		
		public RenderedComponentRenderer(final SerializableSupplier<COMPONENT> componentSupplier)
		{
			super(componentSupplier);
		}
		
	}
	
}
