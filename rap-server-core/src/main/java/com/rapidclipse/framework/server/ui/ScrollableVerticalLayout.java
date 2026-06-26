/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
 */
package com.rapidclipse.framework.server.ui;

import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.Composite;
import com.vaadin.flow.component.HasOrderedComponents;
import com.vaadin.flow.component.HasSize;
import com.vaadin.flow.component.HasStyle;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;


/**
 * @author XDEV Software
 *
 */
public class ScrollableVerticalLayout extends Composite<VerticalLayout>
	implements HasOrderedComponents, HasStyle, HasSize
{
	public ScrollableVerticalLayout()
	{
		super();

		getStyle().set("overflow", "auto");

		final VerticalLayout content = getContent();
		content.setWidth("100%");
		content.setHeight(null);
		content.setPadding(false);
	}

	public ScrollableVerticalLayout(final Component... components)
	{
		this();

		add(components);
	}

	@Override
	public void add(final Component... components)
	{
		getContent().add(components);
	}

	@Override
	public void remove(final Component... components)
	{
		getContent().remove(components);
	}

	@Override
	public void removeAll()
	{
		getContent().removeAll();
	}

	@Override
	public void addComponentAtIndex(final int index, final Component component)
	{
		getContent().addComponentAtIndex(index, component);
	}

	@Override
	public void addComponentAsFirst(final Component component)
	{
		getContent().addComponentAsFirst(component);
	}

	@Override
	public void replace(final Component oldComponent, final Component newComponent)
	{
		getContent().replace(oldComponent, newComponent);
	}

	@Override
	public int indexOf(final Component component)
	{
		return getContent().indexOf(component);
	}

	@Override
	public int getComponentCount()
	{
		return getContent().getComponentCount();
	}

	@Override
	public Component getComponentAt(final int index)
	{
		return getContent().getComponentAt(index);
	}
}
