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
import com.vaadin.flow.component.grid.Grid;
import com.vaadin.flow.data.binder.Binder;
import com.vaadin.flow.data.selection.SingleSelect;
import com.vaadin.flow.shared.Registration;


/**
 * @author XDEV Software
 *
 */
public final class ComponentConnector
{
	public static <T> Registration connect(final Grid<T> master, final Binder<? super T> detail)
	{
		return connect(master.asSingleSelect(), detail);
	}

	public static <C extends Component, T> Registration connect(
		final SingleSelect<C, T> master,
		final Binder<? super T> detail)
	{
		return master.addValueChangeListener(event -> detail.setBean(event.getValue()));
	}

	public static <CD extends Component, T> Registration connect(
		final Grid<T> master,
		final SingleSelect<CD, ? super T> detail)
	{
		return connect(master.asSingleSelect(), detail);
	}

	public static <CM extends Component, T> Registration connect(
		final SingleSelect<CM, T> master,
		final Grid<? super T> detail)
	{
		return connect(master, detail.asSingleSelect());
	}

	public static <T> Registration connect(final Grid<T> master, final Grid<? super T> detail)
	{
		return connect(master.asSingleSelect(), detail.asSingleSelect());
	}

	public static <CM extends Component, CD extends Component, T> Registration connect(
		final SingleSelect<CM, T> master,
		final SingleSelect<CD, ? super T> detail)
	{
		return master.addValueChangeListener(event -> detail.setValue(event.getValue()));
	}

	private ComponentConnector()
	{
		throw new Error();
	}
}
