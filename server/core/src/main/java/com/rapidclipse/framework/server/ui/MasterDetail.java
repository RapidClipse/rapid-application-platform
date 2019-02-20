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

import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.grid.Grid;
import com.vaadin.flow.data.binder.Binder;
import com.vaadin.flow.data.selection.SingleSelect;
import com.vaadin.flow.shared.Registration;


/**
 * @author XDEV Software
 *
 */
public final class MasterDetail
{
	public static <T> Registration bind(final Grid<T> master, final Binder<? super T> detail)
	{
		return bind(master.asSingleSelect(), detail);
	}
	
	public static <C extends Component, T> Registration bind(
		final SingleSelect<C, T> master,
		final Binder<? super T> detail)
	{
		return master.addValueChangeListener(event -> detail.setBean(event.getValue()));
	}
	
	public static <CD extends Component, T> Registration bind(
		final Grid<T> master,
		final SingleSelect<CD, ? super T> detail)
	{
		return bind(master.asSingleSelect(), detail);
	}
	
	public static <CM extends Component, T> Registration bind(
		final SingleSelect<CM, T> master,
		final Grid<? super T> detail)
	{
		return bind(master, detail.asSingleSelect());
	}
	
	public static <T> Registration bind(final Grid<T> master, final Grid<? super T> detail)
	{
		return bind(master.asSingleSelect(), detail.asSingleSelect());
	}
	
	public static <CM extends Component, CD extends Component, T> Registration bind(
		final SingleSelect<CM, T> master,
		final SingleSelect<CD, ? super T> detail)
	{
		return master.addValueChangeListener(event -> detail.setValue(event.getValue()));
	}
	
	private MasterDetail()
	{
		throw new Error();
	}
}
