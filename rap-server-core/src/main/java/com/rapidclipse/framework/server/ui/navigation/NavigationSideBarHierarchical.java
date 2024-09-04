/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
 */
package com.rapidclipse.framework.server.ui.navigation;

import com.vaadin.flow.component.orderedlayout.VerticalLayout;


/**
 * @author XDEV Software
 *
 */
public class NavigationSideBarHierarchical extends NavigationCompositeHierarchical<VerticalLayout>
{
	public NavigationSideBarHierarchical()
	{
		super();
		
		getContent().setSpacing(false);
	}
}
