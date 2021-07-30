/*
 * Copyright (C) 2013-2021 by XDEV Software, All Rights Reserved.
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
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *
 * Contributors:
 *     XDEV Software Corp. - initial API and implementation
 */
package com.rapidclipse.framework.server.reports.grid;

import java.awt.Color;

import net.sf.dynamicreports.report.builder.style.StyleBuilder;
import net.sf.dynamicreports.report.builder.style.Styles;
import net.sf.dynamicreports.report.constant.HorizontalTextAlignment;


/**
 *
 * @author XDEV Software
 * @since 10.01.00
 */
public interface GridReportStyles
{
	public StyleBuilder titleStyle();
	
	public StyleBuilder footerStyle();
	
	public StyleBuilder columnTitleStyle();
	
	public StyleBuilder columnStyle();
	
	public static GridReportStyles New()
	{
		return new Default();
	}

	public static class Default implements GridReportStyles
	{
		protected final StyleBuilder boldStyle       = Styles.style().bold().setPadding(2);
		protected final StyleBuilder boldCenterStyle = Styles.style(this.boldStyle)
			.setHorizontalTextAlignment(HorizontalTextAlignment.CENTER);
		protected final StyleBuilder columnTitle     = Styles.style(this.boldCenterStyle)
			.setBorder(Styles.pen1Point()).setBackgroundColor(Color.LIGHT_GRAY);
		protected final StyleBuilder columnStyle     = Styles.style(this.boldStyle)
			.setBorder(Styles.pen1Point());
		
		Default()
		{
			super();
		}
		
		@Override
		public StyleBuilder titleStyle()
		{
			return this.boldCenterStyle;
		}
		
		@Override
		public StyleBuilder footerStyle()
		{
			return this.boldCenterStyle;
		}
		
		@Override
		public StyleBuilder columnTitleStyle()
		{
			return this.columnTitle;
		}
		
		@Override
		public StyleBuilder columnStyle()
		{
			return this.columnStyle;
		}
	}
}
