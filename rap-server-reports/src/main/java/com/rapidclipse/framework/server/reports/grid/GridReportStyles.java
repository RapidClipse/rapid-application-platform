/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
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
