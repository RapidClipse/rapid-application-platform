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
package com.rapidclipse.framework.server.charts.config;

/**
 *
 * @author XDEV Software
 * @since 10.02.00
 */
public class Tooltip
{
	private boolean   isHtml        = false;
	private String    trigger       = "focus";
	private boolean   ignoreBounds  = false;
	private boolean   showColorCode = false;
	private String    text          = "both";
	private TextStyle textStyle;
	
	public boolean getisHtml()
	{
		return this.isHtml;
	}
	
	/**
	 * If set to true, use HTML-rendered (rather than SVG-rendered) tooltips
	 *
	 * @param isHtml
	 */
	public void setisHtml(final boolean isHtml)
	{
		this.isHtml = isHtml;
	}

	public boolean isIgnoreBounds()
	{
		return this.ignoreBounds;
	}
	
	/**
	 * If set to true, allows the drawing of tooltips to flow outside of the bounds of the chart on all sides.
	 *
	 * @param ignoreBounds
	 */
	public void setIgnoreBounds(final boolean ignoreBounds)
	{
		this.ignoreBounds = ignoreBounds;
	}
	
	public boolean isShowColorCode()
	{
		return this.showColorCode;
	}
	
	/**
	 * If true, show colored squares next to the slice information in the tooltip.
	 *
	 * @param showColorCode
	 */
	public void setShowColorCode(final boolean showColorCode)
	{
		this.showColorCode = showColorCode;
	}
	
	public String getText()
	{
		return this.text;
	}
	
	/**
	 * What information to display when the user hovers over a pie slice. The following values are supported:
	 * <ul>
	 * <li>'both' - [Default] Display both the absolute value of the slice and the percentage of the whole.</li>
	 * <li>'value' - Display only the absolute value of the slice.</li>
	 * <li>'percentage' - Display only the percentage of the whole represented by the slice.</li>
	 * </ul>
	 *
	 * @param text
	 *            'both', 'value' or 'percentage'
	 */
	public void setText(final String text)
	{
		this.text = text;
	}
	
	public TextStyle getTextStyle()
	{
		return this.textStyle;
	}
	
	/**
	 * An object that specifies the tooltip text style.
	 *
	 * @param textStyle
	 *            the Style to set
	 */
	public void setTextStyle(final TextStyle textStyle)
	{
		this.textStyle = textStyle;
	}
	
	/**
	 * @return the trigger
	 */
	public String getTrigger()
	{
		return this.trigger;
	}
	
	/**
	 * The user interaction that causes the tooltip to be displayed: <br>
	 * <ul>
	 * <li>'focus' - The tooltip will be displayed when the user hovers over the
	 * element.</li>
	 * <li>'none' - The tooltip will not be displayed.</li>
	 * <li>'selection' - The tooltip will be displayed when the user selects the
	 * element.</li>
	 * </ul>
	 * <br>
	 *
	 * @param trigger
	 *            the trigger to set
	 */
	public void setTrigger(final String trigger)
	{
		this.trigger = trigger;
	}

	@Override
	public String toString()
	{
		final StringBuilder str = new StringBuilder();
		str.append("{ isHtml :" + this.isHtml + ", trigger: '" + this.trigger + "', ignoreBounds: " + this.ignoreBounds
			+ ", showColorCode: " + this.showColorCode + ", text: '" + this.text + "'");
		if(this.textStyle != null)
		{
			str.append(", textStyle: " + this.textStyle);
		}
		str.append(" } ");
		return str.toString();
	}
	
}
