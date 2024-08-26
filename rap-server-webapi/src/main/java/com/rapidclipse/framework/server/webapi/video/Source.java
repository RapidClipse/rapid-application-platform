/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
 */
package com.rapidclipse.framework.server.webapi.video;

import java.io.Serializable;


/**
 * A video source that can be added to an {@link Video} object.
 * 
 * @author XDEV Software
 * @since 10.02.00
 */
public class Source implements Serializable
{
	private String src;
	private String type;

	public Source()
	{
	}

	public Source(final String src, final String type)
	{
		this.src  = src;
		this.type = type;
	}

	public String getSrc()
	{
		return this.src;
	}

	public Source setSrc(final String src)
	{
		this.src = src;
		return this;
	}

	public String getType()
	{
		return this.type;
	}

	public Source setType(final String type)
	{
		this.type = type;
		return this;
	}
}
