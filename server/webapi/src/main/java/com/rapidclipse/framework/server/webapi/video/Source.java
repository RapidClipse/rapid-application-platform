/*
 * Copyright (C) 2013-2020 by XDEV Software, All Rights Reserved.
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
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *
 * Contributors:
 *     XDEV Software Corp. - initial API and implementation
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
