/*
 * Copyright (C) 2013-2019 by XDEV Software, All Rights Reserved.
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

package com.rapidclipse.framework.server.webapi.geolocation;

import java.io.Serializable;


/**
 *
 *
 * @author XDEV Software
 * @since 10.02.00
 */
public class PositionOptions implements Serializable
{
	private Boolean enableHighAccuracy;
	private Integer timeout;
	private Integer maximumAge;
	
	public static PositionOptions Default()
	{
		return new PositionOptions(true, 10_000, 1_000);
	}
	
	public PositionOptions(final Boolean enableHighAccuracy, final Integer timeout, final Integer maximumAge)
	{
		this.enableHighAccuracy = enableHighAccuracy;
		this.timeout            = timeout;
		this.maximumAge         = maximumAge;
	}
	
	public PositionOptions()
	{
		super();
	}
	
	public Boolean getEnableHighAccuracy()
	{
		return this.enableHighAccuracy;
	}
	
	public PositionOptions setEnableHighAccuracy(final Boolean enableHighAccuracy)
	{
		this.enableHighAccuracy = enableHighAccuracy;
		return this;
	}
	
	public Integer getTimeout()
	{
		return this.timeout;
	}
	
	public PositionOptions setTimeout(final Integer timeout)
	{
		this.timeout = timeout;
		return this;
	}
	
	public Integer getMaximumAge()
	{
		return this.maximumAge;
	}
	
	public PositionOptions setMaximumAge(final Integer maximumAge)
	{
		this.maximumAge = maximumAge;
		return this;
	}
}
