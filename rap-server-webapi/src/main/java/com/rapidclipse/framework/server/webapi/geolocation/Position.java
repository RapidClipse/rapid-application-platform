/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
 */
package com.rapidclipse.framework.server.webapi.geolocation;

import java.io.Serializable;


/**
 *
 * @author XDEV Software
 * @since 10.02.00
 *
 */
public class Position implements Serializable
{
	private Coordinates coords;
	private long        timestamp;
	
	public Position()
	{
	}
	
	public Position(final Coordinates coords, final long timestamp)
	{
		this.coords    = coords;
		this.timestamp = timestamp;
	}
	
	public Coordinates getCoords()
	{
		return this.coords;
	}
	
	public Position setCoords(final Coordinates coords)
	{
		this.coords = coords;
		return this;
	}
	
	public long getTimestamp()
	{
		return this.timestamp;
	}
	
	public Position setTimestamp(final long timestamp)
	{
		this.timestamp = timestamp;
		return this;
	}
}
