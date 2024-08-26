/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
 */
package com.rapidclipse.framework.server.webapi.memory;

/**
 *
 *
 * @author XDEV Software
 * @since 10.02.00
 */
public class StorageEstimate
{
	private final long quota;
	private final long usage;
	
	public StorageEstimate(final long quota, final long usage)
	{
		this.quota = quota;
		this.usage = usage;
	}
	
	/**
	 * The storage quota in bytes.
	 */
	public long getQuota()
	{
		return this.quota;
	}
	
	/**
	 * The storage usage in bytes.
	 */
	public long getUsage()
	{
		return this.usage;
	}
}
