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
package com.rapidclipse.framework.server.mobilekit.camera;

import java.io.InputStream;

import com.vaadin.flow.server.AbstractStreamResource;


/**
 *
 * @author XDEV Software
 *
 */
public interface Picture
{
	public String getBase64data();
	
	public byte[] toRawData();
	
	public InputStream toInputStream();
	
	public AbstractStreamResource toResource();
	
	public String getURI();
}
