/*
 * RapidClipse Application Platform (RAP)
 *
 * Copyright (C) 2013-2024 by XDEV Software, All Rights Reserved.
 *
 * License: GNU Lesser General Public License (LGPL), version 3.0 or later.
 * See the LICENSE file in the root directory or https://www.gnu.org/licenses/lgpl+gpl-3.0.txt.
 */
package com.rapidclipse.framework.server.ui;

import com.vaadin.flow.component.HtmlComponent;
import com.vaadin.flow.component.PropertyDescriptor;
import com.vaadin.flow.component.PropertyDescriptors;
import com.vaadin.flow.component.Tag;
import com.vaadin.flow.server.AbstractStreamResource;
import com.vaadin.flow.server.StreamResource;


/**
 * Component representing an <code>&lt;object&gt;</code> element, an external resource, which can be
 * treated as an image, a nested browsing context, or a resource to be handled by a plugin.
 *
 * @see https://developer.mozilla.org/en-US/docs/Web/HTML/Element/object
 * @see https://www.iana.org/assignments/media-types/media-types.xhtml
 *
 * @author XDEV Software
 * @since 10.01.00
 * 
 * @deprecated Use {@link com.vaadin.flow.component.html.HtmlObject} instead
 */
@Deprecated
@Tag("object")
public class HtmlObject extends HtmlComponent
{
	private final static PropertyDescriptor<String, String> dataDescriptor = PropertyDescriptors
		.attributeWithDefault("data", "");

	private final static PropertyDescriptor<String, String> typeDescriptor = PropertyDescriptors
		.attributeWithDefault("type", "");

	/**
	 * Creates a new empty html object.
	 */
	public HtmlObject()
	{
		super();
	}

	/**
	 * Creates a new html object with a data URL.
	 *
	 * @param data
	 * @see #setData(String)
	 */
	public HtmlObject(final String data)
	{
		setData(data);
	}

	/**
	 * Creates a new html object with a data resource.
	 *
	 * @param data
	 * @see #setData(AbstractStreamResource)
	 */
	public HtmlObject(final AbstractStreamResource data)
	{
		setData(data);
	}

	/**
	 * Creates a new html object with a data URL and a type.
	 *
	 * @param data
	 * @param type
	 * @see #setData(String)
	 * @see #setType(String)
	 */
	public HtmlObject(final String data, final String type)
	{
		setData(data);
		setType(type);
	}

	/**
	 * Creates a new html object with a data resource and a type.
	 *
	 * @param data
	 * @param type
	 * @see #setData(AbstractStreamResource)
	 * @see #setType(String)
	 */
	public HtmlObject(final AbstractStreamResource data, final String type)
	{
		setData(data);
		setType(type);
	}

	/**
	 * Gets the data URL.
	 *
	 * @return the data URL of this html object
	 */
	public String getData()
	{
		return get(dataDescriptor);
	}

	/**
	 * Sets the data URL of this html object.
	 *
	 * @param data
	 */
	public void setData(final String data)
	{
		set(dataDescriptor, data);
	}

	/**
	 * Sets the data URL with the URL of the given {@link StreamResource}.
	 *
	 * @param data
	 *            the resource value, not null
	 */
	public void setData(final AbstractStreamResource data)
	{
		getElement().setAttribute(dataDescriptor.getPropertyName(), data);
	}

	/**
	 * Gets the mime type of this html object.
	 *
	 * @return the mime type of this html object
	 */
	public String getType()
	{
		return get(typeDescriptor);
	}

	/**
	 * Sets the mime type of this html object.
	 *
	 * @param type
	 *            the new mime type
	 * @see https://www.iana.org/assignments/media-types/media-types.xhtml
	 */
	public void setType(final String type)
	{
		set(typeDescriptor, type);
	}
}
