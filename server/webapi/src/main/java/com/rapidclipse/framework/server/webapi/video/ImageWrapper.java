
package com.rapidclipse.framework.server.webapi.video;

import java.io.ByteArrayInputStream;
import java.io.Serializable;

import com.vaadin.flow.component.html.Image;
import com.vaadin.flow.server.StreamResource;


/**
 *
 *
 * @author XDEV Software
 * @since 10.02.00
 */
public class ImageWrapper implements Serializable
{
	private final byte[] data;
	
	public ImageWrapper(final byte[] data)
	{
		this.data = data;
	}
	
	public byte[] getData()
	{
		return this.data;
	}
	
	public StreamResource toStreamResource(final String name)
	{
		return new StreamResource(name, () -> new ByteArrayInputStream(this.data));
	}
	
	public Image toImage(final String altText)
	{
		return new Image(this.toStreamResource(altText), altText);
	}
}
