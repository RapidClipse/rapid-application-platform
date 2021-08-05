
package com.rapidclipse.framework.server.reports.grid.column.headerresolving;

import java.lang.reflect.Field;
import java.util.Optional;

import com.vaadin.flow.component.grid.Grid.Column;
import com.vaadin.flow.data.renderer.Renderer;


/**
 * Tries to access vaadins internal fields of a Grid/Column to get the header.<br/>
 * This will resolve headers who have been set using {@link Column#setHeader(String)}.<br/>
 * <br/>
 * Note: This might fail when the vaadin version changes
 * 
 * @author XDEV Software
 *
 */
public class VaadinInternalRenderingColumnHeaderResolvingStrategy implements ColumnHeaderResolvingStrategy
{
	protected Field findFieldInClass(final Class<?> clazz, final String fieldName)
	{
		Class<?> currentClazz = clazz;
		do
		{
			try
			{
				return currentClazz.getDeclaredField(fieldName);
			}
			catch(final NoSuchFieldException e)
			{
				// continue search
			}
		}
		// stop when we got the field or reached top of class hierarchy
		// no need to search in Object as well
		while((currentClazz = currentClazz.getSuperclass()) != null && currentClazz != Object.class);
		
		return null;
	}
	
	@Override
	public Optional<String> resolve(final Column<?> column)
	{
		try
		{
			final Field colHeaderRendererField = findFieldInClass(Column.class, "headerRenderer");
			if(colHeaderRendererField == null)
			{
				return Optional.empty();
			}
			
			colHeaderRendererField.setAccessible(true);
			
			final Renderer<?> renderer      = (Renderer<?>)colHeaderRendererField.get(column);
			
			final Field       templateField = Renderer.class.getDeclaredField("template");
			templateField.setAccessible(true);
			
			return Optional.ofNullable((String)templateField.get(renderer));
		}
		catch(final Exception e)
		{
			return Optional.empty();
		}
	}
	
}
