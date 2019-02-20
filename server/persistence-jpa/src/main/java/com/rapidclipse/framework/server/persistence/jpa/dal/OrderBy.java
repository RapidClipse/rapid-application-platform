
package com.rapidclipse.framework.server.persistence.jpa.dal;

import static com.rapidclipse.framework.server.persistence.jpa.dal.OrderByDirection.ASC;
import static com.rapidclipse.framework.server.persistence.jpa.dal.OrderByDirection.DESC;

import java.io.Serializable;

import javax.persistence.metamodel.Attribute;

import com.rapidclipse.framework.server.persistence.jpa.AttributeChain;


/**
 * Holder class for search ordering used by the {@link SearchParameters}.
 *
 * @author XDEV Software
 */
public class OrderBy implements Serializable
{
	private static final long serialVersionUID = 1L;
	private final PathHolder  pathHolder;
	private OrderByDirection  direction        = ASC;
	
	public OrderBy(final OrderByDirection direction, final Attribute<?, ?>... attributes)
	{
		this.direction  = direction;
		this.pathHolder = new PathHolder(new AttributeChain(attributes));
	}
	
	public OrderBy(final OrderByDirection direction, final String path, final Class<?> from)
	{
		this.direction  = direction;
		this.pathHolder = new PathHolder(path, from);
	}
	
	public AttributeChain getAttributes()
	{
		return this.pathHolder.getAttributes();
	}
	
	public String getPath()
	{
		return this.pathHolder.getPath();
	}
	
	public OrderByDirection getDirection()
	{
		return this.direction;
	}
	
	public boolean isOrderDesc()
	{
		return DESC == this.direction;
	}
	
	@Override
	public int hashCode()
	{
		final int prime  = 31;
		int       result = 1;
		result = prime * result + ((this.pathHolder == null) ? 0 : this.pathHolder.hashCode());
		return result;
	}
	
	@Override
	public boolean equals(final Object obj)
	{
		if(this == obj)
		{
			return true;
		}
		if(obj == null)
		{
			return false;
		}
		if(getClass() != obj.getClass())
		{
			return false;
		}
		final OrderBy other = (OrderBy)obj;
		if(this.pathHolder == null)
		{
			if(other.pathHolder != null)
			{
				return false;
			}
		}
		else if(!this.pathHolder.equals(other.pathHolder))
		{
			return false;
		}
		return true;
	}
}
