
package com.rapidclipse.framework.server.persistence.jpa.dal;

import java.io.Serializable;

import javax.persistence.metamodel.Attribute;

import com.rapidclipse.framework.server.persistence.jpa.AttributeChain;


/**
 * Range support for {@link Comparable} types.
 *
 * @author XDEV Software
 */
@SuppressWarnings("rawtypes")
public class Range<E, D extends Comparable> implements Serializable
{
	/*
	 * Range builder
	 */
	public static <E, D extends Comparable> Range<E, D> newRange(final Attribute<?, ?>... fields)
	{
		return new Range<E, D>(fields);
	}
	
	private static final long serialVersionUID = 1L;
	
	private final PathHolder  pathHolder;
	private D                 from;
	private D                 to;
	private Boolean           includeNull;
	
	/**
	 * Constructs a new Range with no boundaries and no restrictions on field's
	 * nullability.
	 *
	 * @param attributes
	 *            the path to the attribute of an existing entity.
	 */
	public Range(final Attribute<?, ?>... attributes)
	{
		this.pathHolder = new PathHolder(new AttributeChain(attributes));
	}
	
	/**
	 * Constructs a new Range.
	 *
	 * @param from
	 *            the lower boundary of this range. Null means no lower
	 *            boundary.
	 * @param to
	 *            the upper boundary of this range. Null means no upper
	 *            boundary.
	 * @param attributes
	 *            the path to the attribute of an existing entity.
	 */
	public Range(final D from, final D to, final Attribute<?, ?>... attributes)
	{
		this(attributes);
		this.from = from;
		this.to   = to;
	}
	
	/**
	 * Constructs a new Range.
	 *
	 * @param from
	 *            the lower boundary of this range. Null means no lower
	 *            boundary.
	 * @param to
	 *            the upper boundary of this range. Null means no upper
	 *            boundary.
	 * @param includeNull
	 *            tells whether null should be filtered out or not.
	 * @param attributes
	 *            the path to the attribute of an existing entity.
	 */
	public Range(
		final D from,
		final D to,
		final Boolean includeNull,
		final Attribute<?, ?>... attributes)
	{
		this(from, to, attributes);
		this.includeNull = includeNull;
	}
	
	/**
	 * Constructs a new Range by copy.
	 */
	public Range(final Range<E, D> other)
	{
		this.pathHolder  = other.pathHolder;
		this.from        = other.from;
		this.to          = other.to;
		this.includeNull = other.includeNull;
	}
	
	/**
	 * @return the entity's attribute this Range refers to.
	 */
	public AttributeChain getAttributes()
	{
		return this.pathHolder.getAttributes();
	}
	
	/**
	 * @return the lower range boundary or null for unbound lower range.
	 */
	public D getFrom()
	{
		return this.from;
	}
	
	/**
	 * Sets the lower range boundary. Accepts null for unbound lower range.
	 */
	public void setFrom(final D from)
	{
		this.from = from;
	}
	
	public Range<E, D> from(final D from)
	{
		setFrom(from);
		return this;
	}
	
	public boolean isFromSet()
	{
		return getFrom() != null;
	}
	
	/**
	 * @return the upper range boundary or null for unbound upper range.
	 */
	public D getTo()
	{
		return this.to;
	}
	
	public Range<E, D> to(final D to)
	{
		setTo(to);
		return this;
	}
	
	/**
	 * Sets the upper range boundary. Accepts null for unbound upper range.
	 */
	public void setTo(final D to)
	{
		this.to = to;
	}
	
	public boolean isToSet()
	{
		return getTo() != null;
	}
	
	public void setIncludeNull(final Boolean includeNull)
	{
		this.includeNull = includeNull;
	}
	
	public Range<E, D> includeNull(final Boolean includeNull)
	{
		setIncludeNull(includeNull);
		return this;
	}
	
	public Boolean getIncludeNull()
	{
		return this.includeNull;
	}
	
	public boolean isIncludeNullSet()
	{
		return this.includeNull != null;
	}
	
	public boolean isBetween()
	{
		return isFromSet() && isToSet();
	}
	
	public boolean isSet()
	{
		return isFromSet() || isToSet() || isIncludeNullSet();
	}
	
	@SuppressWarnings("unchecked")
	public boolean isValid()
	{
		if(isBetween())
		{
			return getFrom().compareTo(getTo()) <= 0;
		}
		return true;
	}
	
	public void resetRange()
	{
		this.from        = null;
		this.to          = null;
		this.includeNull = null;
	}
}
