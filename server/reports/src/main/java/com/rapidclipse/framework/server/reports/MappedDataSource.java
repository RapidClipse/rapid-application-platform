/*-
 * ---
 * Rapid Application Platform / Server / Reports
 * --
 * Copyright (C) 2013 - 2019 XDEV Software Corp.
 * --
 * This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License 2.0
 * which accompanies this distribution, and is available at
 * https://www.eclipse.org/legal/epl-2.0/
 * 
 * SPDX-License-Identifier: EPL-2.0
 * 
 * Contributors:
 *     XDEV Software Corp. - initial API and implementation
 * ---
 */

package com.rapidclipse.framework.server.reports;

import java.util.HashMap;
import java.util.Map;

import net.sf.jasperreports.engine.JRDataSource;
import net.sf.jasperreports.engine.JRException;
import net.sf.jasperreports.engine.JRField;
import net.sf.jasperreports.engine.JRRewindableDataSource;
import net.sf.jasperreports.engine.data.IndexedDataSource;
import net.sf.jasperreports.engine.data.JsonData;


/**
 * @author XDEV Software
 *
 */
public class MappedDataSource extends DelegatingDataSource
{
	public static Factory factory(final JRDataSource delegate)
	{
		return new Factory(delegate);
	}
	
	public static class Factory
	{
		private final JRDataSource        delegate;
		private final Map<String, String> fieldNameMapping;
		
		public Factory(final JRDataSource delegate)
		{
			this.delegate         = delegate;
			this.fieldNameMapping = new HashMap<>();
		}
		
		public Factory map(final String from, final String to)
		{
			this.fieldNameMapping.put(from, to);
			return this;
		}
		
		public MappedDataSource create()
		{
			return MappedDataSource.create(this.delegate, this.fieldNameMapping);
		}
	}
	
	@SuppressWarnings({"unchecked", "rawtypes"})
	public static MappedDataSource create(
		final JRDataSource delegate,
		final Map<String, String> fieldNameMapping)
	{
		if(delegate instanceof JsonData)
		{
			return new Json((JsonData)delegate, fieldNameMapping);
		}
		if(delegate instanceof IndexedDataSource && delegate instanceof JRRewindableDataSource)
		{
			return new RewindableIndexed((JRRewindableDataSource)delegate, fieldNameMapping);
		}
		if(delegate instanceof IndexedDataSource)
		{
			return new Indexed((IndexedDataSource)delegate, fieldNameMapping);
		}
		if(delegate instanceof JRRewindableDataSource)
		{
			return new Rewindable((JRRewindableDataSource)delegate, fieldNameMapping);
		}
		return new MappedDataSource(delegate, fieldNameMapping);
	}
	
	private final Map<String, String>  fieldNameMapping;
	private final Map<String, JRField> mappedFields;
	
	public MappedDataSource(final JRDataSource delegate, final Map<String, String> fieldNameMapping)
	{
		super(delegate);
		
		this.fieldNameMapping = fieldNameMapping;
		this.mappedFields     = new HashMap<>();
	}
	
	@Override
	public Object getFieldValue(final JRField jrField) throws JRException
	{
		final String mappedName = this.fieldNameMapping.get(jrField.getName());
		
		JRField      field      = jrField;
		if(mappedName != null)
		{
			field = this.mappedFields.computeIfAbsent(mappedName,
				name -> new MappedField(jrField, name));
		}
		
		return super.getFieldValue(field);
	}
	
	public static class Indexed extends MappedDataSource implements IndexedDataSource
	{
		private final IndexedDataSource delegate;
		
		public Indexed(final IndexedDataSource delegate, final Map<String, String> fieldNameMapping)
		{
			super(delegate, fieldNameMapping);
			
			this.delegate = delegate;
		}
		
		@Override
		public int getRecordIndex()
		{
			return this.delegate.getRecordIndex();
		}
	}
	
	public static class Rewindable extends MappedDataSource implements JRRewindableDataSource
	{
		private final JRRewindableDataSource delegate;
		
		public Rewindable(
			final JRRewindableDataSource delegate,
			final Map<String, String> fieldNameMapping)
		{
			super(delegate, fieldNameMapping);
			
			this.delegate = delegate;
		}
		
		@Override
		public void moveFirst() throws JRException
		{
			this.delegate.moveFirst();
		}
	}
	
	public static class RewindableIndexed<D extends JRRewindableDataSource & IndexedDataSource>
		extends MappedDataSource implements JRRewindableDataSource, IndexedDataSource
	{
		private final D delegate;
		
		public RewindableIndexed(final D delegate, final Map<String, String> fieldNameMapping)
		{
			super(delegate, fieldNameMapping);
			
			this.delegate = delegate;
		}
		
		@Override
		public void moveFirst() throws JRException
		{
			this.delegate.moveFirst();
		}
		
		@Override
		public int getRecordIndex()
		{
			return this.delegate.getRecordIndex();
		}
	}
	
	public static class Json extends Rewindable implements JsonData
	{
		private final JsonData delegate;
		
		public Json(final JsonData delegate, final Map<String, String> fieldNameMapping)
		{
			super(delegate, fieldNameMapping);
			
			this.delegate = delegate;
		}
		
		@Override
		public JsonData subDataSource() throws JRException
		{
			return this.delegate.subDataSource();
		}
		
		@Override
		public JsonData subDataSource(final String selectExpression) throws JRException
		{
			return this.delegate.subDataSource(selectExpression);
		}
	}
}