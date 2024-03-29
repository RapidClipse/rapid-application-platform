/*
 * Copyright (C) 2013-2023 by XDEV Software, All Rights Reserved.
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
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *
 * Contributors:
 *     XDEV Software - initial API and implementation
 */
package com.rapidclipse.framework.server.reports;

import java.io.InputStream;
import java.io.OutputStream;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import com.rapidclipse.framework.server.resources.ApplicationResource;
import com.vaadin.flow.server.StreamResource;

import net.sf.jasperreports.engine.JRDataSource;
import net.sf.jasperreports.engine.JRException;
import net.sf.jasperreports.engine.JasperCompileManager;
import net.sf.jasperreports.engine.JasperReport;
import net.sf.jasperreports.engine.data.JRBeanCollectionDataSource;


/**
 * @author XDEV Software
 *
 */
public interface Report
{
	public Report jrxml(final InputStream jrxmlInputStream);
	
	public Report jrxml(final String jrxmlPath);
	
	public Report dataSource(final JRDataSource dataSource);
	
	public default Report dataSource(final Stream<?> stream)
	{
		return dataSource(stream.collect(Collectors.toList()));
	}
	
	public Report dataSource(Collection<?> beans);
	
	public Report parameters(final Map<String, Object> parameters);
	
	public Report parameter(final String name, final Object value);
	
	public Report subreport(final String name, final InputStream jrxmlInputStream);
	
	public Report subreport(final String name, final String jrxmlPath);
	
	public Report mapFields(Map<String, String> fieldNameMapping);
	
	public Report mapField(String from, String to);
	
	public void export(final Format format, final OutputStream stream);
	
	public byte[] exportToBytes(final Format format);
	
	public StreamResource exportToResource(final Format format);
	
	public StreamResource exportToResource(final Format format, final String fileNamePrefix);
	
	public static Report New()
	{
		return new Default();
	}
	
	public static class Default implements Report
	{
		private InputStream               jrxmlInputStream;
		private String                    jrxmlPath;
		private JRDataSource              dataSource;
		private final Map<String, Object> parameters       = new HashMap<>();
		private final Map<String, String> fieldNameMapping = new HashMap<>();

		protected Default()
		{
			super();
		}
		
		@Override
		public Report jrxml(final InputStream jrxmlInputStream)
		{
			this.jrxmlInputStream = jrxmlInputStream;
			return this;
		}
		
		@Override
		public Report jrxml(final String jrxmlPath)
		{
			this.jrxmlPath = jrxmlPath;
			return this;
		}
		
		@Override
		public Report dataSource(final JRDataSource dataSource)
		{
			this.dataSource = dataSource;
			return this;
		}
		
		@Override
		public Report dataSource(final Collection<?> beans)
		{
			this.dataSource = new JRBeanCollectionDataSource(beans);
			return this;
		}
		
		@Override
		public Report parameters(final Map<String, Object> parameters)
		{
			this.parameters.putAll(parameters);
			return this;
		}
		
		@Override
		public Report parameter(final String name, final Object value)
		{
			this.parameters.put(name, value);
			return this;
		}
		
		@Override
		public Report subreport(final String name, final InputStream jrxmlInputStream)
		{
			try
			{
				final JasperReport subreport = JasperCompileManager
					.compileReport(getJrxml(jrxmlInputStream, null));
				this.parameters.put(name, subreport);
				return this;
			}
			catch(final JRException e)
			{
				throw new ReportException(e);
			}
		}
		
		@Override
		public Report subreport(final String name, final String jrxmlPath)
		{
			try
			{
				final JasperReport subreport = JasperCompileManager
					.compileReport(getJrxml(null, jrxmlPath));
				this.parameters.put(name, subreport);
				return this;
			}
			catch(final JRException e)
			{
				throw new ReportException(e);
			}
		}
		
		@Override
		public Report mapFields(final Map<String, String> fieldNameMapping)
		{
			this.fieldNameMapping.putAll(fieldNameMapping);
			return this;
		}
		
		@Override
		public Report mapField(final String from, final String to)
		{
			this.fieldNameMapping.put(from, to);
			return this;
		}
		
		@Override
		public void export(final Format format, final OutputStream stream)
		{
			format.createExporter().export(getJrxml(), getDataSource(), this.parameters, stream);
		}
		
		@Override
		public byte[] exportToBytes(final Format format)
		{
			return format.createExporter().exportToBytes(getJrxml(), getDataSource(), this.parameters);
		}
		
		@Override
		public StreamResource exportToResource(final Format format)
		{
			return format.createExporter().exportToResource(getJrxml(), getDataSource(), this.parameters);
		}
		
		@Override
		public StreamResource exportToResource(final Format format, final String fileNamePrefix)
		{
			return format.createExporter().exportToResource(getJrxml(), getDataSource(), this.parameters,
				fileNamePrefix);
		}
		
		protected InputStream getJrxml()
		{
			return getJrxml(this.jrxmlInputStream, this.jrxmlPath);
		}
		
		protected InputStream getJrxml(final InputStream jrxmlInputStream, final String jrxmlPath)
		{
			InputStream jrxml = jrxmlInputStream;
			if(jrxml == null)
			{
				if(jrxmlPath == null)
				{
					throw new IllegalStateException(
						"No input specified, provide either a jrxml inputstream or path");
				}
				
				jrxml = ApplicationResource.createInputStream(getCallerClass(), jrxmlPath);
			}
			
			return jrxml;
		}
		
		protected Class<?> getCallerClass()
		{
			for(final StackTraceElement element : Thread.currentThread().getStackTrace())
			{
				final String className = element.getClassName();
				if(!className.startsWith("java.") && !className.startsWith("com.rapidclipse.framework.server.reports."))
				{
					try
					{
						return Class.forName(className);
					}
					catch(final Exception e)
					{
						return null;
					}
				}
			}
			
			return null;
		}
		
		protected JRDataSource getDataSource()
		{
			JRDataSource dataSource = this.dataSource;
			if(dataSource == null)
			{
				throw new IllegalStateException("No data source specified");
			}
			if(!this.fieldNameMapping.isEmpty())
			{
				dataSource = MappedDataSource.create(dataSource, this.fieldNameMapping);
			}
			return dataSource;
		}
	}
}
