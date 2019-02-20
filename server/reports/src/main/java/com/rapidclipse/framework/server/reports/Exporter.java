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

package com.rapidclipse.framework.server.reports;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.Map;

import com.vaadin.flow.server.StreamResource;

import net.sf.dynamicreports.jasper.builder.JasperReportBuilder;
import net.sf.dynamicreports.report.exception.DRException;
import net.sf.jasperreports.engine.JRDataSource;
import net.sf.jasperreports.engine.JRException;
import net.sf.jasperreports.engine.JasperCompileManager;
import net.sf.jasperreports.engine.JasperFillManager;
import net.sf.jasperreports.engine.JasperPrint;
import net.sf.jasperreports.engine.JasperReport;


/**
 * @author XDEV Software
 *
 */
public interface Exporter
{
	public StreamResource exportToResource(final JasperReportBuilder reportBuilder);
	
	public StreamResource exportToResource(
		final JasperReportBuilder reportBuilder,
		final String fileNamePrefix);
	
	public StreamResource exportToResource(
		final InputStream jrxml,
		final JRDataSource dataSource,
		final Map<String, Object> parameters);
	
	public StreamResource exportToResource(
		final InputStream jrxml,
		final JRDataSource dataSource,
		final Map<String, Object> parameters,
		final String fileNamePrefix);
	
	public byte[] exportToBytes(final JasperReportBuilder reportBuilder);
	
	public byte[] exportToBytes(
		final InputStream jrxml,
		final JRDataSource dataSource,
		final Map<String, Object> parameters);
	
	public void export(final JasperReportBuilder reportBuilder, final OutputStream stream);
	
	public void export(final JasperPrint print, final OutputStream stream);
	
	public void export(
		final InputStream jrxml,
		final JRDataSource dataSource,
		final Map<String, Object> parameters,
		final OutputStream stream);
	
	public static Exporter New(
		final Format format,
		final DynamicExporter dynamicExporter,
		final PlainExporter plainExporter)
	{
		return new Implementation(format, dynamicExporter, plainExporter);
	}
	
	public static class Implementation implements Exporter
	{
		private final Format          format;
		private final DynamicExporter dynamicExporter;
		private final PlainExporter   plainExporter;
		
		public Implementation(
			final Format format,
			final DynamicExporter dynamicExporter,
			final PlainExporter plainExporter)
		{
			super();
			
			this.format          = format;
			this.dynamicExporter = dynamicExporter;
			this.plainExporter   = plainExporter;
		}
		
		protected StreamResource createResource(final byte[] bytes, final String fileNamePrefix)
		{
			final StreamResource resource = new StreamResource(
				fileNamePrefix + "." + this.format.fileSuffix(),
				() -> new ByteArrayInputStream(bytes));
			resource.setContentType(this.format.mimeType());
			return resource;
		}
		
		protected String getDefaultFileNamePrefix()
		{
			return "report" + System.currentTimeMillis();
		}
		
		@Override
		public StreamResource exportToResource(final JasperReportBuilder reportBuilder)
		{
			return exportToResource(reportBuilder, getDefaultFileNamePrefix());
		}
		
		@Override
		public StreamResource exportToResource(
			final JasperReportBuilder reportBuilder,
			final String fileNamePrefix)
		{
			return createResource(exportToBytes(reportBuilder), fileNamePrefix);
		}
		
		@Override
		public byte[] exportToBytes(final JasperReportBuilder reportBuilder)
		{
			final ByteArrayOutputStream stream = new ByteArrayOutputStream();
			export(reportBuilder, stream);
			return stream.toByteArray();
		}
		
		@Override
		public void export(final JasperReportBuilder reportBuilder, final OutputStream stream)
		{
			try
			{
				this.dynamicExporter.export(reportBuilder, stream);
			}
			catch(final DRException e)
			{
				throw new ReportException(e);
			}
		}
		
		@Override
		public StreamResource exportToResource(
			final InputStream jrxml,
			final JRDataSource dataSource,
			final Map<String, Object> parameters)
		{
			return exportToResource(jrxml, dataSource, parameters, getDefaultFileNamePrefix());
		}
		
		@Override
		public StreamResource exportToResource(
			final InputStream jrxml,
			final JRDataSource dataSource,
			final Map<String, Object> parameters,
			final String fileNamePrefix)
		{
			return createResource(exportToBytes(jrxml, dataSource, parameters), fileNamePrefix);
		}
		
		@Override
		public byte[] exportToBytes(
			final InputStream jrxml,
			final JRDataSource dataSource,
			final Map<String, Object> parameters)
		{
			final ByteArrayOutputStream stream = new ByteArrayOutputStream();
			export(jrxml, dataSource, parameters, stream);
			return stream.toByteArray();
		}
		
		@Override
		public void export(
			final InputStream jrxml,
			final JRDataSource dataSource,
			final Map<String, Object> parameters,
			final OutputStream stream)
		{
			try
			{
				final JasperReport report = JasperCompileManager.compileReport(jrxml);
				final JasperPrint  print  = JasperFillManager.fillReport(report, parameters,
					dataSource);
				export(print, stream);
			}
			catch(final JRException e)
			{
				throw new ReportException(e);
			}
		}
		
		@Override
		public void export(final JasperPrint print, final OutputStream stream)
		{
			try
			{
				this.plainExporter.export(print, stream);
			}
			catch(final JRException e)
			{
				throw new ReportException(e);
			}
		}
	}
}
