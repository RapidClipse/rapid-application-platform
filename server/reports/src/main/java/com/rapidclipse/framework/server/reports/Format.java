/*
 * Copyright (C) 2013-2021 by XDEV Software, All Rights Reserved.
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
 *     XDEV Software Corp. - initial API and implementation
 */

package com.rapidclipse.framework.server.reports;

import java.util.Objects;

import com.rapidclipse.framework.server.util.ServiceLoader;

import net.sf.dynamicreports.design.transformation.StyleResolver;
import net.sf.dynamicreports.jasper.builder.JasperReportBuilder;
import net.sf.dynamicreports.report.base.style.DRFont;
import net.sf.dynamicreports.report.defaults.Defaults;
import net.sf.jasperreports.engine.DefaultJasperReportsContext;
import net.sf.jasperreports.engine.JasperExportManager;
import net.sf.jasperreports.engine.export.HtmlExporter;
import net.sf.jasperreports.engine.export.JRCsvExporter;
import net.sf.jasperreports.engine.export.JRRtfExporter;
import net.sf.jasperreports.engine.export.JRTextExporter;
import net.sf.jasperreports.engine.export.JRXlsExporter;
import net.sf.jasperreports.engine.export.oasis.JROdsExporter;
import net.sf.jasperreports.engine.export.oasis.JROdtExporter;
import net.sf.jasperreports.engine.export.ooxml.JRDocxExporter;
import net.sf.jasperreports.engine.export.ooxml.JRPptxExporter;
import net.sf.jasperreports.engine.export.ooxml.JRXlsxExporter;
import net.sf.jasperreports.export.SimpleExporterInput;
import net.sf.jasperreports.export.SimpleHtmlExporterOutput;
import net.sf.jasperreports.export.SimpleOutputStreamExporterOutput;
import net.sf.jasperreports.export.SimpleTextReportConfiguration;
import net.sf.jasperreports.export.SimpleWriterExporterOutput;


/**
 * @author XDEV Software
 *
 */
public interface Format
{
	public String name();
	
	public String fileSuffix();
	
	public String mimeType();
	
	public Exporter createExporter();
	
	/**
	 * Returns if a preview can be shown in a "standardized" browser (e.g. the latest versions of Chrome and Firefox)
	 *
	 * @return <code>true</code> if a preview can be shown in the browser
	 * @since 11.00.00
	 */
	boolean isPreviewableInStandardBrowser();
	
	public static Format Csv()
	{
		return new Csv();
	}
	
	public static Format Docx()
	{
		return new Docx();
	}
	
	public static Format Ods()
	{
		return new Ods();
	}
	
	public static Format Odt()
	{
		return new Odt();
	}
	
	public static Format Pdf()
	{
		return new Pdf();
	}
	
	public static Format Pptx()
	{
		return new Pptx();
	}
	
	public static Format Rtf()
	{
		return new Rtf();
	}
	
	public static Format Text()
	{
		return new Text();
	}
	
	public static Format Xls()
	{
		return new Xls();
	}
	
	public static Format Xlsx()
	{
		return new Xlsx();
	}
	
	public static Format Xml()
	{
		return new Xml();
	}
	
	/**
	 *
	 * @author XDEV Software
	 * @since 10.02.02
	 */
	public static Format Html()
	{
		return new Html();
	}
	
	public static Format[] DefaultFormats()
	{
		return new Format[]{
			Csv(),
			Docx(),
			Ods(),
			Odt(),
			Pdf(),
			Pptx(),
			Rtf(),
			Text(),
			Xls(),
			Xlsx(),
			Html()
		};
	}
	
	public static Format[] All()
	{
		return ServiceLoader.forType(Format.class).servicesStream().toArray(Format[]::new);
	}
	
	public abstract static class Abstract implements Format
	{
		private final String name;
		private final String fileSuffix;
		private final String mimeType;
		protected boolean    canBePreviewedInStandardBrowser = false;
		
		protected Abstract(final String name, final String fileSuffix, final String mimeType)
		{
			super();
			
			this.name       = name;
			this.fileSuffix = fileSuffix;
			this.mimeType   = mimeType;
		}
		
		public void canBePreviewedInStandardBrowser()
		{
			setPreviewableInStandardBrowser(true);
		}
		
		public void setPreviewableInStandardBrowser(final boolean canBePreviewedInStandardBrowser)
		{
			this.canBePreviewedInStandardBrowser = canBePreviewedInStandardBrowser;
		}
		
		@Override
		public String name()
		{
			return this.name;
		}
		
		@Override
		public String fileSuffix()
		{
			return this.fileSuffix;
		}
		
		@Override
		public String mimeType()
		{
			return this.mimeType;
		}
		
		@Override
		public boolean isPreviewableInStandardBrowser()
		{
			return this.canBePreviewedInStandardBrowser;
		}
		
		@Override
		public Exporter createExporter()
		{
			return Exporter.New(this, createDynamicExporter(), createPlainExporter());
		}
		
		protected abstract DynamicExporter createDynamicExporter();
		
		protected abstract PlainExporter createPlainExporter();
		
		@Override
		public int hashCode()
		{
			return Objects.hash(this.name);
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
			final Abstract other = (Abstract)obj;
			return Objects.equals(this.name, other.name);
		}
	}
	
	public static class Pdf extends Abstract
	{
		public final static String MIME_TYPE = "application/pdf";
		
		public Pdf()
		{
			super("PDF", "pdf", MIME_TYPE);
			canBePreviewedInStandardBrowser();
		}
		
		@Override
		protected DynamicExporter createDynamicExporter()
		{
			return JasperReportBuilder::toPdf;
		}
		
		@Override
		protected PlainExporter createPlainExporter()
		{
			return (print, stream) -> JasperExportManager.exportReportToPdfStream(print, stream);
		}
	}
	
	/**
	 *
	 * @author XDEV Software
	 * @since 10.02.02
	 */
	public static class Html extends Abstract
	{
		public final static String MIME_TYPE = "text/html";
		
		public Html()
		{
			super("HTML", "html", MIME_TYPE);
			canBePreviewedInStandardBrowser();
		}
		
		@Override
		protected DynamicExporter createDynamicExporter()
		{
			return JasperReportBuilder::toHtml;
		}
		
		@Override
		protected PlainExporter createPlainExporter()
		{
			return (print, stream) -> {
				
				final HtmlExporter exporter = new HtmlExporter(
					DefaultJasperReportsContext.getInstance());
				exporter.setExporterInput(new SimpleExporterInput(print));
				exporter.setExporterOutput(new SimpleHtmlExporterOutput(stream));
				exporter.exportReport();
			};
		}
	}
	
	public static class Xml extends Abstract
	{
		public final static String MIME_TYPE = "text/xml";
		
		public Xml()
		{
			super("XML", "xml", MIME_TYPE);
		}
		
		@Override
		protected DynamicExporter createDynamicExporter()
		{
			return JasperReportBuilder::toXml;
		}
		
		@Override
		protected PlainExporter createPlainExporter()
		{
			return JasperExportManager::exportReportToXmlStream;
		}
	}
	
	public static class Text extends Abstract
	{
		public final static String MIME_TYPE = "text/plain";
		
		public Text()
		{
			super("Text", "txt", MIME_TYPE);
			canBePreviewedInStandardBrowser();
		}
		
		@Override
		protected DynamicExporter createDynamicExporter()
		{
			return JasperReportBuilder::toText;
		}
		
		@Override
		protected PlainExporter createPlainExporter()
		{
			return (print, stream) -> {
				
				final JRTextExporter exporter = new JRTextExporter(
					DefaultJasperReportsContext.getInstance());
				exporter.setExporterInput(new SimpleExporterInput(print));
				exporter.setExporterOutput(new SimpleWriterExporterOutput(stream));
				final SimpleTextReportConfiguration configuration = new SimpleTextReportConfiguration();
				final DRFont                        font          = Defaults.getDefaults().getFont();
				configuration.setCharWidth(new Float(StyleResolver.getFontWidth(font)));
				configuration.setCharHeight(new Float(StyleResolver.getFontHeight(font)));
				exporter.setConfiguration(configuration);
				exporter.exportReport();
			};
		}
	}
	
	public static class Rtf extends Abstract
	{
		public final static String MIME_TYPE = "text/rtf";
		
		public Rtf()
		{
			super("Rich Text", "rtf", MIME_TYPE);
		}
		
		@Override
		protected DynamicExporter createDynamicExporter()
		{
			return JasperReportBuilder::toRtf;
		}
		
		@Override
		protected PlainExporter createPlainExporter()
		{
			return (print, stream) -> {
				
				final JRRtfExporter exporter = new JRRtfExporter(
					DefaultJasperReportsContext.getInstance());
				exporter.setExporterInput(new SimpleExporterInput(print));
				exporter.setExporterOutput(new SimpleWriterExporterOutput(stream));
				exporter.exportReport();
			};
		}
	}
	
	public static class Csv extends Abstract
	{
		public final static String MIME_TYPE = "text/comma-separated-values";
		
		public Csv()
		{
			super("CSV", "csv", MIME_TYPE);
		}
		
		@Override
		protected DynamicExporter createDynamicExporter()
		{
			return JasperReportBuilder::toCsv;
		}
		
		@Override
		protected PlainExporter createPlainExporter()
		{
			return (print, stream) -> {
				
				final JRCsvExporter exporter = new JRCsvExporter(
					DefaultJasperReportsContext.getInstance());
				exporter.setExporterInput(new SimpleExporterInput(print));
				exporter.setExporterOutput(new SimpleWriterExporterOutput(stream));
				exporter.exportReport();
			};
		}
	}
	
	public static class Xls extends Abstract
	{
		public final static String MIME_TYPE = "application/vnd.ms-excel";
		
		public Xls()
		{
			super("Excel (xls)", "xls", MIME_TYPE);
		}
		
		@Override
		protected DynamicExporter createDynamicExporter()
		{
			return JasperReportBuilder::toXls;
		}
		
		@Override
		protected PlainExporter createPlainExporter()
		{
			return (print, stream) -> {
				
				final JRXlsExporter exporter = new JRXlsExporter(
					DefaultJasperReportsContext.getInstance());
				exporter.setExporterInput(new SimpleExporterInput(print));
				exporter.setExporterOutput(new SimpleOutputStreamExporterOutput(stream));
				exporter.exportReport();
			};
		}
	}
	
	public static class Xlsx extends Abstract
	{
		public final static String MIME_TYPE = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet";
		
		public Xlsx()
		{
			super("Excel (xlsx)", "xlsx", MIME_TYPE);
		}
		
		@Override
		protected DynamicExporter createDynamicExporter()
		{
			return JasperReportBuilder::toXlsx;
		}
		
		@Override
		protected PlainExporter createPlainExporter()
		{
			return (print, stream) -> {
				
				final JRXlsxExporter exporter = new JRXlsxExporter(
					DefaultJasperReportsContext.getInstance());
				exporter.setExporterInput(new SimpleExporterInput(print));
				exporter.setExporterOutput(new SimpleOutputStreamExporterOutput(stream));
				exporter.exportReport();
			};
		}
	}
	
	public static class Docx extends Abstract
	{
		public final static String MIME_TYPE =
			"application/vnd.openxmlformats-officedocument.wordprocessingml.document";
		
		public Docx()
		{
			super("Word (docx)", "docx", MIME_TYPE);
		}
		
		@Override
		protected DynamicExporter createDynamicExporter()
		{
			return JasperReportBuilder::toDocx;
		}
		
		@Override
		protected PlainExporter createPlainExporter()
		{
			return (print, stream) -> {
				
				final JRDocxExporter exporter = new JRDocxExporter(
					DefaultJasperReportsContext.getInstance());
				exporter.setExporterInput(new SimpleExporterInput(print));
				exporter.setExporterOutput(new SimpleOutputStreamExporterOutput(stream));
				exporter.exportReport();
			};
		}
	}
	
	public static class Pptx extends Abstract
	{
		public final static String MIME_TYPE =
			"application/vnd.openxmlformats-officedocument.presentationml.presentation";
		
		public Pptx()
		{
			super("Powerpoint (pptx)", "pptx", MIME_TYPE);
		}
		
		@Override
		protected DynamicExporter createDynamicExporter()
		{
			return JasperReportBuilder::toPptx;
		}
		
		@Override
		protected PlainExporter createPlainExporter()
		{
			return (print, stream) -> {
				
				final JRPptxExporter exporter = new JRPptxExporter(
					DefaultJasperReportsContext.getInstance());
				exporter.setExporterInput(new SimpleExporterInput(print));
				exporter.setExporterOutput(new SimpleOutputStreamExporterOutput(stream));
				exporter.exportReport();
			};
		}
	}
	
	public static class Odt extends Abstract
	{
		public final static String MIME_TYPE = "application/vnd.oasis.opendocument.text";
		
		public Odt()
		{
			super("Open Document Text (odt)", "odt", MIME_TYPE);
		}
		
		@Override
		protected DynamicExporter createDynamicExporter()
		{
			return JasperReportBuilder::toOdt;
		}
		
		@Override
		protected PlainExporter createPlainExporter()
		{
			return (print, stream) -> {
				
				final JROdtExporter exporter = new JROdtExporter(
					DefaultJasperReportsContext.getInstance());
				exporter.setExporterInput(new SimpleExporterInput(print));
				exporter.setExporterOutput(new SimpleOutputStreamExporterOutput(stream));
				exporter.exportReport();
			};
		}
	}
	
	public static class Ods extends Abstract
	{
		public final static String MIME_TYPE = "application/vnd.oasis.opendocument.spreadsheet";
		
		public Ods()
		{
			super("Open Document Spreadsheet (ods)", "ods", MIME_TYPE);
		}
		
		@Override
		protected DynamicExporter createDynamicExporter()
		{
			return JasperReportBuilder::toOds;
		}
		
		@Override
		protected PlainExporter createPlainExporter()
		{
			return (print, stream) -> {
				
				final JROdsExporter exporter = new JROdsExporter(
					DefaultJasperReportsContext.getInstance());
				exporter.setExporterInput(new SimpleExporterInput(print));
				exporter.setExporterOutput(new SimpleOutputStreamExporterOutput(stream));
				exporter.exportReport();
			};
		}
	}
}
