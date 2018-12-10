/*
 * Copyright (C) 2013-2018 by XDEV Software, All Rights Reserved.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 *
 * For further information see
 * <http://www.rapidclipse.com/en/legal/license/license.html>.
 */

package software.xdev.rap.server.reports;


import net.sf.dynamicreports.design.transformation.StyleResolver;
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



	public static abstract class Abstract implements Format
	{
		private final String	name;
		private final String	fileSuffix;
		private final String	mimeType;


		public Abstract(final String name, final String fileSuffix, final String mimeType)
		{
			super();

			this.name = name;
			this.fileSuffix = fileSuffix;
			this.mimeType = mimeType;
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
		public Exporter createExporter()
		{
			return Exporter.New(this,createDynamicExporter(),createPlainExporter());
		}


		protected abstract DynamicExporter createDynamicExporter();


		protected abstract PlainExporter createPlainExporter();
	}



	public static class Pdf extends Abstract
	{
		public Pdf()
		{
			super("PDF","pdf","application/pdf");
		}


		@Override
		protected DynamicExporter createDynamicExporter()
		{
			return (builder, stream) -> builder.toPdf(stream);
		}


		@Override
		protected PlainExporter createPlainExporter()
		{
			return (print, stream) -> JasperExportManager.exportReportToPdfStream(print,stream);
		}
	}



	public static class Xml extends Abstract
	{
		public Xml()
		{
			super("XML","xml","text/xml");
		}


		@Override
		protected DynamicExporter createDynamicExporter()
		{
			return (builder, stream) -> builder.toHtml(stream);
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



	public static class Text extends Abstract
	{
		public Text()
		{
			super("Text","txt","text/plain");
		}


		@Override
		protected DynamicExporter createDynamicExporter()
		{
			return (builder, stream) -> builder.toText(stream);
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
				final DRFont font = Defaults.getDefaults().getFont();
				configuration.setCharWidth(new Float(StyleResolver.getFontWidth(font)));
				configuration.setCharHeight(new Float(StyleResolver.getFontHeight(font)));
				exporter.setConfiguration(configuration);
				exporter.exportReport();
			};
		}
	}



	public static class Rtf extends Abstract
	{
		public Rtf()
		{
			super("Rich Text","rtf","text/rtf");
		}


		@Override
		protected DynamicExporter createDynamicExporter()
		{
			return (builder, stream) -> builder.toRtf(stream);
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
		public Csv()
		{
			super("CSV","csv","text/comma-separated-values");
		}


		@Override
		protected DynamicExporter createDynamicExporter()
		{
			return (builder, stream) -> builder.toCsv(stream);
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
		public Xls()
		{
			super("Excel (xls)","xls","application/msexcel");
		}


		@Override
		protected DynamicExporter createDynamicExporter()
		{
			return (builder, stream) -> builder.toXls(stream);
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
		public Xlsx()
		{
			super("Excel (xlsx)","xlsx",
					"application/vnd.openxmlformats-officedocument.spreadsheetml.sheet");
		}


		@Override
		protected DynamicExporter createDynamicExporter()
		{
			return (builder, stream) -> builder.toXlsx(stream);
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
		public Docx()
		{
			super("Word (docx)","docx",
					"application/vnd.openxmlformats-officedocument.wordprocessingml.document");
		}


		@Override
		protected DynamicExporter createDynamicExporter()
		{
			return (builder, stream) -> builder.toDocx(stream);
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
		public Pptx()
		{
			super("Powerpoint (pptx)","pptx","application/mspowerpoint");
		}


		@Override
		protected DynamicExporter createDynamicExporter()
		{
			return (builder, stream) -> builder.toPptx(stream);
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
		public Odt()
		{
			super("Open Document (odt)","odt","application/vnd.oasis.opendocument.text");
		}


		@Override
		protected DynamicExporter createDynamicExporter()
		{
			return (builder, stream) -> builder.toOdt(stream);
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
		public Ods()
		{
			super("Open Document (ods)","ods","vnd.oasis.opendocument.spreadsheet");
		}


		@Override
		protected DynamicExporter createDynamicExporter()
		{
			return (builder, stream) -> builder.toOds(stream);
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
