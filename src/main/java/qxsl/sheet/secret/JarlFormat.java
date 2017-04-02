/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Language: Java Standard Edition 8
 *****************************************************************************
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics http://pafelog.net
*****************************************************************************/
package qxsl.sheet.secret;

import java.io.*;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.StringJoiner;
import javax.xml.parsers.*;
import org.xml.sax.*;
import org.xml.sax.helpers.DefaultHandler;

/**
 * 日本アマチュア無線連盟が推奨するサマリーシートの書式です。
 * 
 * 
 * @author Journal of Hamradio Informatics
 * 
 * @since 2017/03/11
 *
 */
public final class JarlFormat implements qxsl.sheet.SheetFormat {
	/**
	 * この書式を識別する完全な名前を返します。
	 * 
	 * @return 書式の名前
	 */
	@Override
	public String getName() {
		return "jarl";
	}

	/**
	 * この書式の詳細を表示するのに適した文字列を返します。
	 * 
	 * @return 書式の説明
	 */
	@Override
	public final String toString() {
		return "JARL SUMMARY SHEET";
	}

	/**
	 * 指定したストリームをこの書式でデコードして提出書類を読み込みます。
	 * 
	 * @param in 提出書類を読み込むストリーム
	 * @return 提出書類
	 * @throws IOException 入出力時の例外
	 */
	public Map<String, String> decode(InputStream in) throws IOException {
		return new JarlFormatDecoder(in).read();
	}

	/**
	 * この書式でエンコードした提出書類を指定したストリームに書き込みます。
	 * 
	 * @param out 提出書類を書き込むストリーム
	 * @param map 出力する提出書類
	 * @throws IOException 入出力時の例外
	 */
	public void encode(OutputStream out, Map<String, String> map) throws IOException {
		new JarlFormatEncoder(out).write(new HashMap<>(map));
	}

	/**
	 * JARLサマリーシートを開封するデコーダの実装です。
	 * 
	 * 
	 * @author Journal of Hamradio Informatics
	 * 
	 * @since 2014/11/04
	 *
	 */
	private static final class JarlFormatDecoder {
		private SAXParserFactory parserFactory;
		private final InputStreamReader reader;

		/**
		 * 指定された{@link InputStream}を読み込むデコーダを構築します。
		 * 
		 * @param in 提出書類を読み込むストリーム
		 * @throws IOException SJISに対応していない場合
		 */
		public JarlFormatDecoder(InputStream in) throws IOException {
			parserFactory = SAXParserFactory.newInstance();
			this.reader = new InputStreamReader(in, "SJIS");
		}

		/**
		 * 読み込んだサマリーシートを開封します。
		 *
		 * @return 提出書類
		 * @throws IOException 読み込みに失敗した場合
		 */
		public Map<String, String> read() throws IOException	{
			SummaryHandler handler = new SummaryHandler();
			try {
				SAXParser parser = parserFactory.newSAXParser();
				parser.parse(cleanse(), handler);
				return Collections.unmodifiableMap(handler.binds);
			} catch(ParserConfigurationException ex) {
				throw new IOException(ex);
			} catch(SAXException ex) {
				throw new IOException(ex);
			} finally {
				reader.close();
			}
		}

		/**
		 * サマリーシートをXMLパーサーで扱うために整形します。
		 * 
		 * @return XMLを提供するInputSource
		 * @throws IOException 読み込みに失敗した場合
		 */
		private InputSource cleanse() throws IOException {
			String text = preprocess();
			StringBuilder sb = new StringBuilder("<sheet>");
			boolean isTag = false;
			boolean isAttr = false;
			boolean isQuoted = false;
			StringBuilder attrVal = new StringBuilder();
			for(int i = 0; i < text.length(); i++) {
				char ch = text.charAt(i);
				if(isTag && !isAttr && ch == '=') {
					isAttr = true;
					sb.append(ch);
				} else if(isAttr) {
					if(!isQuoted) {
						if(ch == '"') isQuoted = true;
						else if(ch == '>' || ch == ' ') {
							isAttr = false;
							sb.append('"');
							sb.append(attrVal);
							sb.append('"');
							sb.append(ch);
							attrVal.setLength(0);
							if(ch == '>') isTag = false;
						} else attrVal.append(ch);
					} else if(ch == '"') {
						isQuoted = false;
					} else attrVal.append(ch);
				} else if(!isTag && ch == '<') {
					isTag = true;
					sb.append(ch);
				} else if(isTag && ch == '>') {
					isTag = false;
					sb.append(ch);
				} else if(ch == '<') {
					sb.append("&lt;");
				} else if(ch == '>') {
					sb.append("&gt;");
				} else if(ch == '&') {
					sb.append("&amp;");
				} else if(ch == '"') {
					sb.append("&quot;");
				} else if(ch == '\'') {
					sb.append("&apos;");
				} else if(isTag) {
					sb.append(Character.toUpperCase(ch));
				} else sb.append(ch);
			}
			String xml = sb.append("</sheet>").toString();
			return new InputSource(new StringReader(xml));
		}

		/**
		 * サマリーシートを整形する直前に前処理します。
		 * 
		 * @return 前処理済の文字列
		 * @throws IOException 読み込みに失敗した場合
		 */
		private String preprocess() throws IOException {
			BufferedReader br = new BufferedReader(reader);
			StringBuilder buffer = new StringBuilder();
			StringBuilder escape = new StringBuilder();
			String line = null;
			while((line = br.readLine()) != null) {
				for(int i = 0; i < line.length(); i++) {
					char ch = line.charAt(i);
					if(escape.length() == 0) {
						if(ch == '&') escape.append(ch);
						else buffer.append(ch);
					} else if(ch == ';') {
						switch(escape.toString()) {
							case "&gt": buffer.append('>'); break;
							case "&lt": buffer.append('<'); break;
							default: buffer.append(escape); break;
						}
						escape.setLength(0);
					} else escape.append(ch);
				}
				buffer.append("\n");
			}
			return buffer.append(escape).toString();
		}

		/**
		 * 提出書類の各項目を抽出するためのハンドラです。
		 * 
		 * 
		 * @author Journal of Hamradio Informatics
		 * 
		 * @since 2014/11/04
		 *
		 */
		private static class SummaryHandler extends DefaultHandler {
			private final StringBuilder buffer = new StringBuilder();
			public final Map<String, String> binds = new HashMap<>();
			private String tag = null;
			@Override
			public void startElement(String uri, String ln, String qn, Attributes attrs) {
				StringJoiner joiner = new StringJoiner(" ");
				joiner.add(qn);
				for(int i=0; i < attrs.getLength(); i++) {
					final String qname = attrs.getQName(i);
					final String value = attrs.getValue(i);
					if(qn.equals("SUMMARYSHEET")) binds.put(qname, value);
					else joiner.add(String.format("%s=%s", qname, value));
				}
				if(qn.matches("(sheet|SUMMARYSHEET)")) tag = null;
				else tag = qn.equals("LOGSHEET")? qn: joiner.toString();
			}
			@Override
			public void characters(char[] ch, int off, int len) {
				if(tag != null) buffer.append(new String(ch, off, len));
			}
			@Override
			public void endElement(String uri, String ln, String qn) {
				if(tag != null) binds.put(tag, buffer.toString().trim());
				buffer.setLength(0);
				tag = null;
			}
		}
	}

	/**
	 * JARLサマリーシートを作成するエンコーダの実装です。
	 * 
	 * 
	 * @author Journal of Hamradio Informatics
	 * 
	 * @since 2017/03/11
	 *
	 */
	private static final class JarlFormatEncoder {
		private final PrintStream out;

		/**
		 * 指定された{@link OutputStream}に書き込むエンコーダを構築します。
		 * 
		 * @param out 提出書類を書き込むストリーム
		 * @throws IOException SJISに対応していない場合
		 */
		public JarlFormatEncoder(OutputStream out) throws IOException {
			this.out = new PrintStream(out, true, "SJIS");
		}

		/**
		 * サマリーシートを出力します。{@link Writer}は閉じられます。
		 *
		 * @param map 属性名と属性値の集合
		 * @throws IOException 入出力の例外
		 */
		public void write(Map<String, String> map) throws IOException {
			final String version = map.remove("VERSION");
			final String record = map.remove("LOGSHEET");
			if(version == null) out.println("<SUMMARYSHEET>");
			else out.printf("<SUMMARYSHEET VERSION=%s>%n", version);
			for(String key: map.keySet()) {
				final String val = map.get(key).trim();
				final String close = key.split(" ")[0];
				out.printf("<%s>%s</%s>%n", key, val, close);
			}
			out.println("</SUMMARYSHEET>");
			if(record != null) out.printf("<LOGSHEET>%n%s%n</LOGSHEET>", record.trim());
			out.flush();
			out.close();
		}
	}
}
