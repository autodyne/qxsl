/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.extra.table;

import java.io.*;
import java.nio.charset.Charset;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import javax.xml.namespace.QName;

import qxsl.field.FieldFormats;
import qxsl.model.Field;
import qxsl.model.Item;

import static java.nio.charset.StandardCharsets.US_ASCII;

/**
 * ADIFのうちADIと呼ばれる旧方式の書式の部分的な実装です。
 *
 *
 * @author 無線部開発班
 *
 * @since 2019/07/04
 */
public final class AdisFormat extends BaseFormat {
	private static final String URI = "adif.org";

	public AdisFormat() {
		super("adis");
	}

	@Override
	public TableDecoder decoder(Reader reader) {
		return new AdisDecoder(reader);
	}

	@Override
	public TableEncoder encoder(Writer writer) {
		return new AdisEncoder(writer);
	}

	@Override
	public TableDecoder decoder(InputStream is) {
		return decoder(new InputStreamReader(is, US_ASCII));
	}

	@Override
	public TableEncoder encoder(OutputStream os) {
		return encoder(new OutputStreamWriter(os, US_ASCII));
	}

	/**
	 * ADIサブセット書式で直列化された交信記録をデコードします。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2019/07/08
	 */
	private final class AdisDecoder extends PlainTextDecoder {
		private final String PATTERN = "<(\\w+?):(\\d+?)(:\\w)?>";
		private final Pattern pattern;
		private final FieldFormats fields;

		/**
		 * 指定されたリーダを読み込むデコーダを構築します。
		 *
		 * @param reader 交信記録を読み込むリーダ
		 */
		public AdisDecoder(Reader reader) {
			super(reader);
			this.pattern = Pattern.compile(PATTERN);
			this.fields = new FieldFormats();
		}

		/**
		 * 交信記録を読み込みます。
		 *
		 * @return 交信記録
		 *
		 * @throws IOException 読み込みに失敗した場合
		 */
		@Override
		public final List<Item> decode() throws IOException {
			try {
				return items();
			} catch (RuntimeException ex) {
				throw new IOException(ex);
			}
		}

		/**
		 * ヘッダを読み飛ばして交信記録のリストを読み込みます。
		 * ヘッダと交信記録の両方が空の場合は例外を発生します。
		 *
		 * @return 交信記録
		 *
		 * @throws IOException 読み込みに失敗した場合
		 */
		private final List<Item> items() throws IOException {
			final ArrayList<Item> items = new ArrayList<>();
			final boolean hdr = !readLine().startsWith("<");
			super.reset();
			String record;
			boolean valid = false;
			if(hdr) valid |= tokenize("<eoh>") != null;
			while((record = tokenize("<eor>")) != null) {
				items.add(item(record));
				valid = true;
			}
			if(valid) return Collections.unmodifiableList(items);
			throw new IOException("no headers/records detected");
		}

		/**
		 * 文字列から交信記録を1件読み込みます。
		 *
		 * @param text 1件の交信記録の文字列
		 *
		 * @return 読み込んだ1件の交信
		 */
		private final Item item(String text) {
			final Item item = new Item();
			final Matcher mat = pattern.matcher(text);
			while(mat.find()) {
				final int i = mat.end();
				final int j = Integer.parseInt(mat.group(2));
				final String data = text.substring(i, i + j);
				final String name = mat.group(1).toUpperCase();
				item.add(fields.cache(new QName(URI, name)).field(data));
			}
			return item;
		}
	}

	/**
	 * ADIサブセット書式で直列化された交信記録をエンコードします。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2019/07/08
	 */
	private final class AdisEncoder extends PlainTextEncoder {
		private final FieldFormats fields;

		/**
		 * 指定されたライタに交信記録を書き出すデコーダを構築します。
		 *
		 * @param writer 交信記録を出力するライタ
		 */
		public AdisEncoder(Writer writer) {
			super(writer);
			this.fields = new FieldFormats();
		}

		/**
		 * 交信記録を出力します。
		 *
		 * @param items 交信記録
		 *
		 * @throws IOException 書き出しに失敗した場合
		 */
		@Override
		public final void encode(List<Item> items) throws IOException {
			final var form = DateTimeFormatter.ofPattern("uuuuMMdd HHmmss");
			final var date = ZonedDateTime.now().format(form);
			printf("ADIF");
			println();
			print("<adif_ver:5>3.0.0");
			print("<programid:4>qxsl");
			printf("<created_timestamp:%d>%s", date.length(), date);
			print("<eoh>");
			println();
			for(Item r : items) item(r);
		}

		/**
		 * 指定された交信記録をストリームに出力します。
		 *
		 * @param item 出力する交信記録
		 *
		 * @throws IOException 書き出しに失敗した場合
		 */
		private final void item(Item item) throws IOException {
			for(var f: item) if(f.name().getNamespaceURI().equals(URI)) field(f);
			print("<eor>");
			println();
		}

		/**
		 * 指定された属性をストリームに出力します。
		 *
		 * @param field 出力する属性
		 *
		 * @throws IOException 書き出しに失敗した場合
		 */
		private final void field(Field field) throws IOException {
			final String local = field.name().getLocalPart();
			final String value = fields.encode(field);
			printf("<%s:%d>%s", local, value.length(), value);
		}
	}
}
