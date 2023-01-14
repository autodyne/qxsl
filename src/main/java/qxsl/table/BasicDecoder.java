/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Released under the GNU Lesser General Public License (LGPL) v3 (see LICENSE)
 * Univ. Tokyo Amateur Radio Club Development Task Force (https://nextzlog.dev)
*******************************************************************************/
package qxsl.table;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;
import java.io.UncheckedIOException;
import java.time.Year;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeFormatterBuilder;
import java.time.temporal.ChronoField;
import java.util.Properties;
import java.util.stream.Collectors;
import javax.xml.namespace.QName;
import javax.xml.transform.stream.StreamSource;
import javax.xml.validation.Schema;
import javax.xml.validation.SchemaFactory;

import org.xml.sax.SAXException;

import qxsl.field.FieldManager;
import qxsl.utils.AssetUtil;
import qxsl.value.Field;

/**
 * 書式の説明を設定ファイルから取得する機能を提供します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2022/07/16
 */
public abstract class BasicDecoder extends TableDecoder {
	private final FieldManager fields;
	private final Properties config;

	/**
	 * 指定された書式のデコーダを初期化します。
	 *
	 *
	 * @param type 書式の名前
	 *
	 * @throws UncheckedIOException 設定の取得時の例外
	 */
	public BasicDecoder(String type) {
		this.config = AssetUtil.from(this).properties(type);
		this.fields = new FieldManager();
	}

	/**
	 * 指定された名前の設定の値を返します。
	 *
	 *
	 * @param key 設定の名前
	 *
	 * @return 設定の値
	 */
	public final String get(String key) {
		return config.getProperty(key, "");
	}

	/**
	 * この書式の入力時に使う時刻の書式を返します。
	 *
	 *
	 * @return 時刻の書式
	 *
	 * @since 2020/09/06
	 */
	public final DateTimeFormatter getTimeDecoderOld() {
		final var pattern = get("time-decoder");
		final int current = Year.now().getValue();
		final var factory = new DateTimeFormatterBuilder();
		factory.parseDefaulting(ChronoField.YEAR, current);
		return factory.appendPattern(pattern).toFormatter();
	}

	/**
	 * この書式の出力時に使う時刻の書式を返します。
	 *
	 *
	 * @return 時刻の書式
	 *
	 * @since 2020/09/06
	 */
	public final DateTimeFormatter getTimeDecoder() {
		return DateTimeFormatter.ofPattern(get("time-decoder"));
	}

	/**
	 * 指定された名前と整数値から属性を生成します。
	 *
	 *
	 * @param field 名前
	 * @param value 整数値
	 *
	 * @return 属性
	 *
	 * @since 2022/07/16
	 */
	public final Field cache(QName field, int value) {
		return fields.cache(field).field(value);
	}

	/**
	 * 指定された名前と文字列から属性を生成します。
	 *
	 *
	 * @param field 名前
	 * @param value 値の文字列
	 *
	 * @return 属性
	 *
	 * @since 2022/07/16
	 */
	public final Field cache(QName field, String value) {
		return fields.cache(field).field(value);
	}

	/**
	 * 書式の構文の定義をリソースから読み取ります。
	 *
	 *
	 * @return スキーマ
	 *
	 * @throws SAXException スキーマの例外
	 *
	 * @since 2020/09/05
	 */
	public final Schema getXMLSchema() throws SAXException {
		final var path = get("schema");
		final var fact = SchemaFactory.newDefaultInstance();
		return fact.newSchema(getClass().getResource(path));
	}

	/**
	 * 指定された入力がこの書式に従うか検証します。
	 *
	 *
	 * @param reader 入力
	 *
	 * @return 読み取った文字列を読み直す入力
	 *
	 * @throws IOException 構文または読み取り時の例外
	 *
	 * @since 2022/07/17
	 */
	public Reader verify(Reader reader) throws IOException {
		try(final var br = new BufferedReader(reader)) {
			final var join = Collectors.joining("\n");
			final var text = br.lines().collect(join);
			final var read = new StringReader(text);
			final var strm = new StreamSource(read);
			getXMLSchema().newValidator().validate(strm);
			return new StringReader(text);
		} catch (SAXException ex) {
			throw new IOException(ex);
		}
	}
}
