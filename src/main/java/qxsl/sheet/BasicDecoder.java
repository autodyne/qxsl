/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License: GNU Lesser General Public License v3.0 (see LICENSE)
 * Author: Journal of Hamradio Informatics (https://pafelog.net)
*******************************************************************************/
package qxsl.sheet;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;
import java.io.UncheckedIOException;
import java.util.Properties;
import java.util.stream.Collectors;
import javax.xml.transform.stream.StreamSource;
import javax.xml.validation.Schema;
import javax.xml.validation.SchemaFactory;

import org.xml.sax.SAXException;

import qxsl.field.FieldManager;
import qxsl.utils.AssetUtil;

/**
 * 書式の説明を設定ファイルから取得する機能を提供します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2022/07/17
 */
public abstract class BasicDecoder implements SheetDecoder {
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
	 * 交信記録を抽出する鍵の文字列を返します。
	 *
	 *
	 * @return 交信記録を指す鍵
	 */
	public final String getTableKey() {
		return get("table");
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
