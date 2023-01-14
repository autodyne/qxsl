/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Released under the GNU Lesser General Public License (LGPL) v3 (see LICENSE)
 * Univ. Tokyo Amateur Radio Club Development Task Force (https://nextzlog.dev)
*******************************************************************************/
package qxsl.table;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.time.format.DateTimeFormatter;
import java.util.HashSet;
import java.util.Properties;
import java.util.Set;
import javax.xml.namespace.QName;

import qxsl.model.Item;
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
public abstract class BasicEncoder extends TableEncoder {
	private final Set<QName> itemFields;
	private final Set<QName> rcvdFields;
	private final Set<QName> sentFields;
	private final String fieldSpace;
	private final Properties config;

	/**
	 * 指定された書式のエンコーダを初期化します。
	 *
	 *
	 * @param type 書式の名前
	 *
	 * @throws UncheckedIOException 設定の取得時の例外
	 */
	public BasicEncoder(String type) {
		this.config = AssetUtil.from(this).properties(type);
		this.fieldSpace = get("field-space");
		this.itemFields = new HashSet<>();
		this.rcvdFields = new HashSet<>();
		this.sentFields = new HashSet<>();
		loadFields("item-fields", itemFields);
		loadFields("rcvd-fields", rcvdFields);
		loadFields("sent-fields", sentFields);
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
	 * この書式の先頭行の文字列を返します。
	 *
	 *
	 * @return 先頭行の文字列
	 *
	 * @since 2019/07/11
	 */
	public final String getHeaderText() {
		return get("head-text").replaceAll("^\\R+|\\R+$", "");
	}

	/**
	 * この書式の出力時に使う時刻の書式を返します。
	 *
	 *
	 * @return 時刻の書式
	 *
	 * @since 2020/09/06
	 */
	public final DateTimeFormatter getTimeEncoder() {
		return DateTimeFormatter.ofPattern(get("time-encoder"));
	}

	/**
	 * 永続化可能な属性の集合を設定から取得します。
	 *
	 *
	 * @param key 属性の集合を表す名前
	 * @param set 属性が格納される集合
	 */
	private final void loadFields(String key, Set<QName> set) {
		final var value = get(key).replace("\n", "").replace("\r", "");
		for(var qname: value.split(",")) set.add(QName.valueOf(qname));
	}

	/**
	 * 交信記録がこの書式で永続化可能か確認します。
	 *
	 *
	 * @param item 交信記録
	 *
	 * @throws IOException 検査の結果の例外
	 *
	 * @since 2022/07/07
	 */
	public final void verify(Item item) throws IOException {
		for(var f: item) verifyItem(f);
		for(var f: item.getRcvd()) verifyRcvd(f);
		for(var f: item.getSent()) verifySent(f);
	}

	/**
	 * 指定された属性が{@link Item}に設定可能か確認します。
	 *
	 *
	 * @param field 属性
	 *
	 * @throws IOException 検査の結果の例外
	 *
	 * @since 2022/07/07
	 */
	private final void verifyItem(Field field) throws IOException {
		if(!this.fieldSpace.equals(field.name().getNamespaceURI())) {
			if(this.itemFields.contains(field.name())) return;
			final var text = "Item does not support field %s";
			throw new IOException(String.format(text, field));
		}
	}

	/**
	 * 指定された属性が{@link Rcvd}に設定可能か確認します。
	 *
	 *
	 * @param field 属性
	 *
	 * @throws IOException 検査の結果の例外
	 *
	 * @since 2022/07/07
	 */
	private final void verifyRcvd(Field field) throws IOException {
		if(!this.fieldSpace.equals(field.name().getNamespaceURI())) {
			if(this.rcvdFields.contains(field.name())) return;
			final var text = "Rcvd does not support field %s";
			throw new IOException(String.format(text, field));
		}
	}

	/**
	 * 指定された属性が{@link Sent}に設定可能か確認します。
	 *
	 *
	 * @param field 属性
	 *
	 * @throws IOException 検査の結果の例外
	 *
	 * @since 2022/07/07
	 */
	private final void verifySent(Field field) throws IOException {
		if(!this.fieldSpace.equals(field.name().getNamespaceURI())) {
			if(this.sentFields.contains(field.name())) return;
			final var text = "Sent does not support field %s";
			throw new IOException(String.format(text, field));
		}
	}
}
