/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Released under the GNU Lesser General Public License (LGPL) v3 (see LICENSE)
 * Univ. Tokyo Amateur Radio Club Development Task Force (https://nextzlog.dev)
*******************************************************************************/
package qxsl.table;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.util.LinkedList;
import java.util.List;
import java.util.Properties;

import qxsl.utils.AssetUtil;
import qxsl.value.Field;

/**
 * 書式の説明を設定ファイルから取得する機能を提供します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2019/06/16
 */
public abstract class BasicFactory extends TableFactory {
	private final Properties config;

	/**
	 * 指定された名前の書式を初期化します。
	 *
	 *
	 * @param type 書式の名前
	 *
	 * @throws UncheckedIOException 設定の取得時の例外
	 */
	public BasicFactory(String type) {
		this.config = AssetUtil.from(this).properties(type);
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
		return config.getProperty(key);
	}

	/**
	 * この書式を識別する完全な名前を返します。
	 *
	 *
	 * @return 書式の名前
	 */
	@Override
	public final String type() {
		return get("type");
	}

	/**
	 * この書式の表示に適した文字列を返します。
	 *
	 *
	 * @return 書式の文字列表現
	 */
	@Override
	public final String name() {
		return get("name");
	}

	/**
	 * この書式の詳細を述べる文字列を返します。
	 *
	 *
	 * @return 書式の説明
	 */
	@Override
	public final String help() {
		final var text = get("desc-text");
		final var file = get("desc-file");
		if(text != null) return text;
		return AssetUtil.from(this).string(file);
	}

	/**
	 * この書式の拡張子の不変リストを返します。
	 *
	 *
	 * @return 拡張子のリスト
	 */
	@Override
	public final List<String> extensions() {
		return List.of(get("extensions").split(","));
	}

	/**
	 * この書式が対応する属性値を列挙する集合です。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2024/06/02
	 *
	 * @param <F> 属性値の総称型
	 */
	public static final class FieldSet<F extends Field<?>> {
		private final List<F> list;
		private final String name;

		/**
		 * 属性値の列挙子の集合を構築します。
		 *
		 *
		 * @param name 属性の名前
		 */
		public FieldSet(String name) {
			this.name = name;
			this.list = new LinkedList<>();
		}

		/**
		 * 指定された属性を末尾に追加します。
		 *
		 *
		 * @param field 属性
		 */
		public final void add(F field) {
			this.list.add(field);
		}

		/**
		 * 指定された序数の属性値を返します。
		 *
		 *
		 * @param index 序数
		 *
		 * @return 属性値
		 *
		 * @throws IOException 範囲外の場合
		 */
		public final F valueOf(int index) throws IOException {
			if(list.size() > index) return list.get(index);
			final var msg = "index %d is not registered within %s";
			throw new IOException(String.format(msg, index, name));
		}

		/**
		 * 指定された属性値の序数を返します。
		 *
		 *
		 * @param value 属性値
		 *
		 * @return 対応する列挙子があれば返す
		 *
		 * @throws IOException 範囲外の場合
		 */
		public final int indexOf(F value) throws IOException {
			if(list.contains(value)) return list.indexOf(value);
			final var msg = "value %s is not registered within %s";
			throw new IOException(String.format(msg, value, name));
		}
	}
}
