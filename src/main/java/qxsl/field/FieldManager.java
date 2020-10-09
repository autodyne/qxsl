/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.field;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.ServiceLoader;
import javax.xml.namespace.QName;

import qxsl.value.Field;

/**
 * {@link FieldFactory}実装クラスを自動的に検出して管理します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2013/06/08
 */
public final class FieldManager implements Iterable<FieldFactory> {
	private final ServiceLoader<FieldFactory> list;
	private final Map<QName, Cache> caches;

	/**
	 * デフォルトのクラスパスを参照するインスタンスです。
	 *
	 *
	 * @since 2020/08/06
	 */
	public static final FieldManager FIELDS = new FieldManager();

	/**
	 * インスタンスを構築します。
	 */
	public FieldManager() {
		this(FieldManager.class.getClassLoader());
	}

	/**
	 * 指定されたローダを参照するインスタンスを構築します。
	 *
	 *
	 * @param cl 書式を検出するクラスローダ
	 */
	public FieldManager(ClassLoader cl) {
		this.list = ServiceLoader.load(FieldFactory.class, cl);
		this.caches = new HashMap<>();
	}

	/**
	 * このインスタンスが検出した書式を返します。
	 *
	 *
	 * @return 書式のイテレータ
	 */
	@Override
	public Iterator<FieldFactory> iterator() {
		return list.iterator();
	}

	/**
	 * 指定された属性の入出力を行う書式を返します。
	 *
	 *
	 * @param name 属性の名前
	 *
	 * @return 対応する書式 またはnull
	 */
	public FieldFactory forName(QName name) {
		for(var f: list) if(f.target().equals(name)) return f;
		return null;
	}

	/**
	 * 指定された属性を適切な書式で文字列に変換します。
	 *
	 *
	 * @param field 文字列に変換する属性
	 *
	 * @return 文字列による属性値の表現
	 */
	public final String encode(Field field) {
		try {
			return forName(field.name()).encode(field);
		} catch (NullPointerException ex) {
			return String.valueOf(field.value());
		}
	}

	/**
	 * 指定された文字列と名前を持つ属性値を取得します。
	 *
	 *
	 * @param qname 属性の名前
	 * @param value 属性を表す文字列
	 *
	 * @return 属性値
	 */
	public Field decode(QName qname, String value) {
		try {
			return forName(qname).decode(value);
		} catch (NullPointerException ex) {
			return new Any(qname, value);
		}
	}

	/**
	 * 指定された属性名に対する{@link Cache}を返します。
	 *
	 *
	 * @param qname 属性の名前
	 *
	 * @return キャッシュ
	 */
	public final Cache cache(QName qname) {
		return caches.computeIfAbsent(qname, Cache::new);
	}

	/**
	 * 任意の属性を保存的に格納する{@link Field}実装クラスです。
	 * クラスパスに{@link FieldFactory}がない場合に使用されます。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2019/06/28
	 */
	public static final class Any extends Field<String> {
		private final String value;

		/**
		 * 属性名と値を指定して{@link Any}を構築します。
		 *
		 *
		 * @param qname 属性名
		 * @param value 属性値
		 */
		public Any(QName qname, String value) {
			super(qname);
			this.value = value;
		}

		@Override
		public String value() {
			return value;
		}
	}

	/**
	 * 特定の属性名を持つ属性に特化したキャッシュ機構です。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2019/06/26
	 */
	public final class Cache extends HashMap<String, Field> {
		private final FieldFactory format;
		private final QName qname;

		/**
		 * 指定された属性に特化するキャッシュを構築します。
		 *
		 *
		 * @param qname 属性名
		 */
		private Cache(QName qname) {
			this.format = forName(this.qname = qname);
		}

		/**
		 * 指定された値の{@link Field}を生成します。
		 *
		 *
		 * @param value 属性値を表す文字列
		 *
		 * @return 読み込まれた属性
		 */
		private Field createField(String value) {
			try {
				return format.decode(value);
			} catch(NullPointerException ex) {
				return new Any(qname, value);
			}
		}

		/**
		 * 指定された値の{@link Field}を取得します。
		 *
		 *
		 * @param value 属性値を表す整数値
		 *
		 * @return 属性値
		 */
		public Field field(int value) {
			return field(String.valueOf(value));
		}

		/**
		 * 指定された値の{@link Field}を取得します。
		 *
		 *
		 * @param value 属性値を表す文字列
		 *
		 * @return 属性値
		 */
		public Field field(String value) {
			return computeIfAbsent(value, this::createField);
		}
	}
}
