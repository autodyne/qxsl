/*****************************************************************************
 * Amateur Radio Operational Logging Library 'xsum' since 2013 February 16th
 * Language: Java Standard Edition 8
 *****************************************************************************
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics http://pafelog.net
*****************************************************************************/
package qxsl.table;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.ServiceLoader;
import javax.xml.namespace.QName;
import qxsl.model.Field;

/**
 * {@link FieldFormat}クラスの自動検出及びインスタンス化機構を実装します。
 * 
 * 
 * @author Journal of Hamradio Informatics
 * 
 * @since 2013/06/08
 *
 */
public final class Fields implements Iterable<FieldFormat> {
	private final ServiceLoader<FieldFormat> loader;
	private final Map<QName, Cache> caches;

	/**
	 * 現在のクラスローダからインスタンス化機構を構築します。
	 */
	public Fields() {
		this(Thread.currentThread().getContextClassLoader());
	}

	/**
	 * 指定のクラスローダからインスタンス化機構を構築します。
	 * 
	 * @param cl {@link FieldFormat}を検出するクラスローダ
	 */
	public Fields(ClassLoader cl) {
		this.loader = ServiceLoader.load(FieldFormat.class, cl);
		this.caches = new HashMap<>();
	}

	@Override
	public Iterator<FieldFormat> iterator() {
		return loader.iterator();
	}

	/**
	 * 指定された名前を持つ{@link FieldFormat}を検索して返します。
	 * 
	 * @param name 属性の名前
	 * @return 対応するフォーマット 存在しない場合null
	 */
	public FieldFormat getFormat(QName name) {
		for(FieldFormat fmt: loader) {
			if(fmt.name().equals(name)) return fmt;
		}
		return null;
	}

	/**
	 * 特定の属性名を持つ属性に特化したキャッシュ機構です。
	 * 
	 * 
	 * @author Journal of Hamradio Informatics
	 *
	 * @since 2019/06/26
	 */
	public final class Cache extends HashMap<String, Field> {
		private final QName qname;

		/**
		 * 指定された属性に特化するキャッシュを構築します。
		 *
		 * @param qname 属性名
		 */
		private Cache(QName qname) {
			this.qname = qname;
		}

		/**
		 * 指定された値の{@link Field}を返します。
		 * 
		 * @param value {@link Field}の値を表す文字列
		 * @return 属性値 属性が未登録の場合はnull
		 */
		public Field field(String value) {
			return computeIfAbsent(value, k->getFormat(qname).decode(k));
		}
	}

	/**
	 * 指定された属性名に対する{@link Cache}を返します。
	 * 
	 * @param qname {@link Field}の名前
	 * @return キャッシュ
	 */
	public final Cache cache(QName qname) {
		return caches.computeIfAbsent(qname, k->new Cache(k));
	}
}
