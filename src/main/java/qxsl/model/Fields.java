/*****************************************************************************
 * Amateur Radio Operational Logging Library 'xsum' since 2013 February 16th
 * Language: Java Standard Edition 8
 *****************************************************************************
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics http://pafelog.net
*****************************************************************************/
package qxsl.model;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.ServiceLoader;
import javax.xml.namespace.QName;

/**
 * {@link FieldFormat}クラスの自動的な検出とキャッシュの機能を提供します。
 * 
 * 
 * @author Journal of Hamradio Informatics
 * 
 * @since 2013/06/08
 *
 */
public final class Fields implements Iterable<FieldFormat> {
	private final ServiceLoader<FieldFormat> loader;
	private final Map<QName, Map<String, Field>> caches;
	private final QName expert;
	
	/**
	 * スレッドの{@link ClassLoader}を指定して機構を構築します。
	 */
	public Fields() {
		this(Thread.currentThread().getContextClassLoader());
	}

	/**
	 * 指定された{@link QName}の属性に特化した機構を構築します。
	 *
	 * @param qname 属性の名前
	 */
	public Fields(QName qname) {
		this.loader = ServiceLoader.load(FieldFormat.class);
		this.caches = new HashMap<>();
		this.expert = qname;
	}

	/**
	 * 指定された{@link ClassLoader}から検索する機構を構築します。
	 * 
	 * @param cl クラスローダー
	 */
	public Fields(ClassLoader cl) {
		this.loader = ServiceLoader.load(FieldFormat.class, cl);
		this.caches = new HashMap<>();
		this.expert = null;
	}

	@Override
	public Iterator<FieldFormat> iterator() {
		if(expert == null) return loader.iterator();
		return Arrays.asList(getFormat(expert)).iterator();
	}

	/**
	 * 指定された名前を持つ{@link FieldFormat}を検索して返します。
	 * 
	 * @param name 属性の名前
	 * @return 対応するフォーマット 存在しない場合null
	 */
	public FieldFormat getFormat(QName name) {
		for(FieldFormat fmt: loader) {
			if(fmt.type().equals(name)) return fmt;
		}
		return null;
	}

	/**
	 * この機構が特化する属性に対して、指定された値の属性を返します。
	 * 
	 * @param value {@link Field}の値を表す文字列
	 * @return 属性値 属性が未登録の場合はnull
	 * @throws Exception 値を表す文字列が不正な場合
	 */
	public Field cache(String value) throws Exception {
		return cache(expert, value);
	}

	/**
	 * 指定された名前と値の属性を返します。属性はキャッシュされます。
	 * 
	 * @param qname {@link Field}の名前
	 * @param value {@link Field}の値を表す文字列
	 * @return 属性値 属性が未登録の場合はnull
	 * @throws Exception 値を表す文字列が不正な場合
	 */
	public Field cache(QName qname, String value) throws Exception {
		Map<String, Field> map = caches.get(qname);
		if(map == null) caches.put(qname, map = new HashMap<>());
		if(map.containsKey(value)) return map.get(value);
		if(getFormat(qname) == null) return null;
		Field field = getFormat(qname).decode(value);
		map.put(value, field);
		return map.get(value);
	}
}
