/*****************************************************************************
 * Amateur Radio Operational Logging Library 'xsum' since 2013 February 16th
 * Language: Java Standard Edition 8
 *****************************************************************************
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics http://pafelog.net
*****************************************************************************/
package qxsl.field;

import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.ServiceLoader;
import javax.xml.namespace.QName;
import qxsl.model.Field;
import qxsl.model.Tuple;

/**
 * {@link FieldMapper}クラスの自動検出及びインスタンス化機構を実装します。
 * 
 * 
 * @author Journal of Hamradio Informatics
 * 
 * @since 2019/06/28
 *
 */
public final class FieldMappers implements Iterable<FieldMapper> {
	private final ServiceLoader<FieldMapper> loader;

	/**
	 * 現在のクラスローダからインスタンス化機構を構築します。
	 */
	public FieldMappers() {
		this(Thread.currentThread().getContextClassLoader());
	}

	/**
	 * 指定のクラスローダからインスタンス化機構を構築します。
	 * 
	 * @param cl 変換器を検出するクラスローダ
	 */
	public FieldMappers(ClassLoader cl) {
		this.loader = ServiceLoader.load(FieldMapper.class, cl);
	}

	@Override
	public Iterator<FieldMapper> iterator() {
		return loader.iterator();
	}

	/**
	 * 指定された属性の入出力を行う{@link FieldMapper}を返します。
	 * 
	 * @param name 属性の名前
	 * @return 対応する書式 存在しない場合null
	 */
	public List<FieldMapper> getMappers(QName name) {
		List<FieldMapper> result = new LinkedList<>();
		for(FieldMapper map: loader) {
			if(map.target().equals(name)) result.add(map);
		}
		return result;
	}

	/**
	 * 指定された属性に相当する属性を検索し、変換後の属性を返します。
	 * 
	 * @param qname 変換後の属性の名前
	 * @param tuple 属性を検索するタプル
	 * @return 変換後の属性 存在しない場合null
	 */
	public Field search(QName qname, Tuple tuple) {
		for(FieldMapper fm: getMappers(qname)) {
			Field field = fm.search(tuple);
			if(field != null) return field;
		}
		return null;
	}
}
