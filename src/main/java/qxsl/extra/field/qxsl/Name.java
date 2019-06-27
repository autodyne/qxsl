/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Language: Java Standard Edition 8
 *****************************************************************************
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics http://pafelog.net
*****************************************************************************/
package qxsl.extra.field.qxsl;

import javax.xml.namespace.QName;
import qxsl.model.Field;
import qxsl.table.FieldFormat;

/**
 * 交信を行なった運用者の個人の名前を表現します。
 * 
 * 
 * @author Journal of Hamradio Informatics
 * 
 * @since 2013/06/08
 *
 */
public final class Name extends Qxsl<String> {
	private final String name;
	
	/**
	 * 運用者名を指定して{@link Name}を構築します。
	 * 
	 * @param name 運用者名
	 */
	public Name(String name) {
		super(NAME);
		this.name = name;
	}

	@Override
	public String value() {
		return name;
	}

	/**
	 * {@link Name}を生成するフォーマットです。
	 * 
	 * 
	 * @author Journal of Hamradio Informatics
	 * 
	 * @since 2013/06/08
	 *
	 */
	public static final class Format implements FieldFormat {
		@Override
		public QName name() {
			return NAME;
		}
	
		@Override
		public Name decode(String value) {
			return new Name(value);
		}
	
		@Override
		public String encode(Field field) {
			return field.value().toString();
		}
	}
}
