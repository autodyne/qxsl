/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Language: Java Standard Edition 8
 *****************************************************************************
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics http://pafelog.net
*****************************************************************************/
package qxsl.extra.field;

import javax.xml.namespace.QName;
import qxsl.field.FieldFormat;
import qxsl.field.FieldMapper;
import qxsl.model.Field;
import qxsl.model.Rcvd;
import qxsl.model.Sent;

/**
 * コンテストで相手局と交換するシリアル番号です。
 * 
 * 
 * @author Journal of Hamradio Informatics
 * 
 * @since 2013/06/09
 *
 */
public final class Code extends Qxsl<String> {
	private final String value;

	/**
	 * シリアル番号を指定して{@link Code}を構築します。
	 * 
	 * @param code シリアル番号
	 */
	public Code(String code) {
		super(CODE);
		this.value = code;
	}

	@Override
	public String value() {
		return value;
	}

	/**
	 * {@link Code}を生成する書式です。
	 * 
	 * 
	 * @author Journal of Hamradio Informatics
	 * 
	 * @since 2013/06/08
	 *
	 */
	public static final class Format implements FieldFormat {
		@Override
		public QName target() {
			return CODE;
		}

		@Override
		public Code decode(String value) {
			return new Code(value);
		}

		@Override
		public String encode(Field field) {
			return field.value().toString();
		}
	}

	/**
	 * {@link Code}への変換を行う変換器です。
	 * 
	 * 
	 * @author Journal of Hamradio Informatics
	 * 
	 * @since 2019/06/29
	 *
	 */
	public static final class Mapper implements FieldMapper {
		@Override
		public QName target() {
			return CODE;
		}

		@Override
		public Code search(Rcvd rcvd) {
			Object code = rcvd.getItem().value(new QName(ADIF, "SRX"));
			return code != null? new Code(code.toString()): null;
		}

		@Override
		public Code search(Sent sent) {
			Object code = sent.getItem().value(new QName(ADIF, "STX"));
			return code != null? new Code(code.toString()): null;
		}
	}
}
