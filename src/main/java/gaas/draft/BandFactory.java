/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package gaas.draft;

import java.math.BigDecimal;
import javax.xml.namespace.QName;

import qxsl.draft.Band;
import qxsl.draft.Qxsl;
import qxsl.field.FieldFactory;
import qxsl.value.Field;

/**
 * {@link Band}を生成する書式です。
 *
 * @author 無線部開発班
 * @since 2013/06/08
 */
public final class BandFactory implements FieldFactory {
	@Override
	public QName target() {
		return Qxsl.BAND;
	}

	@Override
	public Band decode(String value) {
		return new Band(new BigDecimal(value));
	}

	@Override
	public String encode(Field field) {
		return ((Band) field).value().toPlainString();
	}
}
