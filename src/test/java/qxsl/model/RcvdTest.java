/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.model;

import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.Test;

import qxsl.extra.field.Code;
import qxsl.extra.field.Qxsl;
import qxsl.extra.field.RSTQ;
import qxsl.extra.field.Watt;

import static qxsl.extra.table.QxmlFactory.RCVD;

/**
 * {@link Rcvd}クラスのテスト用クラスです。
 *
 *
 * @author 無線部開発班
 *
 * @since 2017/02/25
 */
public final class RcvdTest extends Assertions {
	private final Code code = new Code("591009");
	private final RSTQ rstq = new RSTQ(5, 9, 9);
	private final Watt watt = new Watt("H");

	@Test
	public void testType() {
		assertThat(new Rcvd(new Item()).name()).isEqualTo(RCVD);
	}

	@Test
	public void testEquals() {
		final var rcvd1 = new Rcvd(new Item());
		final var rcvd2 = new Rcvd(new Item());
		assertThat(rcvd1).isEqualTo(rcvd2);
		assertThat(rcvd1.set(code).get(Qxsl.CODE)).isEqualTo(code);
		assertThat(rcvd1.set(rstq).get(Qxsl.RSTQ)).isEqualTo(rstq);
		assertThat(rcvd1.set(watt).get(Qxsl.WATT)).isEqualTo(watt);
		assertThat(rcvd1).isNotEqualTo(rcvd2);
		assertThat(rcvd2.set(code).get(Qxsl.CODE)).isEqualTo(code);
		assertThat(rcvd2.set(rstq).get(Qxsl.RSTQ)).isEqualTo(rstq);
		assertThat(rcvd2.set(watt).get(Qxsl.WATT)).isEqualTo(watt);
		assertThat(rcvd1).isEqualTo(rcvd2);
	}
}
