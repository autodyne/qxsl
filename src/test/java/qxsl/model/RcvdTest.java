/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Released under the GNU Lesser General Public License (LGPL) v3 (see LICENSE)
 * Univ. Tokyo Amateur Radio Club Development Task Force (https://nextzlog.dev)
*******************************************************************************/
package qxsl.model;

import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.Test;

import qxsl.draft.Code;
import qxsl.draft.Qxsl;
import qxsl.draft.RSTQ;
import qxsl.draft.Watt;

import static gaas.table.QxmlFactory.RCVD;

/**
 * {@link Rcvd}クラスの挙動を検査します。
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
	public void testEquals() {
		final var rcvd1 = new Rcvd();
		final var rcvd2 = new Rcvd();
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

	@Test
	public void testType() {
		assertThat(new Rcvd().name()).isEqualTo(RCVD);
	}
}
