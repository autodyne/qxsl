/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.model;

import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.Test;

import qxsl.draft.Code;
import qxsl.draft.Qxsl;
import qxsl.draft.RSTQ;
import qxsl.draft.Watt;

import static gaas.table.QxmlFactory.SENT;

/**
 * {@link Sent}クラスのテスト用クラスです。
 *
 *
 * @author 無線部開発班
 *
 * @since 2017/02/25
 */
public final class SentTest extends Assertions {
	private final Code code = new Code("591420");
	private final RSTQ rstq = new RSTQ(1, 1, 1);
	private final Watt watt = new Watt("M");

	@Test
	public void testEquals() {
		final var sent1 = new Sent();
		final var sent2 = new Sent();
		assertThat(sent1).isEqualTo(sent2);
		assertThat(sent1.set(code).get(Qxsl.CODE)).isEqualTo(code);
		assertThat(sent1.set(rstq).get(Qxsl.RSTQ)).isEqualTo(rstq);
		assertThat(sent1.set(watt).get(Qxsl.WATT)).isEqualTo(watt);
		assertThat(sent1).isNotEqualTo(sent2);
		assertThat(sent2.set(code).get(Qxsl.CODE)).isEqualTo(code);
		assertThat(sent2.set(rstq).get(Qxsl.RSTQ)).isEqualTo(rstq);
		assertThat(sent2.set(watt).get(Qxsl.WATT)).isEqualTo(watt);
		assertThat(sent1).isEqualTo(sent2);
	}

	@Test
	public void testType() {
		assertThat(new Sent().name()).isEqualTo(SENT);
	}
}
