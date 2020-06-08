/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.model;

import org.junit.jupiter.api.Test;
import qxsl.extra.field.*;

import static qxsl.extra.table.QxmlFormat.SENT;
import static test.RandTest.*;

/**
 * {@link Sent}クラスのテスト用クラスです。
 *
 *
 * @author 無線部開発班
 *
 * @since 2017/02/25
 *
 */
public final class SentTest extends org.assertj.core.api.Assertions {
	private final Code code = new Code("591420");
	private final RSTQ rstq = new RSTQ(1, 1, 1);
	private final Watt watt = new Watt("M");
	@Test
	public void testType() {
		assertThat(new Sent(new Item()).name()).isEqualTo(SENT);
	}
	@Test
	public void testEquals() {
		final Sent sent1 = new Sent(new Item());
		final Sent sent2 = new Sent(new Item());
		assertThat(sent1).isEqualTo(sent2);
		assertThat(sent1.add(code).get(Qxsl.CODE)).isEqualTo(code);
		assertThat(sent1.add(rstq).get(Qxsl.RSTQ)).isEqualTo(rstq);
		assertThat(sent1.add(watt).get(Qxsl.WATT)).isEqualTo(watt);
		assertThat(sent1).isNotEqualTo(sent2);
		assertThat(sent2.add(code).get(Qxsl.CODE)).isEqualTo(code);
		assertThat(sent2.add(rstq).get(Qxsl.RSTQ)).isEqualTo(rstq);
		assertThat(sent2.add(watt).get(Qxsl.WATT)).isEqualTo(watt);
		assertThat(sent1).isEqualTo(sent2);
	}
}
