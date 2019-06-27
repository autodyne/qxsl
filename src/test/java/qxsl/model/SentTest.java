/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Language: Java Standard Edition 8
 *****************************************************************************
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics http://pafelog.net
*****************************************************************************/
package qxsl.model;

import org.junit.Test;
import qxsl.extra.field.qxsl.*;

import static qxsl.extra.table.QxmlFormat.SENT;
import static org.assertj.core.api.Assertions.assertThat;

/**
 * {@link Sent}クラスのテスト用クラスです。
 * 
 * 
 * @author Journal of Hamradio Informatics
 * 
 * @since 2017/02/25
 *
 */
public final class SentTest extends junit.framework.TestCase {
	private final Code code = new Code("591420");
	private final RSTQ rstq = new RSTQ(1, 1, 1);
	private final Watt watt = new Watt("M");
	@Test
	public void testType() {
		assertThat(new Sent().name()).isEqualTo(SENT);
	}
	@Test
	public void testEquals() {
		final Sent sent1 = new Sent();
		final Sent sent2 = new Sent();
		assertThat(sent1).isEqualTo(sent2);
		assertThat(sent1.set(code).get(Code.class)).isEqualTo(code);
		assertThat(sent1.set(rstq).get(RSTQ.class)).isEqualTo(rstq);
		assertThat(sent1.set(watt).get(Watt.class)).isEqualTo(watt);
		assertThat(sent1).isNotEqualTo(sent2);
		assertThat(sent2.set(code).get(Code.class)).isEqualTo(code);
		assertThat(sent2.set(rstq).get(RSTQ.class)).isEqualTo(rstq);
		assertThat(sent2.set(watt).get(Watt.class)).isEqualTo(watt);
		assertThat(sent1).isEqualTo(sent2);
	}
}
