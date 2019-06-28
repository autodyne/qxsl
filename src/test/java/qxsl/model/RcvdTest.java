/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Language: Java Standard Edition 8
 *****************************************************************************
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics http://pafelog.net
*****************************************************************************/
package qxsl.model;

import org.junit.Test;
import qxsl.extra.field.*;

import static qxsl.extra.table.QxmlFormat.RCVD;
import static org.assertj.core.api.Assertions.assertThat;

/**
 * {@link Rcvd}クラスのテスト用クラスです。
 * 
 * 
 * @author Journal of Hamradio Informatics
 * 
 * @since 2017/02/25
 *
 */
public final class RcvdTest extends junit.framework.TestCase {
	private final Code code = new Code("591009");
	private final RSTQ rstq = new RSTQ(5, 9, 9);
	private final Watt watt = new Watt("H");
	@Test
	public void testType() {
		assertThat(new Rcvd(new Item()).name()).isEqualTo(RCVD);
	}
	@Test
	public void testEquals() {
		final Rcvd rcvd1 = new Rcvd(new Item());
		final Rcvd rcvd2 = new Rcvd(new Item());
		assertThat(rcvd1).isEqualTo(rcvd2);
		assertThat(rcvd1.add(code).get(Qxsl.CODE)).isEqualTo(code);
		assertThat(rcvd1.add(rstq).get(Qxsl.RSTQ)).isEqualTo(rstq);
		assertThat(rcvd1.add(watt).get(Qxsl.WATT)).isEqualTo(watt);
		assertThat(rcvd1).isNotEqualTo(rcvd2);
		assertThat(rcvd2.add(code).get(Qxsl.CODE)).isEqualTo(code);
		assertThat(rcvd2.add(rstq).get(Qxsl.RSTQ)).isEqualTo(rstq);
		assertThat(rcvd2.add(watt).get(Qxsl.WATT)).isEqualTo(watt);
		assertThat(rcvd1).isEqualTo(rcvd2);
	}
}
