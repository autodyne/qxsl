/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.ruler;

import org.junit.jupiter.api.Test;

/**
 * {@link RuleKit}クラスのテスト用クラスです。
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/06/08
 */
public final class RuleKitTest extends org.assertj.core.api.Assertions {
	@Test
	public void testName() {
		assertThat(RuleKit.load("elva").name()).isEqualTo("elva");
	}

	@Test
	public void testLoad() {
		assertThat(RuleKit.load("elva")).isNotNull();
	}
}
