/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.ruler;

import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * {@link RuleKit}クラスのテスト用クラスです。
 *
 *
 * @author 無線部開発班
 *
 * @since 2017/02/26
 */
public final class RuleKitTest extends Assertions {
	@Test
	public void testForFile() {
		assertThat(RuleKit.forFile("q.lisp").name()).isEqualTo("elva");
		assertThat(RuleKit.forFile("xsl.rb").name()).isEqualTo("ruby");
	}

	@Test
	public void testForName() {
		assertThat(RuleKit.forName("elva").name()).isEqualTo("elva");
		assertThat(RuleKit.forName("ruby").name()).isEqualTo("ruby");
	}
}
