/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License: GNU Lesser General Public License v3.0 (see LICENSE)
 * Author: Journal of Hamradio Informatics (https://pafelog.net)
*******************************************************************************/
package qxsl.ruler;

import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * {@link RuleKit}クラスの挙動を検査します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2017/02/26
 */
public final class RuleKitTest extends Assertions {
	@Test
	public void testForFile() {
		assertThat(RuleKit.forFile(".lisp").name()).isEqualTo("elva");
	}

	@Test
	public void testForName() {
		assertThat(RuleKit.forName("elva").name()).isEqualTo("elva");
	}
}
