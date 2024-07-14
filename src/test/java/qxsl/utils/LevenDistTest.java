/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Released under the GNU Lesser General Public License (LGPL) v3 (see LICENSE)
 * Univ. Tokyo Amateur Radio Club Development Task Force (https://nextzlog.dev)
*******************************************************************************/
package qxsl.utils;

import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * {@link LevenDist}クラスの挙動を検査します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2024/07/14
 */
public final class LevenDistTest extends Assertions {
	private final int apply(String str1, String str2) {
		return new LevenDist().applyAsInt(str1, str2);
	}

	@Test
	public void testApplyAsInt() {
		assertThat(apply("364", "364")).isEqualTo(0);
		assertThat(apply("114", "514")).isEqualTo(1);
		assertThat(apply("889", "464")).isEqualTo(3);
	}
}
