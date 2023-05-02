/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Released under the GNU Lesser General Public License (LGPL) v3 (see LICENSE)
 * Univ. Tokyo Amateur Radio Club Development Task Force (https://nextzlog.dev)
*******************************************************************************/
package qxsl.utils;

import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

import qxsl.junit.RandomStringParameterExtension;
import qxsl.junit.RandomStringParameterExtension.RandomString;

/**
 * {@link MorseCode}クラスの挙動を検査します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2023/05/02
 */
@ExtendWith(RandomStringParameterExtension.class)
public final class MorseCodeTest extends Assertions {
	private final String encode(String text) {
		return new MorseCode().encode(text);
	}

	private final String decode(String code) {
		return new MorseCode().decode(code);
	}

	@Test
	public void test(@RandomString String text) {
		assertThat(decode(encode(text))).isEqualToIgnoringCase(text);
	}
}
