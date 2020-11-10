/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.draft;

import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

import qxsl.junit.RandomStringParameterExtension;
import qxsl.junit.RandomStringParameterExtension.RandomString;

/**
 * {@link Mul2}クラスのテスト用クラスです。
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/10/28
 */
@ExtendWith(RandomStringParameterExtension.class)
public final class Mul2Test extends Assertions {
	@Test
	public void testToString(@RandomString String text) {
		assertThat(new Mul2(text)).hasToString(text);
	}

	@Test
	public void testValue() {
		assertThat(new Mul2("100110").value()).isEqualTo("100110");
		assertThat(new Mul2("400105").value()).isEqualTo("400105");
	}
}
