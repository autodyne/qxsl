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
 * {@link Code}クラスのテスト用クラスです。
 *
 *
 * @author 無線部開発班
 *
 * @since 2017/02/24
 */
@ExtendWith(RandomStringParameterExtension.class)
public final class CodeTest extends Assertions {
	@Test
	public void testToString(@RandomString String text) {
		assertThat(new Code(text)).hasToString(text);
	}

	@Test
	public void testValue() {
		assertThat(new Code("100110H").value()).isEqualTo("100110H");
		assertThat(new Code("400105M").value()).isEqualTo("400105M");
	}
}
