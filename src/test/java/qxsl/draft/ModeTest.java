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
 * {@link Mode}クラスのテスト用クラスです。
 *
 *
 * @author 無線部開発班
 *
 * @since 2017/02/24
 */
@ExtendWith(RandomStringParameterExtension.class)
public final class ModeTest extends Assertions {
	@Test
	public void testValue() {
		assertThat(new Mode("CW").value()).isEqualTo("CW");
		assertThat(new Mode("AM").value()).isEqualTo("AM");
	}

	@Test
	public void testToString(@RandomString String text) {
		assertThat(new Mode(text)).hasToString(text.toUpperCase());
	}
}
