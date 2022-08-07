/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License: GNU Lesser General Public License v3.0 (see LICENSE)
 * Author: Journal of Hamradio Informatics (https://pafelog.net)
*******************************************************************************/
package qxsl.draft;

import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

import qxsl.junit.RandomStringParameterExtension;
import qxsl.junit.RandomStringParameterExtension.RandomString;

/**
 * {@link Text}クラスの挙動を検査します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2022/08/06
 */
@ExtendWith(RandomStringParameterExtension.class)
public final class TextTest extends Assertions {
	@Test
	public void testToString(@RandomString String text) {
		assertThat(new Text(text)).hasToString(text);
	}

	@Test
	public void testValue() {
		assertThat(new Text("RABBIT").value()).isEqualTo("RABBIT");
		assertThat(new Text("GIBIER").value()).isEqualTo("GIBIER");
	}
}