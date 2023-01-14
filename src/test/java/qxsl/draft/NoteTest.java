/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Released under the GNU Lesser General Public License (LGPL) v3 (see LICENSE)
 * Univ. Tokyo Amateur Radio Club Development Task Force (https://nextzlog.dev)
*******************************************************************************/
package qxsl.draft;

import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

import qxsl.junit.RandomStringParameterExtension;
import qxsl.junit.RandomStringParameterExtension.RandomString;

/**
 * {@link Note}クラスの挙動を検査します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2017/02/24
 */
@ExtendWith(RandomStringParameterExtension.class)
public final class NoteTest extends Assertions {
	@Test
	public void testToString(@RandomString String text) {
		assertThat(new Note(text)).hasToString(text);
	}

	@Test
	public void testValue() {
		assertThat(new Note("RABBIT").value()).isEqualTo("RABBIT");
		assertThat(new Note("GIBIER").value()).isEqualTo("GIBIER");
	}
}
