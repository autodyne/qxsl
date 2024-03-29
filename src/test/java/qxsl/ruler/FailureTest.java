/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Released under the GNU Lesser General Public License (LGPL) v3 (see LICENSE)
 * Univ. Tokyo Amateur Radio Club Development Task Force (https://nextzlog.dev)
*******************************************************************************/
package qxsl.ruler;

import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

import qxsl.junit.RandomStringParameterExtension;
import qxsl.junit.RandomStringParameterExtension.RandomString;
import qxsl.model.Item;

/**
 * {@link Failure}クラスの挙動を検査します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2019/05/18
 */
@ExtendWith(RandomStringParameterExtension.class)
public final class FailureTest extends Assertions {
	@Test
	public void testItem(@RandomString String text) {
		final var item = new Item();
		assertThat(new Failure(item, text).item()).isSameAs(item);
	}

	@Test
	public void testScore(@RandomString String text) {
		assertThat(new Failure(new Item(), text).score()).isEqualTo(0);
	}

	@Test
	public void testText(@RandomString String text) {
		assertThat(new Failure(new Item(), text).text()).isEqualTo(text);
	}
}
