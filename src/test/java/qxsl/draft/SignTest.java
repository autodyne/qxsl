/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License: GNU Lesser General Public License v3.0 (see LICENSE)
 * Author: Journal of Hamradio Informatics (https://pafelog.net)
*******************************************************************************/
package qxsl.draft;

import java.time.ZonedDateTime;

import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * {@link Sign}クラスの挙動を検査します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2022/08/08
 */
public final class SignTest extends Assertions {
	@Test
	public void testToString() {
		final var time = ZonedDateTime.now();
		assertThat(new Sign(time)).hasToString(time.toString());
	}

	@Test
	public void testValue() {
		final var time = ZonedDateTime.now();
		assertThat(new Sign(time).value()).isEqualTo(time);
	}
}
