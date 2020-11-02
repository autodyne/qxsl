/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.ruler;

import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

import qxsl.junit.RandomNumberParameterExtension;
import qxsl.junit.RandomNumberParameterExtension.RandomNumber;
import qxsl.junit.RandomStringParameterExtension;
import qxsl.junit.RandomStringParameterExtension.RandomString;
import qxsl.model.Item;

/**
 * {@link Success}クラスのテスト用クラスです。
 *
 *
 * @author 無線部開発班
 *
 * @since 2019/05/18
 */
@ExtendWith(RandomNumberParameterExtension.class)
@ExtendWith(RandomStringParameterExtension.class)
public final class SuccessTest extends Assertions {
	@Test
	public void testScore(@RandomNumber int score) {
		assertThat(new Success(new Item(), score).score()).isEqualTo(score);
	}

	@Test
	public void testItem(@RandomNumber int score) {
		final var item = new Item();
		assertThat(new Success(item, score).item()).isSameAs(item);
	}

	@Test
	public void testText(@RandomNumber int score) {
		assertThat(new Success(new Item(), score).text()).isEmpty();
	}
}
