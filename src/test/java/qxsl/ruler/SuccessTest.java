/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.ruler;

import qxsl.model.Item;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

import qxsl.junit.RandomNumberParameterExtension;
import qxsl.junit.RandomStringParameterExtension;
import qxsl.junit.RandomNumberParameterExtension.RandomNumber;
import qxsl.junit.RandomStringParameterExtension.RandomString;

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
public final class SuccessTest extends org.assertj.core.api.Assertions {
	@Test
	public void testScore(@RandomNumber int score) {
		assertThat(new Success(new Item(), score).score()).isEqualTo(score);
	}

	@Test
	public void testItem(@RandomNumber int score) {
		final Item item = new Item();
		assertThat(new Success(item, score).item()).isSameAs(item);
	}

	@Test
	public void testKey(@RandomString String a, @RandomString String b) {
		assertThat(new Success(null, 1, a, b).key(0)).isEqualTo(a);
		assertThat(new Success(null, 1, a, b).key(1)).isEqualTo(b);
	}

	@Test
	public void testCountKeys(@RandomNumber int score) {
		assertThat(new Success(null, score, 1, 23).countKeys()).isEqualTo(2);
	}
}
