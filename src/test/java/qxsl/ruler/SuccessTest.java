/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*****************************************************************************/
package qxsl.ruler;

import qxsl.model.Item;
import org.junit.jupiter.api.Test;
import static org.assertj.core.api.Assertions.assertThat;

/**
 * {@link Success}クラスのテスト用クラスです。
 *
 *
 * @author 無線部開発班
 *
 * @since 2019/05/18
 *
 */
public final class SuccessTest extends test.RandTest {
	@Test
	public void testScore() {
		assertThat(new Success(new Item(), 1).score()).isEqualTo(1);
		assertThat(new Success(new Item(), 2).score()).isEqualTo(2);
		assertThat(new Success(new Item(), 3).score()).isEqualTo(3);
	}
	@Test
	public void testItem() {
		final Item item = new Item();
		assertThat(new Success(item, 1).item()).isSameAs(item);
	}
	@Test
	public void testKey() {
		assertThat(new Success(null, 1, "A", "B").key(0)).isEqualTo("A");
		assertThat(new Success(null, 1, "A", "B").key(1)).isEqualTo("B");
	}
	@Test
	public void testCountKeys() {
		assertThat(new Success(null, 2, 1, 23).countKeys()).isEqualTo(2);
	}
}
