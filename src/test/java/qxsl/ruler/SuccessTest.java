/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Language: Java Standard Edition 8
 *****************************************************************************
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics http://pafelog.net
*****************************************************************************/
package qxsl.ruler;

import qxsl.model.Item;
import org.junit.jupiter.api.Test;
import static org.assertj.core.api.Assertions.assertThat;

/**
 * {@link Success}クラスのテスト用クラスです。
 * 
 * 
 * @author Journal of Hamradio Informatics
 * 
 * @since 2019/05/18
 *
 */
public final class SuccessTest extends test.RandTest {
	@Test
	public void testScore() {
		assertThat(new Success(1, new Item()).score()).isEqualTo(1);
		assertThat(new Success(2, new Item()).score()).isEqualTo(2);
		assertThat(new Success(3, new Item()).score()).isEqualTo(3);
	}
	@Test
	public void testItem() {
		final Item item = new Item();
		assertThat(new Success(1, item).item()).isSameAs(item);
	}
	@Test
	public void testKey() {
		assertThat(new Success(1, null, "A", "B").key(0)).isEqualTo("A");
		assertThat(new Success(1, null, "A", "B").key(1)).isEqualTo("B");
	}
	@Test
	public void testCountKeys() {
		assertThat(new Success(2, null, 1, 23).countKeys()).isEqualTo(2);
	}
}
