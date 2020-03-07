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
 * {@link Failure}クラスのテスト用クラスです。
 *
 *
 * @author 無線部開発班
 *
 * @since 2019/05/18
 *
 */
public final class FailureTest extends test.RandTest {
	@Test
	public void testGetMessage() {
		final Item item = new Item();
		final String text = alnum(100);
		assertThat(new Failure(item, text).text()).isSameAs(text);
	}
	@Test
	public void testGetItem() {
		final Item item = new Item();
		final String text = alnum(100);
		assertThat(new Failure(item, text).item()).isSameAs(item);
	}
}
