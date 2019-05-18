/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Language: Java Standard Edition 8
 *****************************************************************************
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics http://pafelog.net
*****************************************************************************/
package qxsl.ruler;

import qxsl.model.Item;
import org.junit.Test;
import static org.assertj.core.api.Assertions.assertThat;

/**
 * {@link Failure}クラスのテスト用クラスです。
 * 
 * 
 * @author Journal of Hamradio Informatics
 * 
 * @since 2019/05/18
 *
 */
public final class FailureTest extends junit.framework.TestCase {
	@Test
	public void testGetMessage() {
		final Item item = new Item();
		String text = util.RandText.alnum(100);
		assertThat(new Failure(text, item).text()).isSameAs(text);
	}
	@Test
	public void testGetItem() {
		final Item item = new Item();
		String text = util.RandText.alnum(100);
		assertThat(new Failure(text, item).item()).isSameAs(item);
	}
}
