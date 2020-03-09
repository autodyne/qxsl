/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*****************************************************************************/
package qxsl.ruler;

import org.junit.jupiter.api.Test;
import static org.assertj.core.api.Assertions.assertThat;

/**
 * {@link Contest}クラスのテスト用クラスです。
 *
 *
 * @author 無線部開発班
 *
 * @since 2017/02/26
 *
 */
public final class ContestTest extends test.RandTest {
	private final Contest ja1;
	private ContestTest() throws Exception {
		ja1 = new RuleKit().contest(Contest.ALLJA1);
	}

	@Test
	public void testForName() {
		assertThat(ja1).isNotNull();
	}

	@Test
	public void testGetName() {
		assertThat(ja1.getName()).isNotBlank();
	}

	@Test
	public void testIterator() {
		assertThat(ja1.iterator()).hasNext();
	}

	@Test
	public void testToString() {
		assertThat(ja1).hasToString(ja1.getName());
	}
}
