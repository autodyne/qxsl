/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*****************************************************************************/
package qxsl.ruler;

import javax.script.ScriptException;
import org.junit.jupiter.api.Test;
import static org.assertj.core.api.Assertions.assertThat;

/**
 * {@link Handler}クラスのテスト用クラスです。
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/02/27
 *
 */
public final class HandlerTest extends test.RandTest {
	private final Handler format;
	private HandlerTest() throws ScriptException {
		format = new RuleKit().handler(Handler.FORMAT);
	}

	@Test
	public void testGetName() {
		assertThat(format.getName()).isNotBlank();
	}

	@Test
	public void testToString() {
		assertThat(format).hasToString(format.getName());
	}
}
