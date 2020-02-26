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
 * {@link Contest}クラスのテスト用クラスです。
 * 
 * 
 * @author Journal of Hamradio Informatics
 * 
 * @since 2017/02/26
 *
 */
public final class ContestTest extends test.RandTest {
	private final Contest ja1;
	private ContestTest() throws ScriptException {
		ja1 = new RuleKit().contest("allja1.lisp");
	}
	@Test
	public void testForName() throws ScriptException {
		assertThat(ja1).isNotNull();
	}
	@Test
	public void testGetName() throws ScriptException {
		assertThat(ja1.getName()).isNotBlank();
	}
	@Test
	public void testIterator() throws ScriptException {
		assertThat(ja1.iterator()).hasNext();
	}
	@Test
	public void testToString() throws ScriptException {
		assertThat(ja1).hasToString(ja1.getName());
	}
}
