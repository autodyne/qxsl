/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Language: Java Standard Edition 8
 *****************************************************************************
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics http://pafelog.net
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
public final class ContestTest extends junit.framework.TestCase {
	private Contest ja1 = null;
	private Contest ja1() throws ScriptException {
		if(ja1 == null) ja1 = Contest.defined("allja1.lisp");
		return ja1;
	}
	@Test
	public void testForName() throws ScriptException {
		assertThat(ja1()).isNotNull();
	}
	@Test
	public void testGetName() throws ScriptException {
		assertThat(ja1().getName()).isNotBlank();
	}
	@Test
	public void testIterator() throws ScriptException {
		assertThat(ja1().iterator()).hasNext();
	}
	@Test
	public void testToString() throws ScriptException {
		assertThat(ja1()).hasToString(ja1().getName());
	}
}
