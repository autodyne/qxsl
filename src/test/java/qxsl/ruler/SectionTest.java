/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*****************************************************************************/
package qxsl.ruler;

import java.util.Iterator;
import javax.script.ScriptException;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;
import static org.assertj.core.api.Assertions.assertThat;

/**
 * {@link Section}クラスのテスト用クラスです。
 *
 *
 * @author 無線部開発班
 *
 * @since 2019/05/18
 *
 */
public final class SectionTest extends test.RandTest {
	static Iterator<Section> sections() throws ScriptException {
		return new RuleKit().contest("allja1.lisp").iterator();
	}
	@ParameterizedTest
	@MethodSource("sections")
	public void testGetName(Section sect) {
		assertThat(sect.getName()).isNotBlank();
	}
	@ParameterizedTest
	@MethodSource("sections")
	public void testGetCode(Section sect) {
		assertThat(sect.getCode()).isNotBlank();
	}
	@ParameterizedTest
	@MethodSource("sections")
	public void testToString(Section sect) {
		assertThat(sect).hasToString(sect.getName());
	}
}
