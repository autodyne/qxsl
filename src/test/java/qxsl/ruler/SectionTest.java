/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*****************************************************************************/
package qxsl.ruler;

import java.util.ArrayList;
import java.util.List;
import javax.script.ScriptException;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;
import static org.assertj.core.api.Assertions.assertThat;

/**
 * {@link Section}クラスのテスト用クラスです。
 * 
 * 
 * @author Journal of Hamradio Informatics
 * 
 * @since 2019/05/18
 *
 */
public final class SectionTest extends test.RandTest {
	public static List<Section> sections() throws ScriptException {
		final Contest ja1 = Contest.defined("allja1.lisp");
		final List<Section> sects = new ArrayList<>();
		for (Section section: ja1) sects.add(section);
		return sects;
	}
	@Test
	public void testContest() throws ScriptException {
		assertThat(sections()).isNotEmpty();
	}
	@ParameterizedTest
	@MethodSource("sections")
	public void testGetName(Section sect) throws ScriptException {
		assertThat(sect.getName()).isNotBlank();
	}
	@ParameterizedTest
	@MethodSource("sections")
	public void testToString(Section sect) throws ScriptException {
		assertThat(sect).hasToString(sect.getName());
	}
}
