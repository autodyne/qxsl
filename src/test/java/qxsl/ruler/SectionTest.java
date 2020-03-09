/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*****************************************************************************/
package qxsl.ruler;

import java.util.ArrayList;

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

	/**
	 * ALLJA1コンテストの各部門の定義を返します。
	 *
	 *
	 * @return 部門の配列
	 * @throws Exception 通常は発生しない
	 */
	private static Section[] sections() throws Exception {
		final var list = new ArrayList<>();
		final var lisp = new RuleKit();
		final var test = lisp.contest(Contest.ALLJA1);
		for (final Section sect: test) list.add(sect);
		return list.toArray(new Section[list.size()]);
	}
}
