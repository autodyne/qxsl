/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.ruler;

import java.io.InputStreamReader;
import java.lang.reflect.Method;

import org.assertj.core.api.Assertions;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;

/**
 * {@link Section}クラスのテスト用クラスです。
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/10/14
 */
public final class SectionTest extends Assertions {
	private static final Class<?> CLS = Section.class;
	private static final String RULES = "allja1.lisp";

	/**
	 * テスト対象のコンテスト部門を読み出します。
	 *
	 *
	 * @return 規約
	 */
	private static final Iterable<Section> loadSections() {
		final var kit = RuleKit.load("elva");
		final var res = CLS.getResourceAsStream(RULES);
		return kit.contest(new InputStreamReader(res));
	}

	@ParameterizedTest
	@MethodSource("loadSections")
	public void testGet(Section rule) {
		assertThat(rule.get("MORSE")).isInstanceOf(String.class);
		assertThat(rule.get("match")).isInstanceOf(Method.class);
	}

	@ParameterizedTest
	@MethodSource("loadSections")
	public void testInvoke(Section rule) {
		assertThat(rule.invoke("boolean", "#t")).isInstanceOf(Boolean.class);
		assertThat(rule.invoke("integer", 7650)).isInstanceOf(Integer.class);
	}
}
