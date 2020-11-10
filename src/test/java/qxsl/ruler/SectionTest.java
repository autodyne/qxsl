/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.ruler;

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
	@ParameterizedTest
	@MethodSource("sections")
	public void testCode(Section rule) {
		assertThat(rule.code()).isNotEmpty();
	}

	@ParameterizedTest
	@MethodSource("sections")
	public void testGet(Section rule) {
		assertThat(rule.get("MORSE")).isInstanceOf(String.class);
		assertThat(rule.get("match")).isInstanceOf(Method.class);
	}

	@ParameterizedTest
	@MethodSource("sections")
	public void testName(Section rule) {
		assertThat(rule.name()).isNotEmpty();
	}

	@ParameterizedTest
	@MethodSource("sections")
	public void testToString(Section rule) {
		assertThat(rule).hasToString(rule.name());
	}

	private static final Contest sections() {
		return RuleKit.load("allja1.lisp").contest();
	}
}
