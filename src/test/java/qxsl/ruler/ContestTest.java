/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.ruler;

import java.io.InputStreamReader;
import java.lang.reflect.Method;

import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * {@link Contest}クラスのテスト用クラスです。
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/10/14
 */
public final class ContestTest extends Assertions {
	private static final Class<?> CLS = Contest.class;
	private static final String RULES = "allja1.lisp";
	private static final Contest RULE = loadContest();

	/**
	 * テスト対象のコンテスト規約を読み出します。
	 *
	 *
	 * @return 規約
	 */
	private static final Contest loadContest() {
		final var kit = RuleKit.load("elva");
		final var res = CLS.getResourceAsStream(RULES);
		return kit.contest(new InputStreamReader(res));
	}

	@Test
	public void testGet() {
		assertThat(RULE.get("DIGIT")).isInstanceOf(String.class);
		assertThat(RULE.get("split")).isInstanceOf(Method.class);
	}

	@Test
	public void testInvoke() {
		assertThat(RULE.invoke("boolean", "#f")).isInstanceOf(Boolean.class);
		assertThat(RULE.invoke("integer", 7290)).isInstanceOf(Integer.class);
	}
}
