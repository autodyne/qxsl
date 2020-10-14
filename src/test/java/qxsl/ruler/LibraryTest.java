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
 * {@link Library}クラスのテスト用クラスです。
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/10/14
 */
public final class LibraryTest extends Assertions {
	private static final Class<?> CLS = Library.class;
	private static final String RULES = "jautil.lisp";
	private static final Library RULE = loadLibrary();

	/**
	 * テスト対象のライブラリ定義を読み出します。
	 *
	 *
	 * @return 規約
	 */
	private static final Library loadLibrary() {
		final var kit = RuleKit.load("elva");
		final var res = CLS.getResourceAsStream(RULES);
		return kit.library(new InputStreamReader(res));
	}

	@Test
	public void testGet() {
		assertThat(RULE.get("PHONE")).isInstanceOf(String.class);
		assertThat(RULE.get("match")).isInstanceOf(Method.class);
	}

	@Test
	public void testInvoke() {
		assertThat(RULE.invoke("boolean", "#t")).isInstanceOf(Boolean.class);
		assertThat(RULE.invoke("integer", 8590)).isInstanceOf(Integer.class);
	}
}
