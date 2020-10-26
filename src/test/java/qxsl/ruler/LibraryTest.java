/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.ruler;

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
	private static final Library rule = RuleKit.loadAsLibrary(RULES);

	@Test
	public void testGet() {
		assertThat(rule.get("PHONE")).isInstanceOf(String.class);
		assertThat(rule.get("match")).isInstanceOf(Method.class);
	}

	@Test
	public void testInvoke() {
		assertThat(rule.invoke("boolean", "#t")).isInstanceOf(Boolean.class);
		assertThat(rule.invoke("integer", 8590)).isInstanceOf(Integer.class);
	}
}
