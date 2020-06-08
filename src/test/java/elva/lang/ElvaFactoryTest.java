/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package elva.lang;

import org.junit.jupiter.api.Test;

/**
 * {@link ElvaFactory}クラスのテスト用クラスです。
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/06/09
 */
public final class ElvaFactoryTest extends org.assertj.core.api.Assertions {
	private final ElvaFactory elva = new ElvaFactory();

	@Test
	public void testGetEngineName() {
		assertThat(elva.getEngineName()).isEqualTo("elva");
	}

	@Test
	public void testGetEngineVersion() {
		assertThat(elva.getEngineVersion()).isNotEmpty();
	}

	@Test
	public void testGetLanguageName() {
		assertThat(elva.getLanguageName()).isEqualTo("elva");
	}

	@Test
	public void testGetLanguageVersion() {
		assertThat(elva.getLanguageVersion()).isNotEmpty();
	}

	@Test
	public void testGetNames() {
		assertThat(elva.getNames()).contains("elva");
	}

	@Test
	public void testGetMimeTypes() {
		assertThat(elva.getMimeTypes()).asList();
	}

	@Test
	public void testGetExtensions() {
		assertThat(elva.getExtensions()).contains("lisp");
	}

	@Test
	public void testGetScriptEngine() {
		assertThat(elva.getScriptEngine()).isInstanceOf(ElvaRuntime.class);
	}

	@Test
	public void testGetProgram() {
		assertThat(elva.getProgram("do some")).isEqualTo("(block do some)");
		assertThat(elva.getProgram("fantasy")).isEqualTo("(block fantasy)");
	}

	@Test
	public void testGetOutputStatement() {
		assertThat(elva.getOutputStatement("bar")).isEqualTo("(print bar)");
		assertThat(elva.getOutputStatement("foo")).isEqualTo("(print foo)");
	}

	@Test
	public void testGetMethodCallSyntax() throws Exception {
		final var exp = elva.getMethodCallSyntax("1", "compareTo", "2");
		assertThat(elva.getScriptEngine().eval(exp)).isEqualTo(-1);
	}
}
